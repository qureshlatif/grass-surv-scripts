library(stringr)
library(dplyr)
library(mvnfast)
library(Rcpp)
library(abind)
library(RcppArmadillo)
library(tictoc)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"
spp <- "BAIS" # BAIS or GRSP
save.out <- str_c("mod_mcmcR_CJSRL_", spp)

# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- data.spp$Covs$firstDay
last <- data.spp$Covs$lastDay
last.alive <- data.spp$Covs$lastAlive
nBird <- nrow(ymat)
nDOS <- ncol(ymat)
SeasonInd <- data.spp$Covs$SeasonInd
nSeason <- max(SeasonInd)
SiteInd <- data.spp$Covs$SiteInd
nSite <- max(SiteInd)

# Covariates #
DOS <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
# DOSdepl <- data.spp$Covs$firstDay
# time_since_depl <- DOS - DOSdepl
# after_depl <- (time_since_depl > 0)*1
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])
DOS2 <- DOS^2

temp.min <- temp.prec7 <- array(NA, dim = dim(DOS))
for(i in 1:nBird) {
  ind <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(DOS)
  temp.min[i, ind] <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(temp.min)
  temp.prec7[i, ind] <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(temp.prec7)
}
temp.min <- (temp.min - mean(temp.min[which(!is.na(ymat))])) / sd(temp.min[which(!is.na(ymat))])
temp.min[which(is.na(temp.min))] <- 0
temp.prec7 <- (temp.prec7 - mean(temp.prec7[which(!is.na(ymat))])) / sd(temp.prec7[which(!is.na(ymat))])
temp.prec7[which(is.na(temp.prec7))] <- 0

Veg.z <- data.spp$Covs %>% ungroup() %>%
  select(Mesquite_5m:Distance_to_Fence) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
#  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) %>%
  data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

drone2.z <- drone.z ^ 2

droneCV.z <- data.spp$Covs %>%
  select(Mesquite_5m_CV:Mean_Shrub_Height_500m_CV) %>%
  select(contains("_100m")) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) %>%
  data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

peso.x <- data.spp$Covs$peso
peso.z <- peso.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)) %>%
  (function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>%
  array(., c(length(.), nDOS))

X <- abind::abind(array(1, dim = dim(DOS)), DOS, DOS2, drone.z, drone2.z, droneCV.z, peso.z, along = 3)
Y <- ymat

X.nams <- c("Intercept",
            "DOS", "DOS2",
            data.spp$Covs %>% ungroup() %>%
              select(Mesquite_5m:Distance_to_Fence) %>%
              select(contains("_100m"), Distance_to_Fence) %>% names,
            str_c(data.spp$Covs %>% ungroup() %>%
                    select(Mesquite_5m:Distance_to_Fence) %>%
                    select(contains("_100m"), Distance_to_Fence) %>% names, "_sqrd"),
            droneCV.z <- data.spp$Covs %>%
              select(Mesquite_5m_CV:Mean_Shrub_Height_500m_CV) %>%
              select(contains("_100m")) %>% names,
            "peso")

n=dim(Y)[1]
J=dim(Y)[2]
#image(1:J,1:n,t(Y),col=c(5,3,2),main="Y",xlab="time",ylab="individual")

####
####  Standardize covariates (and orthogonalize another set, just in case you want to use those) 
####

p.cov=dim(X)[3]
# Already standardized above, so don't think I need this.
# X.stacked=apply(X,3L,c)
# X.std.stacked=X.stacked
# X.std.stacked[,-1]=scale(X.stacked[,-1])
# X.std=array(X.std.stacked,dim(X))
X.std <- X

####
####  Parallel for PML and CV scoring 
####

Rcpp::sourceCpp(str_c(scripts.loc, 'CJSRL5fwdalg.cpp'))
source(str_c(scripts.loc, "CJSRL.hmm.PML.pred.R"))

set.seed(123)
n.folds=10
fold.vec=1:n.folds
idx.rnd=sample(1:n,n,replace=FALSE)
idx.cuts=round(seq(1,n,,n.folds+1))

idx.list=list("vector",n.folds)
for(k in 1:n.folds){
  idx.list[[k]]=sort(idx.rnd[idx.cuts[k]:idx.cuts[k+1]])
}

n.reg=20
s.reg.vec=seq(.001,1,,n.reg)

run.grd=expand.grid(fold.vec,s.reg.vec)
n.run=dim(run.grd)[1]
tmp.mat=matrix(0,n.run,3+p.cov)

tic()
for(j in 1:n.run){
  cat(j," ")
  idx.out=idx.list[[run.grd[j,1]]]
  idx.in=(1:n)[-idx.out]
  Y.in=Y[idx.in,]
  Y.out=Y[idx.out,]
  X.in=X.std[idx.in,,]
  X.out=X.std[idx.out,,]
  tmp.out=CJSRL.hmm.PML.pred(Y.in,X.in,Y.out,X.out,s.reg=run.grd[j,2],p=.8728,psi=.0253,fix.ppsi=FALSE)
  tmp.mat[j,]=c(tmp.out$score,tmp.out$p.hat,tmp.out$psi.hat,tmp.out$beta.hat) 
};cat("\n")
toc.2=toc()
# Takes 7.429914 hours for 42 covariates, n.fold = 10, n.reg = 20
# Optimal regularization ended up being 0.05357895, which is a bit different from the 0.1 reported by Mevin.
#    Might need to run more reg levels between 0.05 and 0.1 to resolve the optimum better.

score.vec=rep(0,n.reg)
for(j in 1:n.reg){
  score.vec[j]=sum(tmp.mat[run.grd[,2]==s.reg.vec[j],1])
}
s.reg.opt=s.reg.vec[score.vec==min(score.vec)]

plot(s.reg.vec,score.vec,type="l",ylab="score",xlab="s.reg")
abline(v=s.reg.opt,col=rgb(0,0,0,.25))

####
####  Fit Fully Bayesian Model using MCMC (3 hrs)
####

Rcpp::sourceCpp(str_c(scripts.loc, 'CJSRL4fwdalg.cpp'))
Rcpp::sourceCpp(str_c(scripts.loc, 'CJSRL5fwdalg.cpp'))
source(str_c(scripts.loc, "CJSRL.hmm.adapt.mcmc.R"))

n.mcmc=200000

tic()
out=CJSRL.hmm.adapt.mcmc(Y,X,n.mcmc,beta.tune=.01,s.reg=s.reg.opt,p=.9,p.tune=0.005,psi=.1,psi.tune=0.005,recov.homog=TRUE,adapt.iter=1000,plot.trace=TRUE)
toc.3=toc()

#####
#####  Summarize and Plot Posterior
#####

layout(matrix(1:3,3,1))
matplot(t(out$beta.save),type="l",lty=1)
abline(h=0,col=8,lty=2)
abline(v=out$n.burn,col=8,lty=2)
plot(out$p.save,type="l")
plot(out$psi.save,type="l")

mean(out$p.save[out$n.burn:out$n.mcmc])
quantile(out$p.save[out$n.burn:out$n.mcmc],c(0.025,0.975))
mean(out$psi.save[out$n.burn:out$n.mcmc])
quantile(out$psi.save[out$n.burn:out$n.mcmc],c(0.025,0.975))

n.rows=ceiling(sqrt(out$p.cov))
n.cols=ceiling(sqrt(out$p.cov))
n.panel=n.rows*n.cols
layout(matrix(1:n.panel,n.rows,n.cols))
for(j in 1:out$p.cov){
  hist(out$beta.save[j,out$n.burn:n.mcmc],col=rgb(0,0,0,.25),prob=TRUE,main="",xlab=bquote(beta[.(j-1)]))
  abline(v=0,lty=2,col=8)
}

layout(matrix(1:2,1,2))
hist(out$p.save[out$n.burn:n.mcmc],col=rgb(0,0,0,.25),breaks=40,prob=TRUE,main="",xlab=bquote(p),xlim=c(.5,1))
hist(out$psi.save[out$n.burn:n.mcmc],col=rgb(0,0,0,.25),breaks=40,prob=TRUE,main="",xlab=bquote(psi),xlim=c(0,.5))

