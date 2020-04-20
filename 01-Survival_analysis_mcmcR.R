library(stringr)
library(dplyr)
#library(QSLpersonal)
library(mvnfast)
library(Rcpp)
library(abind)
library(RcppArmadillo)
library(tictoc)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
Rcpp::sourceCpp('grass-surv-scripts/CJSRL4fwdalg.cpp')
Rcpp::sourceCpp('grass-surv-scripts/CJSRL5fwdalg.cpp')
source("grass-surv-scripts/CJSRL.hmm.adapt.mcmc.R")
n.mcmc=200000
save.out <- str_c("mod_mcmcR_CJSRLtest_", spp)
#______________________________#

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
DOSdepl <- data.spp$Covs$firstDay
time_since_depl <- DOS - DOSdepl
after_depl <- (time_since_depl > 0)*1
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])
DOS2 <- DOS^2

drone.z <- data.spp$Covs %>% ungroup() %>%
  select(Mesquite_5m:Distance_to_Fence) %>%
  select(contains("_100m"), Distance_to_Fence) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) %>%
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

n=dim(Y)[1]
J=dim(Y)[2]
image(1:J,1:n,t(Y),col=c(5,3,2),main="Y",xlab="time",ylab="individual")

tic()
out=CJSRL.hmm.adapt.mcmc(Y,X,n.mcmc,beta.tune=.01,s.reg=.1,p=.9,p.tune=0.01,psi=.1,psi.tune=0.01,recov.homog=TRUE)  
toc()
out$pD
out$DIC

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

n.rows=floor(sqrt(out$p.cov))
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

library(R.utils)
saveObject(out, save.out)


