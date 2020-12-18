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
mod.nam <- "BigCheese"
chain <- 1 # If running parallel chains
save.out <- str_c("mod_mcmcR_", mod.nam, "_", spp)

# Compile detection data #
source(str_c(scripts.loc, "Data_processing_", mod.nam, ".R"))

#image(1:J,1:n,t(Y),col=c(5,3,2),main="Y",xlab="time",ylab="individual")

####
####  Standardize covariates (and orthogonalize another set, just in case you want to use those) 
####

p.cov=dim(X)[3]
# Already standardized above, so don't need this.
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
s.reg.vec=c(s.reg.vec[-which(s.reg.vec > 0.003 & s.reg.vec < 0.3)], seq(.003,0.3,,n.reg))# Add more at higher regs
n.reg <- length(s.reg.vec)

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

score.vec=rep(0,n.reg)
for(j in 1:n.reg){
  score.vec[j]=sum(tmp.mat[run.grd[,2]==s.reg.vec[j],1])
}
s.reg.opt=s.reg.vec[score.vec==min(score.vec)]

plot(s.reg.vec,score.vec,ylab="score",xlab="s.reg") #,type="l"
abline(v=s.reg.opt,col=rgb(0,0,0,.25))

reg_scores <- cbind(s.reg.vec, score.vec)
dimnames(reg_scores)[[2]] <- c("Regularization", "Score")
write.csv(reg_scores, str_c("Reg_scores_", spp, ".csv"), row.names = F)

# ## Run additional regularization levels for fine-tuning ##
# n.reg=20
# s.reg.vec=seq(.005,.15,,n.reg)
# 
# run.grd=expand.grid(fold.vec,s.reg.vec)
# n.run=dim(run.grd)[1]
# tmp.mat=matrix(0,n.run,3+p.cov)
# 
# tic()
# for(j in 1:n.run){
#   cat(j," ")
#   idx.out=idx.list[[run.grd[j,1]]]
#   idx.in=(1:n)[-idx.out]
#   Y.in=Y[idx.in,]
#   Y.out=Y[idx.out,]
#   X.in=X.std[idx.in,,]
#   X.out=X.std[idx.out,,]
#   tmp.out=CJSRL.hmm.PML.pred(Y.in,X.in,Y.out,X.out,s.reg=run.grd[j,2],p=.8728,psi=.0253,fix.ppsi=FALSE)
#   tmp.mat[j,]=c(tmp.out$score,tmp.out$p.hat,tmp.out$psi.hat,tmp.out$beta.hat) 
# };cat("\n")
# toc.2=toc()
# 
# score.vec=rep(0,n.reg)
# for(j in 1:n.reg){
#   score.vec[j]=sum(tmp.mat[run.grd[,2]==s.reg.vec[j],1])
# }
# s.reg.opt=s.reg.vec[score.vec==min(score.vec)]
# 
# reg_scores <- cbind(s.reg.vec, score.vec)
# dimnames(reg_scores)[[2]] <- c("Regularization", "Score")
# reg_scores <- rbind(reg_scores,
#                     read.csv(str_c("Reg_scores_", spp, ".csv"), stringsAsFactors = F))
#   
# 
# plot(reg_scores[, "Regularization"], reg_scores[, "Score"], ylab="score", xlab="s.reg")
# abline(v=s.reg.opt,col=rgb(0,0,0,.25))
# write.csv(reg_scores, str_c("Reg_scores_", spp, ".csv"), row.names = F)

####
####  Fit Fully Bayesian Model using MCMC (3 hrs)
####

#Rcpp::sourceCpp(str_c(scripts.loc, 'CJSRL4fwdalg.cpp'))
Rcpp::sourceCpp(str_c(scripts.loc, 'CJSRL5fwdalg.cpp'))
source(str_c(scripts.loc, "CJSRL.hmm.adapt.mcmc.R"))

n.mcmc=200000
rm(.Random.seed, envir=.GlobalEnv)
#s.reg.opt <- 0.174947368421053 # For BAIS, Big Cheese & ShrubSpp
#s.reg.opt <- 0.0811578947368421 # For GRSP, Big Cheese & ShrubSpp

tic()
out=CJSRL.hmm.adapt.mcmc(Y,X,n.mcmc,beta.tune=.01,s.reg=s.reg.opt,p=.9,p.tune=0.005,psi=.1,
                         psi.tune=0.005,recov.homog=TRUE,adapt.iter=1000,plot.trace=F)
toc.3=toc()

library(R.utils)
saveObject(out, str_c("mod_mcmcR_", mod.nam, "_", chain, "_", spp))

#####
#####  Summarize and Plot Posterior
#####

# If loading model outputs from previous runs
library(R.utils)
out <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", chain, "_", spp))

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

## Assess convergence and combine chains ##
library(R.utils)
out1 <- loadObject(str_c("mod_mcmcR_", mod.nam, "_1_", spp))
out2 <- loadObject(str_c("mod_mcmcR_", mod.nam, "_2_", spp))

out1.arr <- rbind(out1$beta.save, out1$p.save, out1$psi.save)
out1.arr <- out1.arr[,-(1:out1$n.burn)]
out2.arr <- rbind(out2$beta.save, out2$p.save, out2$psi.save)
out2.arr <- out2.arr[,-(1:out2$n.burn)]

# Rhat #
n.mcmc <- dim(out1.arr)[2]
n.chains <- 2
w <- (apply(out1.arr,1,var) + apply(out1.arr,1,var)) / 2
mn.beta.mn <- (apply(out1.arr, 1, mean) + apply(out2.arr, 1, mean)) / 2
b <- (n.mcmc/(n.chains-1))*(((apply(out1.arr, 1, mean)-mn.beta.mn) + (apply(out2.arr, 1, mean)-mn.beta.mn))^2)
Rhat <- sqrt(((n.mcmc-1)/n.mcmc*w+b/n.mcmc)/w)

sims.array <- abind::abind(out1.arr, out2.arr, along = 3)
dimnames(sims.array)[[1]] <- c(str_c("B.", X.nams), "p", "psi")
sims.concat <- cbind(out1.arr, out2.arr)
dimnames(sims.concat)[[1]] <- c(str_c("B.", X.nams), "p", "psi")

out <- list(mod.raw = list(chain1 = out1, chain2 = out2), sims.array = sims.array, sims.concat = sims.concat, Rhat = Rhat)
saveObject(out, save.out)
