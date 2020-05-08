CJSRL.hmm.PML.pred <- function(Y,X,Y.pred,X.pred,s.reg=1.5,p=0.95,psi=0.9,fix.ppsi=FALSE){

#
#  Fits CJS model with dead recoveries using HMM perspective and forward algorithm using Penalized ML 
#  Langrock and King (2013) with extension to handle long dead recoveries (w/ decaying recovery prob from p)
#  This version uses C++ for HMM forward algorithm 
#

###
###  Libraries and Subroutines 
###

#library(rmutil)

logit <-function(x){
  log(x/(1-x))
}

fwdalg <- CJSRL5fwdalg  # homogeneous recovery probability

pll <- function(theta){
  p=theta[1]
  psi=theta[2]
  beta=theta[-(1:2)]
  pll=-fwdalg(Y,X,beta,p,psi,FL)$ll-sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
#  pll=-fwdalg(Y,X,beta,p,psi,FL)$ll-sum(dlaplace(beta,mu.beta,s.beta,log=TRUE))
  pll 
}

pll.2 <- function(theta){
  beta=theta
  pll=-fwdalg(Y,X,beta,p,psi,FL)$ll-sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
#  pll=-fwdalg(Y,X,beta,p,psi,FL)$ll-sum(dlaplace(beta,mu.beta,s.beta,log=TRUE))
  pll 
}

###
###  Setup Variables 
###

n=dim(Y)[1]
p.cov=(dim(X)[3])
J=dim(Y)[2]

FL=cbind(rep(1,n),rep(J,n)) # default first and last time of survey by individual
for(i in 1:n){
  FL[i,1]=min((1:J)[Y[i,]==1 & !is.na(Y[i,])])
  FL[i,2]=max((1:J)[!is.na(Y[i,])])
}

n.pred=dim(Y.pred)[1]

FL.pred=cbind(rep(1,n.pred),rep(J,n.pred)) # default first and last time of survey by prediction individual
for(i in 1:n.pred){
  FL.pred[i,1]=min((1:J)[Y.pred[i,]==1 & !is.na(Y.pred[i,])])
  FL.pred[i,2]=max((1:J)[!is.na(Y.pred[i,])])
}

###
###  Priors 
###

mu.beta=rep(0,p.cov)
s.beta=c(1.5,rep(s.reg,p.cov-1))

###
###  Starting Values 
###

if(is.null(p)){
  p=alpha.p/(alpha.p+beta.p)
}
if(is.null(psi)){
  psi=alpha.psi/(alpha.psi+beta.psi)
}

beta=rep(0,p.cov)
beta[1]=logit(.95)
#beta[-1]=rnorm(p.cov-1,0,.25)

###
###  Maximize Likelihood
###

if(!fix.ppsi){
  optim.out=optim(c(p,psi,beta),pll,method="BFGS")
  p.hat=optim.out$par[1]
  psi.hat=optim.out$par[2]
  beta.hat=optim.out$par[-(1:2)]
}

if(fix.ppsi){
  optim.out=optim(beta,pll.2,method="BFGS")
  p.hat=p
  psi.hat=psi
  beta.hat=optim.out$par
}

###
###  Calculate Predictive Score 
###
  
ll.pred=fwdalg(Y.pred,X.pred,beta.hat,p.hat,psi.hat,FL.pred)$ll
score=-2*ll.pred

###
###  Write Output 
###

list(score=score,beta.hat=beta.hat,p.hat=p.hat,psi.hat=psi.hat)

}
