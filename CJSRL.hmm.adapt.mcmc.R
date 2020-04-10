CJSRL.hmm.adapt.mcmc <- function(Y,X,n.mcmc,beta.tune=0.1,s.reg=1.5,p=0.95,p.tune=0.01,psi=0.9,psi.tune=0.01,recov.homog=FALSE){

#
#  Fits CJS model with dead recoveries using HMM perspective and forward algorithm for likelihood
#  Langrock and King (2013) with extension to handle long dead recoveries (w/ decaying recovery prob from p)
#  This version uses C++ for HMM forward algorithm 
#  Uses adaptive tuning 
#

###
###  Libraries and Subroutines 
###

logit <-function(x){
  log(x/(1-x))
}

logit.inv <-function(x){
  exp(x)/(1+exp(x))
}

library(mvnfast)

if(recov.homog){
  fwdalg <- CJSRL5fwdalg 
}
if(!recov.homog){
  fwdalg <- CJSRL4fwdalg 
}

###
###  Setup Variables 
###

n=dim(Y)[1]
p.cov=(dim(X)[3])
J=dim(Y)[2]
n.burn=round(.25*n.mcmc)

beta.save=matrix(0,p.cov,n.mcmc)
p.save=rep(0,n.mcmc)
psi.save=rep(0,n.mcmc)

###
###  Priors 
###

mu.beta=rep(0,p.cov)
s.beta=c(1.5,rep(s.reg,p.cov-1))

alpha.p=92
beta.p=8

alpha.psi=40
beta.psi=10

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
phi=matrix(NA,n,J)
tmp.sum=0
for(i in 1:n){
  phi[i,]=logit.inv(X[i,,]%*%beta)
}

#ll=CJSRL4fwdalg(Y,X,beta,p,psi)$ll
ll=fwdalg(Y,X,beta,p,psi)$ll

beta.mean=rep(0,p.cov)
p.mean=0
psi.mean=0
D.bar=0

###
###  Begin MCMC Loop 
###

for(k in 1:n.mcmc){
  if((k %% 1000)==0)  cat(k," ")

  ###
  ###  Adaptive proposal 
  ###
  
  if(k<1000){
    beta.star=rnorm(p.cov,beta,beta.tune)
  }
  if((k%%1000==0) & (k<=n.burn)){
    mu.prop=apply(beta.save[,(k-999):k],1,mean)
    Sig.prop=cov(t(beta.save[,(k-999):k]))
  }
  if(k>=1000){
    beta.star=as.vector(rmvn(1,mu.prop,Sig.prop))
  }

  ###
  ###  Sample beta 
  ###

  ll.star=0
#  ll.star=CJSRL4fwdalg(Y,X,beta.star,p,psi)$ll
  ll.star=fwdalg(Y,X,beta.star,p,psi)$ll
  if(k<1000){
    mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))
    mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
  }
  if(k>=1000){
    mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))+dmvn(beta,mu.prop,Sig.prop,log=TRUE)
    mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))+dmvn(beta.star,mu.prop,Sig.prop,log=TRUE)
  }
  mh=exp(mh.1-mh.2)
  if(mh > runif(1)){
    beta=beta.star 
    ll=ll.star
  }

  ###
  ###  Sample p 
  ###

  if(!is.null(p.tune)){ 
    p.star=rnorm(1,p,p.tune)
 
    if(p.star>0 & p.star<1){ 
      ll.star=0
#      ll.star=CJSRL4fwdalg(Y,X,beta,p.star,psi)$ll
      ll.star=fwdalg(Y,X,beta,p.star,psi)$ll
      mh.1=ll.star+dbeta(p.star,alpha.p,beta.p,log=TRUE)
      mh.2=ll+dbeta(p,alpha.p,beta.p,log=TRUE)
      mh=exp(mh.1-mh.2)
      if(mh > runif(1)){
        p=p.star 
        ll=ll.star
      }
    }
  }

  ###
  ###  Sample psi 
  ###

  if(!is.null(psi.tune)){ 
    psi.star=rnorm(1,psi,psi.tune)
 
    if(psi.star>0 & psi.star<1){ 
      ll.star=0
#      ll.star=CJSRL4fwdalg(Y,X,beta,p,psi.star)$ll
      ll.star=fwdalg(Y,X,beta,p,psi.star)$ll
      mh.1=ll.star+dbeta(psi.star,alpha.psi,beta.psi,log=TRUE)
      mh.2=ll+dbeta(psi,alpha.psi,beta.psi,log=TRUE)
      mh=exp(mh.1-mh.2)
      if(mh > runif(1)){
        psi=psi.star 
        ll=ll.star
      }
    }
  }

  ###
  ###  Save Samples 
  ###

  beta.save[,k]=beta
  if(k > n.burn){
    beta.mean=beta.mean+beta/(n.mcmc-n.burn)
    p.mean=p.mean+p/(n.mcmc-n.burn)
    psi.mean=psi.mean+psi/(n.mcmc-n.burn)
    D.bar=D.bar-2*ll/(n.mcmc-n.burn)
  }
  p.save[k]=p
  psi.save[k]=psi

}; cat("\n")

###
###  Calculate DIC 
###
  
#D.hat=-2*CJSRL4fwdalg(Y,X,beta.mean,p.mean,psi.mean)$ll
D.hat=-2*fwdalg(Y,X,beta.mean,p.mean,psi.mean)$ll
pD=D.bar-D.hat
DIC=D.hat+2*pD

###
###  Write Output 
###

list(beta.save=beta.save,n.mcmc=n.mcmc,DIC=DIC,pD=pD,p.save=p.save,psi.save=psi.save,n.burn=n.burn)

}
