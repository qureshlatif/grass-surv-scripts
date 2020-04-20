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
#  fwdalg <- CJSRL6fwdalg  # version to look for problematic individuals by NA in log likelihood
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
n.burn=round(.4*n.mcmc)

beta.save=matrix(0,p.cov,n.mcmc)
p.save=rep(0,n.mcmc)
psi.save=rep(0,n.mcmc)

FL=cbind(rep(1,n),rep(J,n)) # default first and last time of survey by individual
for(i in 1:n){
  FL[i,1]=min((1:J)[Y[i,]==1 & !is.na(Y[i,])])
  FL[i,2]=max((1:J)[!is.na(Y[i,])])
}

###
###  Priors 
###

mu.beta=rep(0,p.cov)
s.beta=c(1.5,rep(s.reg,p.cov-1))

alpha.p=4
beta.p=1

alpha.psi=1
beta.psi=1

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

tmp=fwdalg(Y,X,beta,p,psi,FL)
ll=tmp$ll
#plot(tmp$ll.vec,type="l")

beta.mean=rep(0,p.cov)
p.mean=0
psi.mean=0
D.bar=0

#adapt.iter=5000  	# adaptation period
adapt.iter=n.mcmc+1
s.d=(2.4^2)/p.cov 	# adaptation scaling constant
nug=.01		  	# "nugget" effect to keep cov from being singular

###
###  Begin MCMC Loop 
###

for(k in 1:n.mcmc){
  if((k %% 500)==0){  
    cat(k," ")
    matplot(t(beta.save[,1:(k-1)]),type="l",lty=1)
  }

  ###
  ###  Adaptive proposal (Haario et al., 2001)
  ###
  
  if(k<adapt.iter){
    beta.star=rnorm(p.cov,beta,beta.tune)
  }
  if(k>=adapt.iter & (k%%adapt.iter==0) & k<=n.burn){
    Sig.prop=s.d*cov(t(beta.save[,1:(k-1)]))+s.d*nug*diag(p.cov)
  }
#  if(k==adapt.iter){
#    Sig.prop=s.d*cov(t(beta.save[,1:(k-1)]))+s.d*nug*diag(p.cov)
#    beta.mn.kminus1=apply(beta.save[,1:(k-2)],1,mean)
#    beta.mn.k=apply(beta.save[,1:(k-1)],1,mean)
#  }
#  if(k>adapt.iter & k<=n.burn){
#    Sig.prop=((k-2)/(k-1))*Sig.prop+s.d/(k-1)*((k-1)*beta.mn.kminus1%*%t(beta.mn.kminus1)+k*(beta.mn.k%*%t(beta.mn.k))+nug*diag(p.cov))
#    beta.mn.kminus1=beta.mn.k
#    beta.mn.k=beta.mn.k+(beta.save[,k-1]-beta.mn.k)/k
#  }
  if(k>=adapt.iter){
    beta.star=as.vector(rmvn(1,beta,Sig.prop))
#    beta.star=as.vector(rmvnorm(1,beta,Sig.prop))
  }

  ###
  ###  Sample beta 
  ###

  ll.star=0
  tmp=fwdalg(Y,X,beta.star,p,psi,FL)
  ll.star=tmp$ll
  if(k<adapt.iter){
    mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))
    mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
  }
  if(k>=adapt.iter){
    mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))+dmvn(beta,beta.star,Sig.prop,log=TRUE)
    mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))+dmvn(beta.star,beta,Sig.prop,log=TRUE)
#    mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))+dmvnorm(beta,beta.star,Sig.prop,log=TRUE)
#    mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))+dmvnorm(beta.star,beta,Sig.prop,log=TRUE)
  }
  mh=exp(mh.1-mh.2)
#  if(is.na(mh)){
#    plot(tmp$ll.vec,type="l",main=k,ylim=c(-150,0))
#    abline(v=(1:n)[is.na(tmp$ll.vec)],col=2)
#  }
  if(mh > runif(1) & !is.na(mh)){
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
      ll.star=fwdalg(Y,X,beta,p.star,psi,FL)$ll
      mh.1=ll.star+dbeta(p.star,alpha.p,beta.p,log=TRUE)
      mh.2=ll+dbeta(p,alpha.p,beta.p,log=TRUE)
      mh=exp(mh.1-mh.2)
      if(mh > runif(1) & !is.na(mh)){
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
      ll.star=fwdalg(Y,X,beta,p,psi.star,FL)$ll
      mh.1=ll.star+dbeta(psi.star,alpha.psi,beta.psi,log=TRUE)
      mh.2=ll+dbeta(psi,alpha.psi,beta.psi,log=TRUE)
      mh=exp(mh.1-mh.2)
      if(mh > runif(1) & !is.na(mh)){
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
  
D.hat=-2*fwdalg(Y,X,beta.mean,p.mean,psi.mean,FL)$ll
pD=D.bar-D.hat
DIC=D.hat+2*pD

###
###  Write Output 
###

list(beta.save=beta.save,n.mcmc=n.mcmc,DIC=DIC,pD=pD,p.save=p.save,psi.save=psi.save,n.burn=n.burn,FL=FL,p.cov=p.cov)

}
