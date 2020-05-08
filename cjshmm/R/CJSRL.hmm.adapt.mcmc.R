CJSRL.hmm.adapt.mcmc <- function(Y,X,n.mcmc,beta.tune=0.1,s.reg=1.5,p=0.95,p.tune=0.01,psi=0.9,psi.tune=0.01,recov.homog=FALSE,adapt.iter=500,plot.trace=FALSE){

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
n.burn=round(.2*n.mcmc)

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

ll=fwdalg(Y,X,beta,p,psi,FL)$ll

beta.mean=rep(0,p.cov)
p.mean=0
psi.mean=0
D.bar=0

adapt.iter=1000  	# adaptation interval 
keep.vec=rep(0,n.mcmc)

acc.target=.3

###
###  Begin MCMC Loop 
###

for(k in 1:n.mcmc){
  if((k %% adapt.iter)==0){  
    cat(k," ")
    if(plot.trace){
      layout(matrix(c(1,1,2,3),4,1))
      matplot(t(beta.save[,1:(k-1)]),type="l",lty=1,main="trace plot",ylab=bquote(beta),xlab="iteration")
      matplot(cbind(p.save[1:(k-1)],psi.save[1:(k-1)]),type="l",lty=1,main="",ylab="detection and recovery",xlab="iteration",ylim=c(0,1))
      plot(cumsum(keep.vec[1:k])/1:k,type="l",ylim=c(0,1),main=paste("beta.tune =",round(beta.tune,5)),ylab="acceptance rate",xlab="iteration")
      abline(h=acc.target,col=3)
      lines(cumsum(keep.vec[1:k])/1:k,col=1)
      abline(v=seq(0,n.burn,adapt.iter),lty=2,col=8)
      abline(v=n.burn,lty=2)
      legend("topright",lty=2,col=c(8,1),lwd=2,legend=c("adaptation","burn-in"),bg="white")
    }
  }

  ###
  ###  Adaptive proposal 
  ###

  mag=(1-k/n.burn)
  if(k>=adapt.iter & (k%%adapt.iter==0) & k<=n.burn){
    if(mean(keep.vec[(k-adapt.iter+1):(k-1)])<acc.target){
      beta.tune=beta.tune/(1+.5*mag)
    }
    if(mean(keep.vec[(k-adapt.iter+1):(k-1)])>acc.target){
      beta.tune=beta.tune*(1+.5*mag)
    }
  }
  
  beta.star=rnorm(p.cov,beta,beta.tune)

  ###
  ###  Sample beta 
  ###

  ll.star=0
  ll.star=fwdalg(Y,X,beta.star,p,psi,FL)$ll
  mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))
  mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
  mh=exp(mh.1-mh.2)
  if(mh > runif(1) & !is.na(mh)){
    beta=beta.star 
    ll=ll.star
    keep.vec[k]=1
  }

  ###
  ###  Sample p and psi jointly
  ###

  if(!is.null(p.tune) & !is.null(psi.tune)){ 
    p.star=rnorm(1,p,p.tune)
    psi.star=rnorm(1,psi,psi.tune)
 
    if(p.star>0 & p.star<1 & psi.star>0 & psi.star<1){ 
      ll.star=0
      ll.star=fwdalg(Y,X,beta,p.star,psi.star,FL)$ll
      mh.1=ll.star+dbeta(p.star,alpha.p,beta.p,log=TRUE)+dbeta(psi.star,alpha.psi,beta.psi,log=TRUE)
      mh.2=ll+dbeta(p,alpha.p,beta.p,log=TRUE)+dbeta(psi,alpha.psi,beta.psi,log=TRUE)
      mh=exp(mh.1-mh.2)
      if(mh > runif(1) & !is.na(mh)){
        p=p.star 
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

};  cat("\n")

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
