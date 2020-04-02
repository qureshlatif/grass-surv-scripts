CJS.hmm.cpp.mcmc <- function(Y,X,last.alive,n.mcmc,beta.tune=0.1,s.reg=1.5){

#
#  Fits CJS model using HMM perspective and forward algorithm for likelihood
#  Assumes that detection probability p=1 for now. 
#
#  This version uses C++ for HMM forward algorithm 
#

###
###  Libraries and Subroutines 
###

#library(Rcpp)
#library(RcppArmadillo)
#Rcpp::sourceCpp('fwdalg.cpp')

logit <-function(x){
  log(x/(1-x))
}

logit.inv <-function(x){
  exp(x)/(1+exp(x))
}

###
###  Setup Variables 
###

n=dim(Y)[1]
p=(dim(X)[3])
J=dim(Y)[2]
n.burn=round(.2*n.mcmc)

beta.save=matrix(0,p,n.mcmc)

###
###  Priors 
###

mu.beta=rep(0,p)
s.beta=c(1.5,rep(s.reg,p-1))

###
###  Starting Values 
###

mu.beta=rep(0,p)
s.reg=1.5       # make small to regularize
s.beta=c(1.5,rep(s.reg,p-1))

beta=rep(0,p)
beta[1]=logit(.95)
#beta[-1]=rnorm(p-1,0,.25)
phi=matrix(NA,n,J)
Z=Y
tmp.sum=0
for(i in 1:n){
  phi[i,]=logit.inv(X[i,,]%*%beta)
  Z[i,1:last.alive[i]]=1
  if(sum(Z[i,]==0,na.rm=TRUE)>0){
    idx.0=max((1:J)[Z[i,]==0],na.rm=TRUE)
    Z[i,idx.0:J]=0
  }
}

p.detect=1

ll=fwdalg(Z,X,beta,p.detect)$ll

beta.mean=rep(0,p)
D.bar=0

###
###  Begin MCMC Loop 
###

for(k in 1:n.mcmc){
  if((k %% 1000)==0)  cat(k," ")

  ###
  ###  Sample beta 
  ###

  beta.star=rnorm(p,beta,beta.tune)

  ll.star=0
  ll.star=fwdalg(Z,X,beta.star,p.detect)$ll
  mh.1=ll.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))
  mh.2=ll+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
  mh=exp(mh.1-mh.2)
  if(mh > runif(1)){
    beta=beta.star 
    ll=ll.star
  }

  ###
  ###  Save Samples 
  ###

  beta.save[,k]=beta
  if(k > n.burn){
    beta.mean=beta.mean+beta/(n.mcmc-n.burn)
    D.bar=D.bar-2*ll/(n.mcmc-n.burn)
  }

}; cat("\n")

###
###  Calculate DIC 
###
  
D.hat=-2*fwdalg(Z,X,beta.mean,p.detect)$ll
pD=D.bar-D.hat
DIC=D.hat+2*pD

###
###  Write Output 
###

list(beta.save=beta.save,n.mcmc=n.mcmc,DIC=DIC,pD=pD)

}
