# Model from M Hooten #
surv.censor.mcmc <- function(Y, X, first, last.alive, last, n.mcmc, beta.tune=0.1,
                             s.reg=1.5, n.burnin = 1000, n.thin = 1) {
  
  #
  #  Survival Model for data censored to omit trailing NAs 
  #  and zeros after the first one (because we know it's dead)
  #
  
  ###
  ###  Subroutines
  ###
  
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
  
  beta.save=matrix(NA,p,ceiling((n.mcmc - n.burnin)/n.thin))
  
  ###
  ###  Priors and Starting Values 
  ###
  
  mu.beta=rep(0,p)
  s.beta=c(1.5,rep(s.reg,p-1))
  
  beta=rep(0,p)
  beta[1]=logit(.98)
  phi=matrix(NA, nrow = n, ncol = J)
  Z=Y
  samp.z.list=vector("list", n)	# times we need to sample Z
  tmp.sum=0
  for(i in 1:n){
    phi[i,]=logit.inv(X[i,,]%*%beta)
    Z[i, first[i]:last.alive[i]]=1
    samp.z.list[[i]]=(first[i]:last[i])[is.na(Z[i, first[i]:last[i]])]
    if(!is.null(samp.z.list[[i]])){
      for(j in samp.z.list[[i]]){
        Z[i,j]=rbinom(1,1,phi[i,j]*Z[i,(j-1)])
      } 
    }
    tmp.sum=ifelse(last[i]<J,
                   tmp.sum+sum(dbinom(Z[i,-c(1:first[i],(last[i]+1):J)][Z[i,-c(1:(first[i]-1),last[i]:J)]==1],1,
                               phi[i,-c(1:first[i],(last[i]+1):J)][Z[i,-c(1:(first[i]-1),last[i]:J)]==1],log=TRUE)),
                   ifelse(last[i]==J,
                          tmp.sum+sum(dbinom(Z[i,-c(1:first[i])][Z[i,-c(1:(first[i]-1),J)]==1],1,
                                      phi[i,-c(1:first[i])][Z[i,-c(1:(first[i]-1),J)]==1],log=TRUE)),
                          stop(str_c("last ",i," is greater than width of ymat"))))
  }
  phi.star=matrix(NA, nrow = n, ncol = J)
  
  ###
  ###  MCMC Loop 
  ###
  
  for(k in 1:n.mcmc){
    if(k%%1000==0) cat(k," ")
    
    ###
    ###  Sample beta
    ###
    
    beta.star=rnorm(p,beta,beta.tune)
    tmp.sum.star=0
    for(i in 1:n){
      phi.star[i,]=logit.inv(X[i,,]%*%beta.star)
      tmp.sum.star=ifelse(last[i]<J,
                          tmp.sum.star+sum(dbinom(Z[i,-c(1:first[i],(last[i]+1):J)][Z[i,-c(1:(first[i]-1),last[i]:J)]==1],1,
                                                  phi.star[i,-c(1:first[i],(last[i]+1):J)][Z[i,-c(1:(first[i]-1),last[i]:J)]==1],log=TRUE)),
                          tmp.sum.star+sum(dbinom(Z[i,-c(1:first[i])][Z[i,-c(1:(first[i]-1),J)]==1],1,
                                                  phi.star[i,-c(1:first[i])][Z[i,-c(1:(first[i]-1),J)]==1],log=TRUE)))
    }
    
    mh.1=tmp.sum.star+sum(dnorm(beta.star,mu.beta,s.beta,log=TRUE))
    mh.2=tmp.sum+sum(dnorm(beta,mu.beta,s.beta,log=TRUE))
    mh=exp(mh.1-mh.2)
    if(mh>runif(1)){
      beta=beta.star
      phi=phi.star
      tmp.sum=tmp.sum.star
    }
    
    ###
    ###  Sample Missing Z 
    ###
    
    for(i in 1:n){
      if(!is.null(samp.z.list[[i]])){
        for(j in samp.z.list[[i]]){
          if(Z[i,j-1]==1 & Z[i,j+1]==0){
            tmp.num=(1-phi[i,j+1])*phi[i,j]
            phi.tmp=tmp.num/(tmp.num+(1-phi[i,j])) 
            Z[i,j]=rbinom(1,1,phi.tmp)
          }
          if(Z[i,(j-1)]==0){
            Z[i,j]=0
          }
        } 
      }
    } 
    
    ###
    ###  Save samples 
    ###
    
    if(k > n.burnin & (k-n.burnin)%%n.thin == 0) beta.save[,((k-n.burnin)/n.thin)]=beta
    
  };cat("\n")
  
  ###
  ###  Write Output 
  ###
  
  list(beta.save=beta.save,n.mcmc=n.mcmc)
  
}
