library(jagsUI)
#library(saveJAGS)
library(tidyverse)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_KnownFate.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
model.file <- "grass-surv-scripts/model_LogExp.jags"
maxInt <- 1

# MCMC values
nc <- 3 # number of chains
nb <- 500 # burn in
ni <- 1000 # number of iterations
nt <- 1#10 # thinning

save.out <- str_c("mod_LogExp_Dot_", spp)
#______________________________#

# Data objects to send to JAGS
data.nams <- c("Y", "Int", "nObs")

# Stuff to save from JAGS
parameters <- c("B0", "S")

# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
IntData <- data.spp$obsInt
IntData <- IntData %>% filter(survive != 9) #& IntLength <= maxInt)
Y <- IntData$survive
nObs <- length(Y)
Int <- IntData$IntLength

# Function for setting initial values in JAGS
inits <- function()
  list(B0 = rnorm(1))

# Fit model
data <- list()
for(i in 1:length(data.nams)) data[[length(data) + 1]] <- eval(as.name(data.nams[[i]]))
names(data) <- data.nams

st.time <- Sys.time()
out <- jagsUI(data, inits, parameters.to.save = parameters, model.file, n.thin=nt, n.chains=nc,
              n.burnin=nb, n.iter=ni, parallel=TRUE)
#if(!file.exists(str_c("saveJAGS/", save.out))) dir.create(str_c("saveJAGS/", save.out))
#out <- saveJAGS(data = data, inits = inits, params = parameters, modelFile = model.file, thin = nt, chains = nc, # taking up saveJAGS instead to save as we go.
#                burnin = nb, sample2save = ((ni/nt)/50), nSaves = 50, fileStub = str_c("saveJAGS/", save.out, "/modsave"))
end.time <- Sys.time()
run.time <- end.time - st.time
run.time
rm(st.time,end.time)


#out <- resumeJAGS(fileStub = str_c("saveJAGS/", save.out, "/modsave"), nSaves = 40)

# Gather, combine, and summarize JAGS saves from hard drive #
rsav <- recoverSaves(str_c("saveJAGS/", save.out, "/modsave"))
mod.raw <- combineSaves(rsav)
Rhat <- gelman.diag(mod.raw)$psrf[, 2]
neff <- effectiveSize(mod.raw)

# Check the basics
#max(out$summary[,"Rhat"])
max(Rhat)
#sort(Rhat, decreasing = T)
#traceplot(mod.raw[, "B.drone[4]"])

#min(out$summary[,"n.eff"])
min(neff)
#sort(neff)

# traceplots #
pdf(file=str_c(save.out, '_traceplots.pdf'))
plot.params <- params.saved <- parameters
for(i in 1:length(plot.params)) {
  par.i <- plot.params[i]
  pars.lst <- params.saved[which(substring(params.saved,1,nchar(par.i))==par.i)]
  pars.cols <- c()
  for(j in 1:length(pars.lst)) {
    pars.cols <- c(pars.cols,which(substring(colnames(mod.raw$AA),1,nchar(pars.lst[j]))==pars.lst[j]))
  }
  
  if(length(pars.cols)<=9) {
    par(mfrow=c(ceiling(sqrt(length(pars.cols))),ceiling(sqrt(length(pars.cols)))),xpd=NA,mar=c(5,4,1,1))
    for(j in pars.cols) {
      matplot(cbind(mod.raw$AA[,j],mod.raw$AB[,j],mod.raw$AC[,j]),type='l',lty=1,ylab=colnames(mod.raw$AA)[j])
    }
  }
  if(length(pars.cols)>9) {
    for(j in 1:length(pars.cols)) {
      if((j-1)%%9==0) {par(mfrow=c(3,3),xpd=NA,mar=c(5,4,1,1))}
      matplot(cbind(mod.raw$AA[,pars.cols[j]],mod.raw$AB[,pars.cols[j]],mod.raw$AC[,pars.cols[j]]),type='l',lty=1,ylab=colnames(mod.raw$AA)[pars.cols[j]])
    }
  }
}
dev.off()

# Save output
library(R.utils)
#saveObject(out, save.out)
mod <- simsList(mod.raw)
mod <- list(sims.list = mod, Rhat = Rhat, neff = neff)
saveObject(mod, save.out)
