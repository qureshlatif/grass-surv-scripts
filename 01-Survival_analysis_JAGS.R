#library(jagsUI)
library(saveJAGS)
library(stringr)
library(dplyr)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
mod.nam <- "JAGS"
scripts.loc <- "grass-surv-scripts/"
saveJAGS.loc <- "saveJAGS/"
model.file <- str_c(scripts.loc, "model_CJSRL_SiteXSeason_Transmitter.jags")

# MCMC values
nc <- 3 # number of chains
nb <- 10 #0 #1000 # burn in
ni <- 20 #20000 # number of iterations
nt <- 1# 10 # thinning
ns <- 50 # number of files for saveJAGS

save.out <- str_c("mod_CJSRL_SiteXSeason_Transmitter_", spp)
#______________________________#

# Data objects to send to JAGS
data.nams <- c("Y.alive", "Y.dead", "first", "last",
               "nBird", "nSite", "nSeason", "nDOS", "ncovs",
               "SeasonInd", "SiteInd", "X",
               "DOSdepl", "time_since_depl", "after_depl")

# Stuff to save from JAGS
parameters <- c("B0.mean", "B0.sd", "B0", "B", "B.trans", "P.trans", "p", "psi", "z")

# Compile data #
source(str_c(scripts.loc, "Data_processing_", mod.nam, ".R"))

# Function for setting initial values in JAGS
inits <- function()
  list(z = z.init, B0 = matrix(rnorm(nSite*nSeason, 4.8, 1), nSite, nSeason), B = rep(0,ncovs), p = 0.9, psi = 0.5) #B0 = rnorm(1, 4.8, 1)

# Fit model
data <- list()
for(i in 1:length(data.nams)) data[[length(data) + 1]] <- eval(as.name(data.nams[[i]]))
names(data) <- data.nams

st.time <- Sys.time()
out <- jagsUI(data, inits, parameters.to.save = parameters, model.file, n.thin=nt, n.chains=nc,
              n.burnin=nb, n.iter=ni, parallel=TRUE)
#if(!file.exists(str_c(saveJAGS.loc, save.out))) dir.create(str_c(saveJAGS.loc, save.out))
#out <- saveJAGS(data = data, inits = inits, params = parameters, modelFile = model.file, thin = nt, chains = nc, # taking up saveJAGS instead to save as we go.
#                burnin = nb, sample2save = ((ni/nt)/ns), nSaves = ns, fileStub = str_c(saveJAGS.loc, save.out, "/modsave"))
end.time <- Sys.time()
run.time <- end.time - st.time
run.time
rm(st.time,end.time)

#out <- resumeJAGS(fileStub = str_c("saveJAGS/", save.out, "/modsave"), nSaves = 40)

# Gather, combine, and summarize JAGS saves from hard drive #
rsav <- recoverSaves(str_c(saveJAGS.loc, save.out, "/modsave"))
mod.raw <- combineSaves(rsav, burnFiles = 5, thin = 10)
#Rhat <- gelman.diag(mod.raw)$psrf[, 2]
#neff <- effectiveSize(mod.raw)
mod <- mcmcOutput(mod.raw)
sumTab <- summary(mod, MCEpc = F, n.eff = T, f = T, overlap0 = T, verbose = F)
mod <- list(mcmcOutput = mod, sims.list = simsList(mod.raw), summary = sumTab)

# Check the basics
#max(out$summary[,"Rhat"])
#max(Rhat)
#sort(Rhat, decreasing = T)
#traceplot(mod.raw[, "B.drone[4]"])
#max(mod$summary[, "Rhat"], na.rm = T)
max(mod$summary[-which(str_sub(dimnames(mod$summary)[[1]], 1, 2) == "z["), "Rhat"], na.rm = T)
#mod$summary[which(round(mod$summary[, "Rhat"], digits = 1) > 1.1), ]

#min(out$summary[,"n.eff"])
#min(neff)
#sort(neff)
min(mod$summary[, "n.eff"], na.rm = T)
#sort(mod$summary[, "n.eff"])[1:50]

# # traceplots (***not sure this will work if z's are saved and included.) #
# pdf(file=str_c(save.out, '_traceplots.pdf'))
# plot.params <- params.saved <- parameters
# for(i in 1:length(plot.params)) {
#   par.i <- plot.params[i]
#   pars.lst <- params.saved[which(substring(params.saved,1,nchar(par.i))==par.i)]
#   pars.cols <- c()
#   for(j in 1:length(pars.lst)) {
#     pars.cols <- c(pars.cols,which(substring(colnames(mod.raw$AA),1,nchar(pars.lst[j]))==pars.lst[j]))
#   }
#   
#   if(length(pars.cols)<=9) {
#     par(mfrow=c(ceiling(sqrt(length(pars.cols))),ceiling(sqrt(length(pars.cols)))),xpd=NA,mar=c(5,4,1,1))
#     for(j in pars.cols) {
#       matplot(cbind(mod.raw$AA[,j],mod.raw$AB[,j],mod.raw$AC[,j]),type='l',lty=1,ylab=colnames(mod.raw$AA)[j])
#     }
#   }
#   if(length(pars.cols)>9) {
#     for(j in 1:length(pars.cols)) {
#       if((j-1)%%9==0) {par(mfrow=c(3,3),xpd=NA,mar=c(5,4,1,1))}
#       matplot(cbind(mod.raw$AA[,pars.cols[j]],mod.raw$AB[,pars.cols[j]],mod.raw$AC[,pars.cols[j]]),type='l',lty=1,ylab=colnames(mod.raw$AA)[pars.cols[j]])
#     }
#   }
# }
# dev.off()

# Save output
library(R.utils)
#saveObject(out, save.out)
#mod <- simsList(mod.raw)
#mod <- list(sims.list = mod, Rhat = Rhat, neff = neff)
saveObject(mod, save.out)
