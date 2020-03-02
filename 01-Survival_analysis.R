library(jagsUI)
library(stringr)
library(dplyr)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Script inputs________#
spp <- "GRSP" # BAIS or GRSP
model.file <- "grass-surv-scripts/model_prototype.jags"

# MCMC values
nc <- 3 # number of chains
nb <- 10000 # burn in
ni <- 20000 # number of iterations
nt <- 10 # thinning

save.out <- str_c("mod_prototype_", spp)
#______________________________#


# Data objects to send to JAGS
data.nams <- c("ymat", "first", "last", "nBird", "nSite", "nSeason", "nDOS",
             "SeasonInd", "SiteInd", "DOS", "time_since_depl", "after_depl")

# Stuff to save from JAGS
parameters <- c("B0.mean", "sigma.B0", "B0", "B.DOS", "B.DOS2", "B.trans", "P.trans")

# Function for setting initial values in JAGS
inits <- function()
  list(B0.mean = rnorm(1, 4.8, 1), sigma.B0 = runif(1), B.DOS = rnorm(1), B.DOS2 = rnorm(1), B.trans = rnorm(1))

# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- data.spp$Covs$firstDay
last <- data.spp$Covs$lastDay
nBird <- nrow(ymat)
nDOS <- ncol(ymat)
SeasonInd <- data.spp$Covs$SeasonInd
nSeason <- max(SeasonInd)
SiteInd <- data.spp$Covs$SiteInd
nSite <- max(SiteInd)

# Covariates #
DOS <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
DOSdepl <- data.spp$Covs$DOSdepl
time_since_depl <- DOS - DOSdepl
after_depl <- (time_since_depl > 0)*1
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])

# Fit model
data <- list()
for(i in 1:length(data.nams)) data[[length(data) + 1]] <- eval(as.name(data.nams[[i]]))
names(data) <- data.nams

st.time <- Sys.time()
out <- jagsUI(data, inits, parameters.to.save = parameters, model.file, n.thin=nt, n.chains=nc,
              n.burnin=nb, n.iter=ni, parallel=TRUE)
end.time <- Sys.time()
run.time <- end.time - st.time
run.time
rm(st.time,end.time)

# Check the basics
max(out$summary[,"Rhat"])
#sort(out$summary[,"Rhat"], decreasing = T)[1:100]

min(out$summary[,"n.eff"])
#sort(out$summary[,"n.eff"])[1:50]

# Save output
library(R.utils)
saveObject(out, save.out)
