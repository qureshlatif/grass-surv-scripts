library(jagsUI)
library(stringr)
library(dplyr)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Script inputs________#
spp <- "GRSP" # BAIS or GRSP
model.file <- "grass-surv-scripts/model_prototype_Veg.jags"

# MCMC values
nc <- 3 # number of chains
nb <- 1000 # burn in
ni <- 6000 # number of iterations
nt <- 10 # thinning

save.out <- str_c("mod_prototype_Veg_", spp)
#______________________________#


# Data objects to send to JAGS
data.nams <- c("ymat", "first", "last", "nBird", "nSite", "nSeason", "nDOS",
             "SeasonInd", "SiteInd", "DOS", "time_since_depl", "after_depl",
             
             "hierbas.z", "hierbas.missing", "hierbas.sd", "hierbas.lower", "hierbas.upper",
             "hierba_ht.z", "hierba_ht.missing", "hierba_ht.sd", "hierba_ht.lower",
             "arbusto.z", "arbusto.missing", "arbusto.sd", "arbusto.lower", "arbusto.upper",
             "pastos.z", "pastos.missing", "pastos.sd", "pastos.lower", "pastos.upper",
             "pasto_ht.z", "pasto_ht.missing", "pasto_ht.sd", "pasto_ht.lower",
             "salsola.z", "salsola.missing", "salsola.sd", "salsola.lower", "salsola.upper",
             "salsola_ht.z", "salsola_ht.missing", "salsola_ht.sd", "salsola_ht.lower",
             "arbusto_cv.z", "arbusto_cv.missing", "arbusto_cv.sd", "arbusto_cv.lower",
             
             "peso.z")

# Stuff to save from JAGS
parameters <- c("B0.mean", "sigma.B0", "B0", "B.DOS", "B.DOS2", "B.trans", "P.trans",
                "B.hierbas", "B.hierba_ht", "B.arbusto", "B.pastos", "B.pasto_ht",
                "B.salsola", "B.salsola_ht", "B.arbusto_cv", "B.peso")

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

hierbas.x <- data.spp$Covs$hierbas
hierbas.z <- hierbas.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
hierbas.missing <- is.na(hierbas.z)*1
hierbas.sd <- sd(hierbas.z, na.rm = T)
hierbas.z[which(is.na(hierbas.z))] <- mean(hierbas.z, na.rm = T)
hierbas.lower <- (0 - mean(hierbas.x, na.rm = T)) / sd(hierbas.x, na.rm = T)
hierbas.upper <- (100 - mean(hierbas.x, na.rm = T)) / sd(hierbas.x, na.rm = T)

hierba_ht.x <- data.spp$Covs$hierba_ht
hierba_ht.z <- hierba_ht.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
hierba_ht.missing <- is.na(hierba_ht.z)*1
hierba_ht.sd <- sd(hierba_ht.z, na.rm = T)
hierba_ht.z[which(is.na(hierba_ht.z))] <- mean(hierba_ht.z, na.rm = T)
hierba_ht.lower <- (0 - mean(hierba_ht.x, na.rm = T)) / sd(hierba_ht.x, na.rm = T)

arbusto.x <- data.spp$Covs$arbusto
arbusto.z <- arbusto.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
arbusto.missing <- is.na(arbusto.z)*1
arbusto.sd <- sd(arbusto.z, na.rm = T)
arbusto.z[which(is.na(arbusto.z))] <- mean(arbusto.z, na.rm = T)
arbusto.lower <- (0 - mean(arbusto.x, na.rm = T)) / sd(arbusto.x, na.rm = T)
arbusto.upper <- (100 - mean(arbusto.x, na.rm = T)) / sd(arbusto.x, na.rm = T)

pastos.x <- data.spp$Covs$pastos
pastos.z <- pastos.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
pastos.missing <- is.na(pastos.z)*1
pastos.sd <- data.spp$Covs$pastos_predsd / sd(pastos.x, na.rm = T)
pastos.sd[which(is.na(pastos.sd))] <- sd(pastos.z, na.rm = T)
pastos.z[which(is.na(pastos.z))] <- ((data.spp$Covs$pastos_pred - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T))[which(is.na(pastos.z))]
pastos.z[which(is.na(pastos.z))] <- mean(pastos.z, na.rm = T)
pastos.lower <- (0 - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T)
pastos.upper <- (100 - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T)

pasto_ht.x <- data.spp$Covs$pasto_ht
pasto_ht.z <- pasto_ht.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
pasto_ht.missing <- is.na(pasto_ht.z)*1
pasto_ht.sd <- sd(pasto_ht.z, na.rm = T)
pasto_ht.z[which(is.na(pasto_ht.z))] <- mean(pasto_ht.z, na.rm = T)
pasto_ht.lower <- (0 - mean(pasto_ht.x, na.rm = T)) / sd(pasto_ht.x, na.rm = T)

salsola.x <- data.spp$Covs$salsola
salsola.z <- salsola.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
salsola.missing <- is.na(salsola.z)*1
salsola.sd <- sd(salsola.z, na.rm = T)
salsola.z[which(is.na(salsola.z))] <- mean(salsola.z, na.rm = T)
salsola.lower <- (0 - mean(salsola.x, na.rm = T)) / sd(salsola.x, na.rm = T)
salsola.upper <- (100 - mean(salsola.x, na.rm = T)) / sd(salsola.x, na.rm = T)

salsola_ht.x <- data.spp$Covs$salsola_ht
salsola_ht.z <- salsola_ht.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
salsola_ht.missing <- is.na(salsola_ht.z)*1
salsola_ht.sd <- sd(salsola_ht.z, na.rm = T)
salsola_ht.z[which(is.na(salsola_ht.z))] <- mean(salsola_ht.z, na.rm = T)
salsola_ht.lower <- (0 - mean(salsola_ht.x, na.rm = T)) / sd(salsola_ht.x, na.rm = T)

arbusto_cv.x <- data.spp$Covs$arbusto_cv
arbusto_cv.z <- arbusto_cv.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
arbusto_cv.missing <- is.na(arbusto_cv.z)*1
arbusto_cv.sd <- sd(arbusto_cv.z, na.rm = T)
arbusto_cv.z[which(is.na(arbusto_cv.z))] <- mean(arbusto_cv.z, na.rm = T)
arbusto_cv.lower <- (0 - mean(arbusto_cv.x, na.rm = T)) / sd(arbusto_cv.x, na.rm = T)

peso.x <- data.spp$Covs$peso
peso.z <- peso.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))

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
