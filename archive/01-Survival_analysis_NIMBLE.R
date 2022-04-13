library(nimble)
library(stringr)
library(dplyr)

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")
load("Data_compiled_MissingCovsImputed.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
source("grass-surv-scripts/archive/model_CJSRLHomog_Dot.nimble")

# MCMC values
nc <- 3 # number of chains
nb <- 1000 # burn in
ni <- 6000 # number of iterations
nt <- 1 # thinning

save.out <- str_c("mod_CJSRLHomog_Dot_", spp, "_nimble")
#______________________________#

# Data objects to send to JAGS
data.nams <- c("Y.alive", "Y.dead", "first", "last",
               "nBird", "nSite", "nSeason", "nDOS", "ncovs",
               "SeasonInd", "SiteInd", "X",
               "DOSdepl", "time_since_depl", "after_depl")

# Stuff to save from JAGS
#parameters <- c("B0", "B", "p", "psi") # With covariates
parameters <- c("B0", "p", "psi") # No covariates

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
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])
DOS2 <- DOS^2

DOSdepl <- first
time_since_depl <- DOS - DOSdepl
after_depl <- (time_since_depl > 0)*1

# hierbas.x <- data.spp$Covs$hierbas
# hierbas.z <- hierbas.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# hierbas.missing <- is.na(hierbas.z)*1
# hierbas.sd <- sd(hierbas.z, na.rm = T)
# hierbas.z[which(is.na(hierbas.z))] <- mean(hierbas.z, na.rm = T)
# hierbas.lower <- (0 - mean(hierbas.x, na.rm = T)) / sd(hierbas.x, na.rm = T)
# hierbas.upper <- (100 - mean(hierbas.x, na.rm = T)) / sd(hierbas.x, na.rm = T)
# 
# hierba_ht.x <- data.spp$Covs$hierba_ht
# hierba_ht.z <- hierba_ht.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# hierba_ht.missing <- is.na(hierba_ht.z)*1
# hierba_ht.sd <- sd(hierba_ht.z, na.rm = T)
# hierba_ht.z[which(is.na(hierba_ht.z))] <- mean(hierba_ht.z, na.rm = T)
# hierba_ht.lower <- (0 - mean(hierba_ht.x, na.rm = T)) / sd(hierba_ht.x, na.rm = T)
# 
# arbusto.x <- data.spp$Covs$arbusto
# arbusto.z <- arbusto.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# arbusto.missing <- is.na(arbusto.z)*1
# arbusto.sd <- sd(arbusto.z, na.rm = T)
# arbusto.z[which(is.na(arbusto.z))] <- mean(arbusto.z, na.rm = T)
# arbusto.lower <- (0 - mean(arbusto.x, na.rm = T)) / sd(arbusto.x, na.rm = T)
# arbusto.upper <- (100 - mean(arbusto.x, na.rm = T)) / sd(arbusto.x, na.rm = T)
# 
# pastos.x <- data.spp$Covs$pastos
# pastos.z <- pastos.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# pastos.missing <- is.na(pastos.z)*1
# pastos.sd <- data.spp$Covs$pastos_predsd / sd(pastos.x, na.rm = T)
# pastos.sd[which(is.na(pastos.sd))] <- sd(pastos.z, na.rm = T)
# pastos.z[which(is.na(pastos.z))] <- ((data.spp$Covs$pastos_pred - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T))[which(is.na(pastos.z))]
# pastos.z[which(is.na(pastos.z))] <- mean(pastos.z, na.rm = T)
# pastos.lower <- (0 - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T)
# pastos.upper <- (100 - mean(pastos.x, na.rm = T)) / sd(pastos.x, na.rm = T)
# 
# pasto_ht.x <- data.spp$Covs$pasto_ht
# pasto_ht.z <- pasto_ht.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# pasto_ht.missing <- is.na(pasto_ht.z)*1
# pasto_ht.sd <- sd(pasto_ht.z, na.rm = T)
# pasto_ht.z[which(is.na(pasto_ht.z))] <- mean(pasto_ht.z, na.rm = T)
# pasto_ht.lower <- (0 - mean(pasto_ht.x, na.rm = T)) / sd(pasto_ht.x, na.rm = T)
# 
# salsola.x <- data.spp$Covs$salsola
# salsola.z <- salsola.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# salsola.missing <- is.na(salsola.z)*1
# salsola.sd <- sd(salsola.z, na.rm = T)
# salsola.z[which(is.na(salsola.z))] <- mean(salsola.z, na.rm = T)
# salsola.lower <- (0 - mean(salsola.x, na.rm = T)) / sd(salsola.x, na.rm = T)
# salsola.upper <- (100 - mean(salsola.x, na.rm = T)) / sd(salsola.x, na.rm = T)
# 
# salsola_ht.x <- data.spp$Covs$salsola_ht
# salsola_ht.z <- salsola_ht.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# salsola_ht.missing <- is.na(salsola_ht.z)*1
# salsola_ht.sd <- sd(salsola_ht.z, na.rm = T)
# salsola_ht.z[which(is.na(salsola_ht.z))] <- mean(salsola_ht.z, na.rm = T)
# salsola_ht.lower <- (0 - mean(salsola_ht.x, na.rm = T)) / sd(salsola_ht.x, na.rm = T)
# 
# arbusto_cv.x <- data.spp$Covs$arbusto_cv
# arbusto_cv.z <- arbusto_cv.x %>%
#   (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))
# arbusto_cv.missing <- is.na(arbusto_cv.z)*1
# arbusto_cv.sd <- sd(arbusto_cv.z, na.rm = T)
# arbusto_cv.z[which(is.na(arbusto_cv.z))] <- mean(arbusto_cv.z, na.rm = T)
# arbusto_cv.lower <- (0 - mean(arbusto_cv.x, na.rm = T)) / sd(arbusto_cv.x, na.rm = T)

drone.z <- data.spp$Covs %>% ungroup() %>%
  select(Mesquite_5m:Distance_to_Fence) %>%
  select(contains("_100m"), Distance_to_Fence) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) %>%
  data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

drone2.z <- drone.z ^ 2

droneCV.z <- data.spp$Covs %>%
  select(Mesquite_5m_CV:Mean_Shrub_Height_500m_CV) %>%
  select(contains("_100m")) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) %>%
  data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

peso.x <- data.spp$Covs$peso
peso.z <- peso.x %>%
  (function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)) %>%
  (function(x) ifelse(is.na(x), mean(x, na.rm = T), x)) %>%
  array(., c(length(.), nDOS))

X <- abind::abind(DOS, DOS2, drone.z, drone2.z, droneCV.z, peso.z, along = 3)
ncovs <- dim(X)[3]
Y.alive <- (ymat == 1)*1
Y.dead <- (ymat == 2)*1
z.init <- (ymat == 1)*1
for(i in 1:length(first)) {
  z.init[i, (first[i]+1):data.spp$Covs$lastAlive[i]] <- 1
  z.init[i, first[i]] <- NA
  if(any(ymat[i, ] == 2, na.rm = T)) z.init[i, which(ymat[i, ] == 2)] <- 0
}

# Function for setting initial values in JAGS
#inits <- function()
#  list(z = z.init, B0 = rnorm(1, 4.8, 1), B = rep(0,ncovs), p = 0.9, psi = 0.5) # With covariates
inits <- function()
  list(z = z.init, B0 = rnorm(1, 4.8, 1), p = 0.9, psi = 0.5) # Dot model

# Fit model
data <- list()
for(i in 1:length(data.nams)) data[[length(data) + 1]] <- eval(as.name(data.nams[[i]]))
names(data) <- data.nams

st.time <- Sys.time()
#out <- jagsUI(data, inits, parameters.to.save = parameters, model.file, n.thin=nt, n.chains=nc,
#              n.burnin=nb, n.iter=ni, parallel=TRUE)
#if(!file.exists(str_c("saveJAGS/", save.out))) dir.create(str_c("saveJAGS/", save.out))
#out <- saveJAGS(data = data, inits = inits, params = parameters, modelFile = model.file, thin = nt, chains = nc, # taking up saveJAGS instead to save as we go.
#                burnin = nb, sample2save = ((ni/nt)/50), nSaves = 50, fileStub = str_c("saveJAGS/", save.out, "/modsave"))
out <- nimbleMCMC(code = model,
                  constants = data,
                  inits = inits,
                  monitors = parameters,
                  niter = ni,
                  nburnin = nb,
                  nchains = nc,
                  thin = nt,
                  summary = TRUE,
                  WAIC = TRUE)
end.time <- Sys.time()
run.time <- end.time - st.time
run.time
rm(st.time,end.time)


saveObject(mod, save.out)
