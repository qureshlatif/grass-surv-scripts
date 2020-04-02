library(stringr)
library(dplyr)
library(QSLpersonal)
library(tictoc)
library(Rcpp)
library(abind)
library(RcppArmadillo)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Script inputs________#
spp <- "BAIS" # BAIS or GRSP
Rcpp::sourceCpp('grass-surv-scripts/fwdalg.cpp')
source("grass-surv-scripts/CJS.hmm.cpp.mcmc.R")
n.mcmc=200000
save.out <- str_c("mod_mcmcR_test_", spp)
#______________________________#

# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- data.spp$Covs$firstDay
last <- data.spp$Covs$lastDay
last.alive <- apply(ymat,1,function(x) max(which(x==1)))
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
DOS2 <- DOS^2

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
  array(., c(length(.), nDOS))

X <- abind::abind(array(1, dim = dim(DOS)), DOS, DOS2, drone.z, drone2.z, droneCV.z, peso.z, along = 3)
tic()
out=CJS.hmm.cpp.mcmc(ymat,X,last.alive,n.mcmc,beta.tune=0.04,s.reg=1)
toc()
out$pD
out$DIC
matplot(t(out$beta.save),type="l")

#library(R.utils)
#saveObject(out, save.out)


