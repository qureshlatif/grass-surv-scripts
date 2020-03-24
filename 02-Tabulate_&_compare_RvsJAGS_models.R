library(QSLpersonal)
library(R.utils)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")
spp <- "BAIS" # BAIS or GRSP

modR <- loadObject(str_c("mod_mcmcR_test_", spp))
modJAGS <- loadObject(str_c("mod_regularization_test_", spp))
data.spp <- str_c("data.", spp) %>% as.name %>% eval

#X <- abind::abind(DOS, DOS2, drone.z, drone2.z, droneCV.z, peso.z, along = 3)
params <- c("B0", "B_DOS", "B_DOS2",
            data.spp$Covs %>% ungroup() %>%
              select(Mesquite_5m:Distance_to_Fence) %>%
              select(contains("_100m"), Distance_to_Fence) %>%
              names %>%
              str_c("B_",.),
            data.spp$Covs %>% ungroup() %>%
              select(Mesquite_5m:Distance_to_Fence) %>%
              select(contains("_100m"), Distance_to_Fence) %>%
              names %>%
              str_c("B2_",.),
            data.spp$Covs %>%
              select(Mesquite_5m_CV:Mean_Shrub_Height_500m_CV) %>%
              select(contains("_100m")) %>%
              names %>%
              str_c("B_",.),
            "B_peso")
cols <- c("R", "Rsupp", "JAGS", "JAGSsupp")
out <- matrix("", nrow = length(params), ncol = length(cols),
              dimnames = list(params, cols))

# Summarize JAGS model estimates #
tabp <- c("B0", "B_DOS", "B_DOS2", "B_peso")
JAGSp <- c("B0", "B.DOS", "B.DOS2", "B.peso")
for(p in 1:length(tabp)) out[tabp[p], "JAGS"] <-
  str_c(median(modJAGS$sims.list[[JAGSp[p]]]) %>% round(digits = 2), "(",
        quantile(modJAGS$sims.list[[JAGSp[p]]], probs = 0.025, type = 8) %>% round(digits = 2),",",
        quantile(modJAGS$sims.list[[JAGSp[p]]], probs = 0.975, type = 8) %>% round(digits = 2),")")

st <- which(params == "B_Mesquite_100m")
end <- which(params == "B_Distance_to_Fence")
md <- apply(modJAGS$sims.list$B.drone, 2, median)
lo <- apply(modJAGS$sims.list$B.drone, 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$B.drone, 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")

st <- which(params == "B2_Mesquite_100m")
end <- which(params == "B2_Distance_to_Fence")
md <- apply(modJAGS$sims.list$B2.drone, 2, median)
lo <- apply(modJAGS$sims.list$B2.drone, 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$B2.drone, 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")

st <- which(params == "B_Mesquite_100m_CV")
end <- which(params == "B_Mean_Shrub_Height_100m_CV")
md <- apply(modJAGS$sims.list$BCV.drone, 2, median)
lo <- apply(modJAGS$sims.list$BCV.drone, 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$BCV.drone, 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")

# Summarize R model estimates #
md <- apply(modR$beta.save, 1, median)
lo <- apply(modR$beta.save, 1, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modR$beta.save, 1, function(x) quantile(x, probs = 0.975, type = 8))
out[, "R"] <- str_c(md %>% round(digits = 2), "(",
                    lo %>% round(digits = 2),",",
                    hi %>% round(digits = 2),")")

