library(QSLpersonal)
library(R.utils)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")
spp <- "BAIS" # BAIS or GRSP

modR <- loadObject(str_c("mod_mcmcR_CJSRLtest_", spp))
modJAGS <- loadObject(str_c("mod_CJSRLHomog_test_", spp))
data.spp <- str_c("data.", spp) %>% as.name %>% eval

#X <- abind::abind(DOS, DOS2, drone.z, drone2.z, droneCV.z, peso.z, along = 3)
params <- c("B0", "p", "psi", "B_DOS", "B_DOS2",
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
tabp <- c("B0", "p", "psi")
JAGSp <- c("B0", "p", "psi")
for(p in 1:length(tabp)) out[tabp[p], "JAGS"] <-
  str_c(median(modJAGS$sims.list[[JAGSp[p]]]) %>% round(digits = 2), "(",
        quantile(modJAGS$sims.list[[JAGSp[p]]], probs = 0.025, type = 8) %>% round(digits = 2),",",
        quantile(modJAGS$sims.list[[JAGSp[p]]], probs = 0.975, type = 8) %>% round(digits = 2),")")

out["B_DOS", "JAGS"] <-
  str_c(median(modJAGS$sims.list$B[,1]) %>% round(digits = 2), "(",
        quantile(modJAGS$sims.list$B[,1], probs = 0.025, type = 8) %>% round(digits = 2),",",
        quantile(modJAGS$sims.list$B[,1], probs = 0.975, type = 8) %>% round(digits = 2),")")
out["B_DOS", "JAGSsupp"] <- ifelse(quantile(modJAGS$sims.list$B[,1], probs = 0.025, type = 8) > 0,
                                   "pos",
                                   ifelse(quantile(modJAGS$sims.list$B[,1], probs = 0.975, type = 8) < 0,
                                   "neg", "none"))

out["B_DOS2", "JAGS"] <-
  str_c(median(modJAGS$sims.list$B[,2]) %>% round(digits = 2), "(",
        quantile(modJAGS$sims.list$B[,2], probs = 0.025, type = 8) %>% round(digits = 2),",",
        quantile(modJAGS$sims.list$B[,2], probs = 0.975, type = 8) %>% round(digits = 2),")")
out["B_DOS2", "JAGSsupp"] <- ifelse(quantile(modJAGS$sims.list$B[,2], probs = 0.025, type = 8) > 0,
                                   "pos",
                                   ifelse(quantile(modJAGS$sims.list$B[,2], probs = 0.975, type = 8) < 0,
                                          "neg", "none"))

st <- which(params == "B_Mesquite_100m")
end <- which(params == "B_Distance_to_Fence")
md <- apply(modJAGS$sims.list$B[,3:((end-st)+3)], 2, median)
lo <- apply(modJAGS$sims.list$B[,3:((end-st)+3)], 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$B[,3:((end-st)+3)], 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")
out[st:end, "JAGSsupp"] <- ifelse(lo > 0, "pos", ifelse(hi < 0, "neg", "none"))

st <- which(params == "B2_Mesquite_100m")
end <- which(params == "B2_Distance_to_Fence")
md <- apply(modJAGS$sims.list$B[,16:((end-st)+16)], 2, median)
lo <- apply(modJAGS$sims.list$B[,16:((end-st)+16)], 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$B[,16:((end-st)+16)], 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")
out[st:end, "JAGSsupp"] <- ifelse(lo > 0, "pos", ifelse(hi < 0, "neg", "none"))

st <- which(params == "B_Mesquite_100m_CV")
end <- which(params == "B_Mean_Shrub_Height_100m_CV")
md <- apply(modJAGS$sims.list$B[,29:((end-st)+29)], 2, median)
lo <- apply(modJAGS$sims.list$B[,29:((end-st)+29)], 2, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modJAGS$sims.list$B[,29:((end-st)+29)], 2, function(x) quantile(x, probs = 0.975, type = 8))
out[st:end, "JAGS"] <- str_c(md %>% round(digits = 2), "(",
                             lo %>% round(digits = 2),",",
                             hi %>% round(digits = 2),")")
out[st:end, "JAGSsupp"] <- ifelse(lo > 0, "pos", ifelse(hi < 0, "neg", "none"))

out["B_peso", "JAGS"] <- str_c(median(modJAGS$sims.list$B[,41]) %>% round(digits = 2), "(",
                               quantile(modJAGS$sims.list$B[,41], prob = 0.025, type = 8) %>% round(digits = 2),",",
                               quantile(modJAGS$sims.list$B[,41], prob = 0.975, type = 8) %>% round(digits = 2),")")
out["B_peso", "JAGSsupp"] <- ifelse(quantile(modJAGS$sims.list$B[,41], prob = 0.025, type = 8) > 0, "pos",
                                    ifelse(quantile(modJAGS$sims.list$B[,41], prob = 0.975, type = 8) < 0, "neg", "none"))

# Summarize R model estimates #
md <- apply(modR$beta.save, 1, median)
lo <- apply(modR$beta.save, 1, function(x) quantile(x, probs = 0.025, type = 8))
hi <- apply(modR$beta.save, 1, function(x) quantile(x, probs = 0.975, type = 8))
out[-c(2:3), "R"] <- str_c(md %>% round(digits = 2), "(",
                    lo %>% round(digits = 2),",",
                    hi %>% round(digits = 2),")")
out[-c(2:3), "Rsupp"] <- ifelse(lo > 0, "pos", ifelse(hi < 0, "neg", "none"))

out["psi", "R"] <- str_c(median(modR$psi.save) %>% round(digits = 2), "(",
                         quantile(modR$psi.save, prob = 0.025, type = 8) %>% round(digits = 2),",",
                         quantile(modR$psi.save, prob = 0.975, type = 8) %>% round(digits = 2),")")
out["p", "R"] <- str_c(median(modR$p.save) %>% round(digits = 2), "(",
                         quantile(modR$p.save, prob = 0.025, type = 8) %>% round(digits = 2),",",
                         quantile(modR$p.save, prob = 0.975, type = 8) %>% round(digits = 2),")")

write.csv(out, "Compare_RvsJAGS_estimates.csv", row.names = T)
