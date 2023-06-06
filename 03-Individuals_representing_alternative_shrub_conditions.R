library(tidyverse)
library(R.utils)

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")

## Functions ##
expit <- function(x) exp(x) / (1 + exp(x))

## Load data & models ##

load("Data_compiled_MissingCovsImputed.RData")
mod.nam <- "BigCheese"
mod.BAIS <- loadObject(str_c("mod_mcmcR_", mod.nam, "_BAIS"))
mod.GRSP <- loadObject(str_c("mod_mcmcR_", mod.nam, "_GRSP"))

## Apply models to predict survival rates at observed shrub conditions ##
# BAIS #
spp <- "GRSP" # BAIS or GRSP
coefs <- mod.BAIS$sims.concat[c("B.Intercept", "B.Shrub_All_5m",
                                "B.Max_Shrub_Height_5m", "B.Shrub_All_5m2",
                                "B.Max_Shrub_Height_5m2", "B.Shrub_All_5m_CV",
                                "B.Shrub_All_50m_CV", "B.Shrub_All_500m_CV",
                                "B.Max_Shrub_Height_5mXShrub_All_5m"),]
scripts.loc <- "grass-surv-scripts/"
chop.init <- 8 # Number of days to chop off after transmitter was first deployed (`NULL` if keeping all data).
source(str_c(scripts.loc, "Data_processing_", mod.nam, ".R"))
X <- X[,1,c("Shrub_All_5m", "Max_Shrub_Height_5m", "Shrub_All_5m2",
            "Max_Shrub_Height_5m2", "Shrub_All_5m_CV", "Shrub_All_50m_CV",
            "Shrub_All_500m_CV", "Max_Shrub_Height_5mXShrub_All_5m")]
ymat.ID$pred.md <- NA
ymat.ID$pred.lo <- NA
ymat.ID$pred.hi <- NA
for(i in 1:nrow(ymat.ID)) {
  S <- expit(coefs["B.Intercept", ] +
               coefs["B.Shrub_All_5m", ] * X[i, "Shrub_All_5m"] +
               coefs["B.Max_Shrub_Height_5m", ] * X[i, "Max_Shrub_Height_5m"] +
               coefs["B.Shrub_All_5m2", ] * X[i, "Shrub_All_5m2"] +
               coefs["B.Max_Shrub_Height_5m2", ] * X[i, "Max_Shrub_Height_5m2"] +
               coefs["B.Shrub_All_5m_CV", ] * X[i, "Shrub_All_5m_CV"] +
               coefs["B.Shrub_All_50m_CV", ] * X[i, "Shrub_All_50m_CV"] +
               coefs["B.Shrub_All_500m_CV", ] * X[i, "Shrub_All_500m_CV"] +
               coefs["B.Max_Shrub_Height_5mXShrub_All_5m", ] * X[i, "Max_Shrub_Height_5mXShrub_All_5m"])
  ymat.ID$pred.md[i] <- median(S)
  ymat.ID$pred.lo[i] <- quantile(S, prob = 0.025, type = 8)
  ymat.ID$pred.hi[i] <- quantile(S, prob = 0.975, type = 8)
}
ymat.ID.sort <- ymat.ID %>% arrange(pred.md)
ymat.ID.clip10 <- ymat.ID.sort %>% slice(c(1:10, (nrow(ymat.ID.sort)-10):nrow(ymat.ID.sort)))
write.csv(ymat.ID.clip10, str_c(spp, "_10_worst_and_best_shrubs.csv"), row.names = F)
ymat.ID.clip30 <- ymat.ID.sort %>% slice(c(1:30, (nrow(ymat.ID.sort)-30):nrow(ymat.ID.sort)))
write.csv(ymat.ID.clip30, str_c(spp, "_30_worst_and_best_shrubs.csv"), row.names = F)
