library(tidyverse)
library(QSLpersonal)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")

dat.drn2018 <- read.csv("Drone_individual_nonShrub2018.csv", stringsAsFactors = F)

## BAIS ##
ind.keep <- which(data.BAIS$Covs$Season == "2017-2018")
data.BAIS$ymat <- data.BAIS$ymat[ind.keep,]
data.BAIS$Covs <- data.BAIS$Covs %>% slice(ind.keep)
surv.days <- which(apply(data.BAIS$ymat, 2, function(x) any(!is.na(x))))
st <- surv.days[1]
end <- surv.days[length(surv.days)]
data.BAIS$ymat <- data.BAIS$ymat[, st:end]
rm(ind.keep, st, end, surv.days)

data.BAIS$Covs <- data.BAIS$Covs %>%
  left_join(
    dat.drn2018 %>%
      select(Site, anillo, Tumbleweed_5m:Hilaria_500m_n),
    by = c("Site", "anillo")
    )

## GRSP ##
ind.keep <- which(data.GRSP$Covs$Season == "2017-2018")
data.GRSP$ymat <- data.GRSP$ymat[ind.keep,]
data.GRSP$Covs <- data.GRSP$Covs %>% slice(ind.keep)
surv.days <- which(apply(data.GRSP$ymat, 2, function(x) any(!is.na(x))))
st <- surv.days[1]
end <- surv.days[length(surv.days)]
data.GRSP$ymat <- data.GRSP$ymat[, st:end]
rm(ind.keep, st, end, surv.days)

data.GRSP$Covs <- data.GRSP$Covs %>%
  left_join(
    dat.drn2018 %>%
      select(Site, anillo, Tumbleweed_5m:Hilaria_500m_n),
    by = c("Site", "anillo")
  )

save.image("Data_compiled_2018.RData")

#####################################
# Explore and tabulate correlations #
#####################################
library(corrplot)

## Baird's Sparrow ##
data.BAIS$Covs %>%
  select(Shrub_All_5m, Mean_Shrub_Height_5m, Juniper_5m, Mesquite_5m, Yucca_5m,
         Shrub_All_5m_CV, Shrub_All_50m_CV, Mean_Shrub_Height_50m_CV, Mean_Shrub_Height_500m_CV,
         Tumbleweed_5m:Hilaria_500m,
         Bare_Ground_5m_CV:Other_Grass_500m_CV,
         Grass_5m_CV:Hilaria_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  write.csv("Correlations_2018_BAIS.csv")

pdf("Correlations_2018_BAIS.pdf")
data.BAIS$Covs %>%
  select(Shrub_All_5m, Mean_Shrub_Height_5m, Juniper_5m, Mesquite_5m, Yucca_5m,
         Shrub_All_5m_CV, Shrub_All_50m_CV, Mean_Shrub_Height_50m_CV, Mean_Shrub_Height_500m_CV,
         Tumbleweed_5m:Hilaria_500m,
         Bare_Ground_5m_CV:Other_Grass_500m_CV,
         Grass_5m_CV:Hilaria_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()

## Grasshopper Sparrow ##
data.GRSP$Covs %>%
  select(Shrub_All_5m, Mean_Shrub_Height_5m, Juniper_5m, Mesquite_5m, Yucca_5m,
         Shrub_All_5m_CV, Shrub_All_50m_CV, Mean_Shrub_Height_50m_CV, Mean_Shrub_Height_500m_CV,
         Tumbleweed_5m:Hilaria_500m,
         Bare_Ground_5m_CV:Other_Grass_500m_CV,
         Grass_5m_CV:Hilaria_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  write.csv("Correlations_2018_GRSP.csv")

pdf("Correlations_2018_GRSP.pdf")
data.GRSP$Covs %>%
  select(Shrub_All_5m, Mean_Shrub_Height_5m, Juniper_5m, Mesquite_5m, Yucca_5m,
         Shrub_All_5m_CV, Shrub_All_50m_CV, Mean_Shrub_Height_50m_CV, Mean_Shrub_Height_500m_CV,
         Tumbleweed_5m:Hilaria_500m,
         Bare_Ground_5m_CV:Other_Grass_500m_CV,
         Grass_5m_CV:Hilaria_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()
