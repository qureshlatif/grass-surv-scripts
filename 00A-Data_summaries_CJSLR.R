library(dplyr)
library(stringr)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#######################################
# Summarize capture / monitoring data #
#######################################

cols <- c("No_indivs", "nDays", "nDets", "prpDaysDetWhenAlive", "nMort", "nMortDay1",
          "MortConfDayMed", "MortConfDay95thQntl", "nMortConfDayGT95th", "MortConfDayMax",
          "nSurv", "nMalf", "nUnk")
out <- matrix(NA, nrow = 2, ncol = length(cols),
              dimnames = list(c("BAIS", "GRSP"), cols))

for(spp in dimnames(out)[[1]]) {
  data.spp <- eval(as.name(str_c("data.", spp)))
  out[spp, "No_indivs"] <- nrow(data.spp$ymat)
  out[spp, "nDays"] <- sum(!is.na(data.spp$ymat)) - nrow(data.spp$ymat)
  out[spp, "nDets"] <- sum(data.spp$ymat %in% c(1, 2), na.rm = T) - nrow(data.spp$ymat)
  out[spp, "prpDaysDetWhenAlive"] <- sum(data.spp$ymat == 1, na.rm = T) /
    sum(data.spp$Covs$lastAlive - data.spp$Covs$firstDay)
  out[spp, "nMort"] <- sum(data.spp$ymat == 2, na.rm = T)
  ydied <- data.spp$ymat[which(apply(data.spp$ymat, 1, function(x) any(x == 2, na.rm = T))),]
  out[spp, "nMortDay1"] <- sum(apply(ydied, 1, function(x) {
    ind.died <- which(x == 2)
    ind.pre <- max(which(x %in% c(0,1)))
    return(x[ind.pre] == 1)
  }))
  DayConfDied <- apply(ydied, 1, function(x) {
    ind.died <- which(x == 2)
    ind.lastalive <- max(which(x == 1))
    return(ind.died - ind.lastalive)
    })
  out[spp, "MortConfDayMed"] <- median(DayConfDied)
  out[spp, "MortConfDay95thQntl"] <- quantile(DayConfDied, prob = 0.95, type = 8)
  out[spp, "nMortConfDayGT95th"] <- sum(DayConfDied > out[spp, "MortConfDay95thQntl"])
  out[spp, "MortConfDayMax"] <- max(DayConfDied)
  out[spp, "nSurv"] <- sum(data.spp$Covs$resultado.final == "S")
  out[spp, "nMalf"] <- sum(data.spp$Covs$resultado.final %in% c("RC", "P"))
  out[spp, "nUnk"] <- sum(data.spp$Covs$resultado.final %in% c("Q", "U"))
}
write.csv(out, "Summary.csv", row.names = T)

########################
# Summarize covariates #
########################

## Individual-level covariates ##

load("Data_compiled.RData")
imputed.BAIS <- data.BAIS$Covs %>%
  select(hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
         Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
         Distance_to_Fence, peso, female, adult) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  data.matrix() %>%
  t()
imputed.BAIS <- data.frame(Covariate = row.names(imputed.BAIS),
                           Imputed = as.numeric(imputed.BAIS), stringsAsFactors = F)

imputed.GRSP <- data.GRSP$Covs %>%
  select(hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
         Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
         Distance_to_Fence, peso, female) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  data.matrix() %>%
  t()
imputed.GRSP <- data.frame(Covariate = row.names(imputed.GRSP),
                           Imputed = as.numeric(imputed.GRSP), stringsAsFactors = F)

load("Data_compiled_MissingCovsImputed.RData")
covs <- data.BAIS$Covs %>%
  select(hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
         Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
         Distance_to_Fence, peso, female, adult) %>%
  mutate(hierbas_cv = hierbas_cv * 100,
         pasto_ht_cv = pasto_ht_cv * 100,
         otra_cv = otra_cv * 100,
         Shrub_All_5m = Shrub_All_5m * 100,
         Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
         Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
         Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
         Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100,
         Max_Shrub_Height_50m_CV = Max_Shrub_Height_50m_CV * 100,
         Max_Shrub_Height_500m_CV = Max_Shrub_Height_500m_CV * 100,
         Juniper_5m = Juniper_5m * 100,
         Juniper_500m = Juniper_500m * 100,
         Yucca_5m = Yucca_5m * 100,
         Yucca_500m = Yucca_500m * 100,
         Mesquite_5m = Mesquite_5m * 100)
sumTab <- data.frame(Covariate = names(covs), stringsAsFactors = F) %>%
  mutate(mean = apply(covs %>% data.matrix, 2, mean, na.rm = T) %>% round(digits = 2),
         sd = apply(covs %>% data.matrix, 2, sd, na.rm = T) %>% round(digits = 2),
         median = apply(covs %>% data.matrix, 2, median, na.rm = T) %>% round(digits = 2),
         q05 = apply(covs %>% data.matrix, 2, function(x) quantile(x, prob = 0.05, type = 8, na.rm = T)) %>% round(digits = 2),
         q95 = apply(covs %>% data.matrix, 2, function(x) quantile(x, prob = 0.95, type = 8, na.rm = T)) %>% round(digits = 2),
         range = str_c(apply(covs %>% data.matrix, 2, min, na.rm = T) %>% round(digits = 2),
                       " - ",
                       apply(covs %>% data.matrix, 2, max, na.rm = T) %>% round(digits = 2))) %>%
  left_join(imputed.BAIS, by = "Covariate")
write.csv(sumTab, "Covariate_summaries_individual_BAIS.csv", row.names = F)

covs <- data.GRSP$Covs %>%
  select(hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
         Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
         Distance_to_Fence, peso, female) %>%
  mutate(hierbas_cv = hierbas_cv * 100,
         pasto_ht_cv = pasto_ht_cv * 100,
         otra_cv = otra_cv * 100,
         Shrub_All_5m = Shrub_All_5m * 100,
         Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
         Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
         Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
         Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100,
         Max_Shrub_Height_50m_CV = Max_Shrub_Height_50m_CV * 100,
         Max_Shrub_Height_500m_CV = Max_Shrub_Height_500m_CV * 100,
         Juniper_5m = Juniper_5m * 100,
         Juniper_500m = Juniper_500m * 100,
         Yucca_5m = Yucca_5m * 100,
         Yucca_500m = Yucca_500m * 100,
         Mesquite_5m = Mesquite_5m * 100)
sumTab <- data.frame(Covariate = names(covs), stringsAsFactors = F) %>%
  mutate(mean = apply(covs %>% data.matrix, 2, mean, na.rm = T) %>% round(digits = 2),
         sd = apply(covs %>% data.matrix, 2, sd, na.rm = T) %>% round(digits = 2),
         median = apply(covs %>% data.matrix, 2, median, na.rm = T) %>% round(digits = 2),
         q05 = apply(covs %>% data.matrix, 2, function(x) quantile(x, prob = 0.05, type = 8, na.rm = T)) %>% round(digits = 2),
         q95 = apply(covs %>% data.matrix, 2, function(x) quantile(x, prob = 0.95, type = 8, na.rm = T)) %>% round(digits = 2),
         range = str_c(apply(covs %>% data.matrix, 2, min, na.rm = T) %>% round(digits = 2),
                       " - ",
                       apply(covs %>% data.matrix, 2, max, na.rm = T) %>% round(digits = 2))) %>%
  left_join(imputed.GRSP, by = "Covariate")
write.csv(sumTab, "Covariate_summaries_individual_GRSP.csv", row.names = F)

## Site-level covariate summaries ##
load("Data_compiled_MissingCovsImputed.RData")
dat.site %>%
  select(Site, Season, LOSH, prey, NDVI) %>%
  mutate(LOSH = round(LOSH, digits = 2) %>% as.character(),
         prey = round(prey, digits = 2) %>% as.character(),
         NDVI = round(NDVI, digits = 2) %>% as.character()) %>%
  left_join(
    data.BAIS$Covs %>%
      select(Site, Season, hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
             Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
             Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
             Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
             Distance_to_Fence, peso, female) %>%
      mutate(hierbas_cv = hierbas_cv * 100,
             pasto_ht_cv = pasto_ht_cv * 100,
             otra_cv = otra_cv * 100,
             Shrub_All_5m = Shrub_All_5m * 100,
             Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
             Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
             Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
             Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100,
             Max_Shrub_Height_50m_CV = Max_Shrub_Height_50m_CV * 100,
             Max_Shrub_Height_500m_CV = Max_Shrub_Height_500m_CV * 100,
             Juniper_5m = Juniper_5m * 100,
             Juniper_500m = Juniper_500m * 100,
             Yucca_5m = Yucca_5m * 100,
             Yucca_500m = Yucca_500m * 100,
             Mesquite_5m = Mesquite_5m * 100) %>%
      group_by(Site, Season) %>%
      summarise_all(function(x) {
        str_c(
          mean(x, na.rm = T) %>% round(digits = 2),
          ",",
          median(x, na.rm = T) %>% round(digits = 2),
          "(",
          quantile(x, prob = 0.05, type = 8, na.rm = T) %>% round(digits = 2),
          ",",
          quantile(x, prob = 0.95, type = 8, na.rm = T) %>% round(digits = 2),
          ")"
        )
      }),
    by = c("Site", "Season")
  ) %>%
  arrange(Site, Season) %>%
  as.matrix() %>%
  t() %>%
  write.csv("Covariate_summaries_site_BAIS.csv")

dat.site %>%
  select(Site, Season, LOSH, prey, NDVI) %>%
  mutate(LOSH = round(LOSH, digits = 2) %>% as.character(),
         prey = round(prey, digits = 2) %>% as.character(),
         NDVI = round(NDVI, digits = 2) %>% as.character()) %>%
  left_join(
    data.GRSP$Covs %>%
      select(Site, Season, hierbas, hierbas_cv, pastos, pasto_ht_cv, salsola, otra, otra_cv,
             Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
             Max_Shrub_Height_5m, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
             Juniper_5m, Juniper_500m, Yucca_5m, Yucca_500m, Mesquite_5m,
             Distance_to_Fence, peso, female) %>%
      mutate(hierbas_cv = hierbas_cv * 100,
             pasto_ht_cv = pasto_ht_cv * 100,
             otra_cv = otra_cv * 100,
             Shrub_All_5m = Shrub_All_5m * 100,
             Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
             Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
             Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
             Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100,
             Max_Shrub_Height_50m_CV = Max_Shrub_Height_50m_CV * 100,
             Max_Shrub_Height_500m_CV = Max_Shrub_Height_500m_CV * 100,
             Juniper_5m = Juniper_5m * 100,
             Juniper_500m = Juniper_500m * 100,
             Yucca_5m = Yucca_5m * 100,
             Yucca_500m = Yucca_500m * 100,
             Mesquite_5m = Mesquite_5m * 100) %>%
      group_by(Site, Season) %>%
      summarise_all(function(x) {
        str_c(
          mean(x, na.rm = T) %>% round(digits = 2),
          ",",
          median(x, na.rm = T) %>% round(digits = 2),
          "(",
          quantile(x, prob = 0.05, type = 8, na.rm = T) %>% round(digits = 2),
          ",",
          quantile(x, prob = 0.95, type = 8, na.rm = T) %>% round(digits = 2),
          ")"
        )
      }),
    by = c("Site", "Season")
  ) %>%
  arrange(Site, Season) %>%
  as.matrix() %>%
  t() %>%
  write.csv("Covariate_summaries_site_GRSP.csv")
