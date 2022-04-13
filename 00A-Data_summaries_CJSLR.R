library(dplyr)
library(stringr)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")
load("Data_compiled.RData")

#######################################
# Summarize capture / monitoring data #
#######################################

cols <- c("No_indivs", "nDays", "nDets", "prpDaysDetWhenAlive", "nMort", "nMortPred", "nMortExp", "nMortDay1",
          "MortConfDayMed", "MortConfDay95thQntl", "nMortConfDayGT95th", "MortConfDayMax",
          "nSurv", "nMalf", "nUnk")
out <- matrix(NA, nrow = 2, ncol = length(cols),
              dimnames = list(c("BAIS", "GRSP"), cols))
chop.init <- 8 # Number of days to chop following transmitter deployment (NULL if none).

for(spp in dimnames(out)[[1]]) {
  data.spp <- eval(as.name(str_c("data.", spp)))
  ymat <- data.spp$ymat
  Covs <- data.spp$Covs %>%
    left_join(
      dat.trans %>%
        select(Site, Season, anillo, causa) %>%
        distinct() %>%
        dplyr::group_by(Site, Season, anillo) %>%
        summarise(Dep = sum(causa == "D"),
                  Exp = sum(causa == "F")),
      by = c("Site", "Season", "anillo")
    )
  first <- data.spp$Covs$firstDay
  last <- data.spp$Covs$lastDay
  last.alive <- data.spp$Covs$lastAlive
  #_____ Trim data array and adjust indexing._____#
  if(!is.null(chop.init)) {
    for(i in 1:nrow(ymat)) {
      # Chop off initial days as indicated by 'chop.init'
      last.chp <- min(last[i], first[i] + (chop.init - 1))
      ymat[i, first[i]:last.chp] <- NA
      if(!any(ymat[i,] == 1, na.rm = T)) ymat[i,] <- NA
      
      # Chop more days off up to the first remaining day when the individual was detected.
      if(any(!is.na(ymat[i,]))) {
        first[i] <- last.chp + 1
        while(is.na(ymat[i,first[i]])) first[i] <- first[i] + 1
        if(ymat[i, first[i]] != 1) {
          first[i] <- which(ymat[i,] == 1)[1]
          ymat[i,1:first[i]] <- NA
        }
      }
    }
    
    # Trim rows
    ind.rows.chop <- which(apply(ymat, 1, function(x) !any(x == 1, na.rm = T)))
    ymat <- ymat[-ind.rows.chop,]
    first <- first[-ind.rows.chop]
    last <- last[-ind.rows.chop]
    last.alive <- last.alive[-ind.rows.chop]
    Covs <- Covs %>% slice(-ind.rows.chop)
    
    # Trim columns
    ind.cols.chop <- which(apply(ymat, 2, function(x) !any(!is.na(x))))
    ind.cols.chop.keep <- c(1, which(ind.cols.chop[2:length(ind.cols.chop)] ==
                                       ind.cols.chop[1:(length(ind.cols.chop)-1)]+1)+1)
    ind.cols.chop <- ind.cols.chop[ind.cols.chop.keep]
    rm(ind.cols.chop.keep)
    ymat <- ymat[,-ind.cols.chop]
    first <- first - length(ind.cols.chop)
    last <- last - length(ind.cols.chop)
    last.alive <- last.alive - length(ind.cols.chop)
  }
  #_______________________________________________#
  out[spp, "No_indivs"] <- nrow(ymat)
  out[spp, "nDays"] <- sum(!is.na(ymat))
  out[spp, "nDets"] <- sum(ymat %in% c(1, 2), na.rm = T)
  if(is.null(chop.init)) {
    out[spp, "nDays"] <- out[spp, "nDays"] - nrow(ymat)
    out[spp, "nDets"] <- out[spp, "nDets"] - nrow(ymat)
  }
  out[spp, "prpDaysDetWhenAlive"] <- sum(ymat == 1, na.rm = T) /
    sum(last.alive - first)
  out[spp, "nMort"] <- sum(ymat == 2, na.rm = T)
  ydied <- ymat[which(apply(ymat, 1, function(x) any(x == 2, na.rm = T))),]
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
  out[spp, "nMortPred"] <- sum(Covs$Dep[which(apply(ymat, 1, function(x) any(x == 2, na.rm = T)))])
  out[spp, "nMortExp"] <- sum(Covs$Exp[which(apply(ymat, 1, function(x) any(x == 2, na.rm = T)))])
  out[spp, "MortConfDayMed"] <- median(DayConfDied)
  out[spp, "MortConfDay95thQntl"] <- quantile(DayConfDied, prob = 0.95, type = 8)
  out[spp, "nMortConfDayGT95th"] <- sum(DayConfDied > out[spp, "MortConfDay95thQntl"])
  out[spp, "MortConfDayMax"] <- max(DayConfDied)
  out[spp, "nSurv"] <- sum(Covs$resultado.final == "S")
  out[spp, "nMalf"] <- sum(Covs$resultado.final %in% c("RC"))
  out[spp, "nUnk"] <- sum(Covs$resultado.final %in% c("P", "U"))
  assign(str_c("ind.rows.chop.", spp), ind.rows.chop)
  assign(str_c("ind.cols.chop.", spp), ind.cols.chop)
  rm(ind.rows.chop, ind.cols.chop)
}
write.csv(out, "Summary.csv", row.names = T)

########################
# Summarize covariates #
########################

## Individual-level covariates ##

load("Data_compiled.RData")
imputed.BAIS <- data.BAIS$Covs
if(!is.null(chop.init)) imputed.BAIS <- imputed.BAIS %>% slice(-ind.rows.chop.BAIS)
imputed.BAIS <- imputed.BAIS %>%
select(hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
       Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
       Max_Shrub_Height_5m, peso, female, adult) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  data.matrix() %>%
  t()
imputed.BAIS <- data.frame(Covariate = row.names(imputed.BAIS),
                           Imputed = as.numeric(imputed.BAIS), stringsAsFactors = F)

imputed.GRSP <- data.GRSP$Covs
if(!is.null(chop.init)) imputed.GRSP <- imputed.GRSP %>% slice(-ind.rows.chop.GRSP)
imputed.GRSP <- imputed.GRSP %>%
  select(hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, peso, female, adult) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  data.matrix() %>%
  t()
imputed.GRSP <- data.frame(Covariate = row.names(imputed.GRSP),
                           Imputed = as.numeric(imputed.GRSP), stringsAsFactors = F)

load("Data_compiled_MissingCovsImputed.RData")
covs <- data.BAIS$Covs
if(!is.null(chop.init)) covs <- covs %>% slice(-ind.rows.chop.BAIS)
covs <- covs %>%
  select(hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, peso, female, adult) %>%
  mutate(hierbas_cv = hierbas_cv * 100,
         pastos_cv = pastos_cv * 100,
         otra_cv = otra_cv * 100,
         Shrub_All_5m = Shrub_All_5m * 100,
         Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
         Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
         Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
         Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100)
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

covs <- data.GRSP$Covs
if(!is.null(chop.init)) covs <- covs %>% slice(-ind.rows.chop.GRSP)
covs <- covs %>%
  select(hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
         Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m, peso, female, adult) %>%
  mutate(hierbas_cv = hierbas_cv * 100,
         pastos_cv = pastos_cv * 100,
         otra_cv = otra_cv * 100,
         Shrub_All_5m = Shrub_All_5m * 100,
         Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
         Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
         Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
         Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100)
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
##*** Only works now if chop.init is defined ***##
load("Data_compiled_MissingCovsImputed.RData")
dat.site %>%
  select(Site, Season, LOSH, prey, NDVI) %>%
  mutate(LOSH = round(LOSH, digits = 2) %>% as.character(),
         prey = round(prey, digits = 2) %>% as.character(),
         NDVI = round(NDVI, digits = 2) %>% as.character()) %>%
  left_join(
    data.BAIS$Covs %>% slice(-ind.rows.chop.BAIS) %>%
      select(Site, Season, hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
             Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
             Max_Shrub_Height_5m, peso, female, adult) %>%
      mutate(hierbas_cv = hierbas_cv * 100,
             pastos_cv = pastos_cv * 100,
             otra_cv = otra_cv * 100,
             Shrub_All_5m = Shrub_All_5m * 100,
             Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
             Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
             Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
             Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100) %>%
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
    data.GRSP$Covs %>% slice(-ind.rows.chop.BAIS) %>%
      select(Site, Season, hierbas, hierbas_cv, hierba_ht, pastos_cv, pasto_ht, salsola, otra, otra_cv,
             Shrub_All_5m, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
             Max_Shrub_Height_5m, peso, female, adult) %>%
      mutate(hierbas_cv = hierbas_cv * 100,
             pastos_cv = pastos_cv * 100,
             otra_cv = otra_cv * 100,
             Shrub_All_5m = Shrub_All_5m * 100,
             Shrub_All_5m_CV = Shrub_All_5m_CV * 100,
             Shrub_All_50m_CV = Shrub_All_50m_CV * 100,
             Shrub_All_500m_CV = Shrub_All_500m_CV * 100,
             Max_Shrub_Height_5m = Max_Shrub_Height_5m * 100) %>%
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
