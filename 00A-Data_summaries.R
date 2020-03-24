library(dplyr)
library(stringr)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

minDOS <- 317 # Sets first day of season - currently corresponds with 11/12/2012

dat.banding <- read.csv("wintersurvival_accessDBs/ExportedTables/Anillamiento_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday) %>%
  filter(captura != 6) %>%
  arrange(Site, Season, anillo, DOY) %>%
  distinct(Site, Season, anillo, DOY, .keep_all = T) # Not sure if I need this. Inherited from data compilation script, but added in DOY because needed for data summaries of mass change.
dat.locations <- read.csv("wintersurvival_accessDBs/ExportedTables/Locaciones_Todos_all_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday)
dat.trans <- read.csv("wintersurvival_accessDBs/ExportedTables/Transmisores_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOYdepl = fecha_depl %>% mdy %>% yday)
dat.banding <- dat.banding %>%
  mutate(Tagged = anillo %in% dat.trans$anillo)
dat.veg <- read.csv("Veg_individual.csv", header = T, stringsAsFactors = F)

# Convert all day of years to day of season #
dat.banding$DOS <- dat.banding$DOY
ind.beforeD31 <- which(dat.banding$DOY > 200)
ind.afterD31 <- which(dat.banding$DOY < 200)
dat.banding$DOS[ind.beforeD31] <- dat.banding$DOS[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.banding$fecha %>% str_sub(-4, -1) %>% str_c("12/31/", .) %>% mdy %>% yday)  - (minDOS - 1)
dat.banding$DOS[ind.afterD31] <- dat.banding$DOS[ind.afterD31] + D31[ind.afterD31]

dat.locations$DOS <- dat.locations$DOY
ind.beforeD31 <- which(dat.locations$DOY > 200)
ind.afterD31 <- which(dat.locations$DOY < 200)
dat.locations$DOS[ind.beforeD31] <- dat.locations$DOS[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.locations$fecha %>% str_sub(-4, -1) %>% str_c("12/31/", .) %>% mdy %>% yday)  - (minDOS - 1)
dat.locations$DOS[ind.afterD31] <- dat.locations$DOS[ind.afterD31] + D31[ind.afterD31]

dat.trans$DOSdepl <- dat.trans$DOYdepl
ind.beforeD31 <- which(dat.trans$DOYdepl > 200)
ind.afterD31 <- which(dat.trans$DOYdepl < 200)
dat.trans$DOSdepl[ind.beforeD31] <- dat.trans$DOSdepl[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.trans$fecha_depl %>% str_sub(-4, -1) %>% str_c("12/31/", .) %>% mdy %>% yday)  - (minDOS - 1)
dat.trans$DOSdepl[ind.afterD31] <- dat.trans$DOSdepl[ind.afterD31] + D31[ind.afterD31]
rm(D31)

# Get tagging statuses into banding data table.
dat.banding <- dat.banding %>%
  left_join( # If transmitter replaced, uses first time transmitter was deployed for date of deployment.
    dat.trans %>%
      select(Site, Season, anillo, DOSdepl) %>%
      group_by(Site, Season, anillo) %>%
      summarise(DOSdepl = min(DOSdepl)),
    by = c("Site", "Season", "anillo")) %>%
  mutate(Tagged = !is.na(DOSdepl))

# Temp corrections #
dat.locations <- dat.locations %>%
  filter(!(anillo == 111111312 & Site == "VACO" & Season == "2018-2019" & DOS > 89)) # Remove extraneous mortality records following the initial observation.

# Get all detection histories by species #
species <- unique(dat.locations$especie)
for(sp in species) {
  dlocs <- dat.locations %>% filter(especie == sp) %>%
    select(anillo, Site, Season, DOS, survive) %>%
    #filter(survive != 9) %>%
    mutate(Dataset = "Locations") %>% unique
  dbands <- dat.banding %>% filter(especie == sp) %>%
    select(anillo, Site, Season, DOS) %>%
    mutate(survive = 1, Dataset = "Banding") %>% unique
  dloc.detections <- str_c(dlocs$anillo, "_", dlocs$DOS, "_", dlocs$Season)
  dbands <- dbands %>%
    mutate(detections = str_c(anillo, "_", DOS, "_", Season)) %>%
    filter(!detections %in% dloc.detections) %>%
    select(-detections)
  detections <- dlocs %>% bind_rows(dbands)
  assign(str_c("detections.", sp), detections)
}
rm(dlocs, dbands, dloc.detections, detections)

# Plot Day of season by mass #
# spp <- "BAIS"
# plot(dat.banding %>% filter(especie == spp) %>% filter(DOS != -999 & peso != -999) %>% pull(DOS),
#      dat.banding %>% filter(especie == spp) %>% filter(DOS != -999 & peso != -999)  %>% pull(peso))
# cor(dat.banding %>% filter(especie == spp) %>% filter(DOS != -999 & peso != -999) %>% pull(DOS),
#     dat.banding %>% filter(especie == spp) %>% filter(DOS != -999 & peso != -999)  %>% pull(peso))

# Plot Time of day by mass #
# spp <- "GRSP"
# dat.banding <- dat.banding %>%
#   mutate(hora_dec = (str_sub(hora, 1, 2) %>% as.numeric) +
#            (str_sub(hora, -2, -1) %>% as.numeric) / 60)
# dat.banding$hora_dec[which(dat.banding$hora == -9999)] <- -999
# plot(dat.banding %>% filter(especie == spp) %>% filter(hora_dec != -999 & peso != -999) %>% pull(hora_dec),
#      dat.banding %>% filter(especie == spp) %>% filter(hora_dec != -999 & peso != -999)  %>% pull(peso))
# cor(dat.banding %>% filter(especie == spp) %>% filter(hora_dec != -999 & peso != -999) %>% pull(hora_dec),
#     dat.banding %>% filter(especie == spp) %>% filter(hora_dec != -999 & peso != -999)  %>% pull(peso))

# Data summaries #
cols <- c("No_indivs", "No_Rtagged",  "No_ind_monitored", "Recap_indivis", "nDays", "prpDaysDet", "nMort",
          "Mean_Days_radiotracked", "Min_Days_radiotracked", "Max_Days_radiotracked",
          "n_PercMassChange_Tagged", "Mean_PercMassChange_Tagged", "Min_PercMassChange_Tagged", "Max_PercMassChange_Tagged",
          "n_PercMassChange_Untagged", "Mean_PercMassChange_Untagged", "Min_PercMassChange_Untagged", "Max_PercMassChange_Untagged")
out <- matrix(NA, nrow = length(species), ncol = length(cols),
              dimnames = list(species, cols))

for(spp in species) {
  out[spp, "No_indivs"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>% filter(survive != 9) %>%
    pull(anillo) %>% unique %>% length
  out[spp, "No_Rtagged"] <- dat.banding %>%
    filter(especie == spp & Tagged) %>%
    pull(anillo) %>% unique %>% length
  out[spp, "Recap_indivis"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>% filter(survive != 9) %>%
    select(anillo, Season) %>% unique %>%
    pull(anillo) %>% tapply(., ., length) %>%
    (function(x) sum(x > 1))
  daysMonitored <- str_c("detections.", spp) %>%
    as.name %>% eval %>% filter(survive != 9) %>%
    dplyr::group_by(anillo, Season) %>%
    summarise(minDOS = min(DOS), maxDOS = max(DOS)) %>%
    mutate(days = maxDOS - minDOS) %>%
    ungroup %>%
    dplyr::group_by(anillo) %>%
    summarize(days = sum(days)) %>%
    pull(days)
  out[spp, "nDays"] <- sum(daysMonitored)
  out[spp, "nMort"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>% filter(survive != 9) %>% pull(survive) %>% (function(x) sum(x == 0))
  out[spp, "No_ind_monitored"] <- sum(daysMonitored > 0)
  daysMissed <- str_c("detections.", spp) %>%
    as.name %>% eval %>% filter(survive == 1 & Dataset == "Locations") %>%
    dplyr::group_by(Season, anillo) %>%
    summarise(minDOS = min(DOS), maxDOS = max(DOS), nObs = n()) %>%
    mutate(nDays = maxDOS - (minDOS - 1)) %>%
    ungroup %>% select(nObs, nDays)
  out[spp, "prpDaysDet"] <- sum(daysMissed$nObs) / sum(daysMissed$nDays)
  out[spp, "Mean_Days_radiotracked"] <- mean(daysMonitored[which(daysMonitored > 0)])
  out[spp, "Min_Days_radiotracked"] <- min(daysMonitored[which(daysMonitored > 0)])
  out[spp, "Max_Days_radiotracked"] <- max(daysMonitored[which(daysMonitored > 0)])
  massChange <- dat.banding %>%
    filter(especie == spp & Tagged & peso != -999) %>%
    arrange(Season, anillo, DOS) %>%
    dplyr::group_by(Season, anillo) %>%
    mutate(firstMass = first(peso), lastMass = last(peso)) %>%
    summarise(ncapt = n(), firstMass = first(firstMass), lastMass = first(lastMass)) %>%
    filter(ncapt > 1) %>%
    #mutate(deltaMass = lastMass - firstMass) %>% pull(deltaMass)
    mutate(deltaMass = ((lastMass - firstMass) / firstMass) * 100) %>% pull(deltaMass)
  out[spp, "n_PercMassChange_Tagged"] <- length(massChange)
  out[spp, "Mean_PercMassChange_Tagged"] <- mean(massChange)
  out[spp, "Min_PercMassChange_Tagged"] <- min(massChange)
  out[spp, "Max_PercMassChange_Tagged"] <- max(massChange)
  massChange <- dat.banding %>%
    filter(especie == spp & !Tagged & peso != -999) %>%
    arrange(Season, anillo, DOS) %>%
    dplyr::group_by(Season, anillo) %>%
    mutate(firstMass = first(peso), lastMass = last(peso)) %>%
    summarise(ncapt = n(), firstMass = first(firstMass), lastMass = first(lastMass)) %>%
    filter(ncapt > 1) %>%
    mutate(deltaMass = ((lastMass - firstMass) / firstMass) * 100) %>% pull(deltaMass)
  out[spp, "n_PercMassChange_Untagged"] <- length(massChange)
  out[spp, "Mean_PercMassChange_Untagged"] <- mean(massChange)
  out[spp, "Min_PercMassChange_Untagged"] <- min(massChange)
  out[spp, "Max_PercMassChange_Untagged"] <- max(massChange)
}
#write.csv(out, "Summary.csv", row.names = T)

# Summarize mass by site #
sites <- unique(dat.banding$Site)
nrows <- length(species) * length(sites)
out <- data.frame(Species = character(length = nrows),
                  Site = character(length = nrows),
                  Mean = numeric(length = nrows),
                  SD = numeric(length = nrows), stringsAsFactors = F)
counter <- 1
for(spp in species) for(sit in sites) {
  mass <- dat.banding %>% filter(especie == spp & Site == sit & peso != -999) %>%
    pull(peso)
  out$Species[counter] <- spp
  out$Site[counter] <- sit
  if(length(mass) > 0) out$Mean[counter] <- mean(mass)
  if(length(mass) > 1) out$SD[counter] <- sd(mass)
  counter <- counter + 1
}
#write.csv(out, "Summary_mass_by_site.csv", row.names = T)

# Summarize gaps in detection histories to get a sense of imperfect detection #
out <- dat.locations %>%
  select(Site, Season) %>% distinct %>%
  mutate(prpGaps.alive.BAIS = 0,
         prpGaps.alive.BAIS.n = 0,
         prpGaps.alive.GRSP = 0,
         prpGaps.alive.GRSP.n = 0,
         nMort.BAIS = 0,
         nMortWithGaps.BAIS = 0,
         SumDaysMortGaps.BAIS = 0,
         MedDaysMortGaps.BAIS = 0,
         nMort.GRSP = 0,
         nMortWithGaps.GRSP = 0,
         SumDaysMortGaps.GRSP = 0,
         MedDaysMortGaps.GRSP = 0)
survey.days <- vector("list", nrow(out))
for(i in 1:nrow(out)) survey.days[[i]] <- dat.locations %>%
  filter(Site == out$Site[i] & Season == out$Season[i]) %>%
  pull(DOS) %>% unique %>% sort

for(i in 1:nrow(out)) {
  obs <- detections.BAIS %>%
    filter(survive == 1 &
             Dataset == "Locations" &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    dplyr::group_by(anillo) %>%
    summarise(minDOS = min(DOS), maxDOS = max(DOS), nObs = n()) %>%
    mutate(nDays = 0) %>% ungroup
  for(j in 1:nrow(obs)) obs$nDays[j] <- sum(survey.days[[i]] >= obs$minDOS[j] &
                                              survey.days[[i]] <= obs$maxDOS[j])
  out$prpGaps.alive.BAIS[i] <- mean(obs$nObs / obs$nDays)
  out$prpGaps.alive.BAIS.n[i] <- sum(obs$nDays)
  
  obs <- detections.GRSP %>%
    filter(survive == 1 &
             Dataset == "Locations" &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    dplyr::group_by(anillo) %>%
    summarise(minDOS = min(DOS), maxDOS = max(DOS), nObs = n()) %>%
    mutate(nDays = 0) %>% ungroup
  for(j in 1:nrow(obs)) obs$nDays[j] <- sum(survey.days[[i]] >= obs$minDOS[j] &
                                              survey.days[[i]] <= obs$maxDOS[j])
  out$prpGaps.alive.GRSP[i] <- mean(obs$nObs / obs$nDays)
  out$prpGaps.alive.GRSP.n[i] <- sum(obs$nDays)
  
  obs.mort <- detections.BAIS %>%
    filter(survive == 0 &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    arrange(anillo)
  out$nMort.BAIS[i] <- nrow(obs.mort)
  obs.premort <- detections.BAIS %>%
    filter(survive == 1 &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    group_by(anillo) %>%
    summarise(penultDOS = max(DOS)) %>%
    ungroup %>% filter(anillo %in% obs.mort$anillo) %>%
    arrange(anillo)
  gaps <- obs.mort %>%
      left_join(obs.premort, by = "anillo") %>%
      mutate(diff = DOS - penultDOS) %>%
      pull(diff)
  out$nMortWithGaps.BAIS[i] <- sum(gaps > 1)
  out$SumDaysMortGaps.BAIS[i] <- sum(gaps)
  out$MedDaysMortGaps.BAIS[i] <- ifelse(any(gaps > 1), median(gaps[which(gaps > 1)]), 0)
  
  obs.mort <- detections.GRSP %>%
    filter(survive == 0 &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    arrange(anillo)
  out$nMort.GRSP[i] <- nrow(obs.mort)
  obs.premort <- detections.GRSP %>%
    filter(survive == 1 &
             Site == out$Site[i] &
             Season == out$Season[i]) %>%
    group_by(anillo) %>%
    summarise(penultDOS = max(DOS)) %>%
    ungroup %>% filter(anillo %in% obs.mort$anillo) %>%
    arrange(anillo)
  gaps <- obs.mort %>%
    left_join(obs.premort, by = "anillo") %>%
    mutate(diff = DOS - penultDOS) %>%
    pull(diff)
  out$nMortWithGaps.GRSP[i] <- sum(gaps > 1)
  out$SumDaysMortGaps.GRSP[i] <- sum(gaps)
  out$MedDaysMortGaps.GRSP[i] <- ifelse(any(gaps > 1), median(gaps[which(gaps > 1)]), 0)
}

write.csv(out, "Prop_survey_days_detected_when_known_alive.csv", row.names = F)
