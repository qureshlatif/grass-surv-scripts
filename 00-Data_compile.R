library(dplyr)
library(stringr)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

minDOS <- 317 # Sets first day of season - currently corresponds with 11/12/2012

dat.banding <- read.csv("wintersurvival_accessDBs/ExportedTables/Anillamiento_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday) %>%
  filter(captura != 6)
dat.locations <- read.csv("wintersurvival_accessDBs/ExportedTables/Locaciones_Todos_all_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday)
dat.trans <- read.csv("wintersurvival_accessDBs/ExportedTables/Transmisores_forImport.csv", stringsAsFactors = F) %>%
  tbl_df()
dat.banding <- dat.banding %>%
  mutate(Tagged = anillo %in% dat.trans$anillo)

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
rm(D31)

# Remove human-caused deaths #
#dat.locations$survive[which(dat.locations$anillo == 272162859 & dat.locations$DOS == 84)] <- 9 # found dead without injuries the day after deployment.

# Get all detection histories by species #
species <- unique(dat.locations$especie)
for(sp in species) {
  dlocs <- dat.locations %>% filter(especie == sp) %>%
    select(anillo, Site, Season, DOS, survive) %>%
    filter(survive != 9) %>%
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

# Data summaries #
cols <- c("No_indivs", "No_Rtagged",  "No_ind_monitored", "Recap_indivis", "nDays", "nMort",
          "Mean_Days_radiotracked", "Min_Days_radiotracked", "Max_Days_radiotracked",
          "n_MassChange_Tagged", "Mean_MassChange_Tagged", "Min_MassChange_Tagged", "Max_MassChange_Tagged",
          "n_MassChange_Untagged", "Mean_MassChange_Untagged", "Min_MassChange_Untagged", "Max_MassChange_Untagged")
out <- matrix(NA, nrow = length(species), ncol = length(cols),
              dimnames = list(species, cols))

for(spp in species) {
  out[spp, "No_indivs"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>%
    pull(anillo) %>% unique %>% length
  out[spp, "No_Rtagged"] <- dat.banding %>%
    filter(especie == spp & Tagged) %>%
    pull(anillo) %>% unique %>% length
  out[spp, "Recap_indivis"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>%
    select(anillo, Season) %>% unique %>%
    pull(anillo) %>% tapply(., ., length) %>%
    (function(x) sum(x > 1))
  daysMonitored <- str_c("detections.", spp) %>%
    as.name %>% eval %>%
    dplyr::group_by(anillo, Season) %>%
    summarise(minDOS = min(DOS), maxDOS = max(DOS)) %>%
    mutate(days = maxDOS - minDOS) %>%
    ungroup %>%
    dplyr::group_by(anillo) %>%
    summarize(days = sum(days)) %>%
    pull(days)
  out[spp, "nDays"] <- sum(daysMonitored)
  out[spp, "nMort"] <- str_c("detections.", spp) %>%
    as.name %>% eval %>% pull(survive) %>% (function(x) sum(x == 0))
  out[spp, "No_ind_monitored"] <- sum(daysMonitored > 0)
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
    mutate(deltaMass = lastMass - firstMass) %>% pull(deltaMass)
  out[spp, "n_MassChange_Tagged"] <- length(massChange)
  out[spp, "Mean_MassChange_Tagged"] <- mean(massChange)
  out[spp, "Min_MassChange_Tagged"] <- min(massChange)
  out[spp, "Max_MassChange_Tagged"] <- max(massChange)
  massChange <- dat.banding %>%
    filter(especie == spp & !Tagged & peso != -999) %>%
    arrange(Season, anillo, DOS) %>%
    dplyr::group_by(Season, anillo) %>%
    mutate(firstMass = first(peso), lastMass = last(peso)) %>%
    summarise(ncapt = n(), firstMass = first(firstMass), lastMass = first(lastMass)) %>%
    filter(ncapt > 1) %>%
    mutate(deltaMass = lastMass - firstMass) %>% pull(deltaMass)
  out[spp, "n_MassChange_Untagged"] <- length(massChange)
  out[spp, "Mean_MassChange_Untagged"] <- mean(massChange)
  out[spp, "Min_MassChange_Untagged"] <- min(massChange)
  out[spp, "Max_MassChange_Untagged"] <- max(massChange)
}
write.csv(out, "Summary.csv", row.names = T)

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
write.csv(out, "Summary_mass_by_site.csv", row.names = T)

# Compile interval data #
for(sp in species[1:2]) {
  dets <- str_c("detections.", sp) %>%
    as.name %>% eval %>%
    arrange(Site, Season, anillo, DOS)
  obsInt <- dets %>% slice(1:(nrow(dets) - 1)) %>%
    bind_cols(
      dets %>% slice(2:nrow(dets))
    ) %>%
    filter(anillo1 == anillo &
             Site1 == Site &
             Season1 == Season &
             DOS1 > DOS) %>%
    mutate(Indiv = as.factor(anillo) %>% as.integer(),
           SiteInd = as.factor(Site) %>% as.integer(),
           SeasonInd = as.factor(Season) %>% as.integer(),
           IntLength = DOS1 - DOS) %>%
    select(anillo, Indiv, Site, SiteInd, Season, SeasonInd, DOS, survive1, IntLength) %>%
    rename(DOSst = DOS, survive = survive1)
  #cols <- c("Indiv", "SiteInd", "SeasonInd", "DOSst", "DOSend", "nDays", "survive")
  assign(str_c("obsIntervals.", sp), obsInt)
}
rm(obsInt, dets)

save.image("Data_compiled.RData")

# # Compile detection history matrix for review #
# season_days <- dat$season_day %>%
#   unique() %>% sort
# bands <- dat$band %>% unique %>% sort
# out <- matrix(NA, nrow = length(bands), ncol = length(season_days),
#               dimnames = list(bands, season_days))
# for(day in season_days) {
#   obs <- dat %>% filter(season_day == day) %>%
#     mutate(survive = as.character(survive)) %>%
#     mutate(survive = replace(survive, which(is.na(survive)), "NDet")) %>%
#     group_by(band, season_day) %>%
#     summarise(survive = min(survive))
#   out[as.character(obs$band), day] <- obs$survive
# }
# row.labs <- dat %>%
#   select(band, species) %>%
#   unique %>%
#   arrange(band) %>%
#   mutate(lab = str_c(species, band, sep = "_")) %>%
#   pull(lab)
# out <- out
