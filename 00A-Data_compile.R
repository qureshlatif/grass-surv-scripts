library(tidyverse)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

minDOS <- 317 # Sets first day of season - currently corresponds with 11/12/2012

dat.banding <- read.csv("wintersurvival_accessDBs/ExportedTables/Anillamiento_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday) %>%
  filter(captura != 6) %>%
  arrange(Site, Season, anillo, DOY) %>%
  distinct(Site, Season, anillo, .keep_all = T) # Not sure why I need this, but locations don't line up with veg without it. Need to investigate further once waypoints are clean.
dat.locations <- read.csv("wintersurvival_accessDBs/ExportedTables/Locaciones_Todos_all_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday)
dat.trans <- read.csv("wintersurvival_accessDBs/ExportedTables/Transmisores_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOYdepl = fecha_depl %>% mdy %>% yday)
dat.banding <- dat.banding %>%
  mutate(Tagged = anillo %in% dat.trans$anillo)
dat.veg <- read.csv("Veg_individual.csv", header = T, stringsAsFactors = F)
dat.drone <- read.csv("Drone_individual.csv", header = T, stringsAsFactors = F)

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
# rm(out, cols, counter, sit, spp, mass, massChange, nrows, daysMonitored)

# Compile capture histories and corresponding table of covariates and indices for analysis #
for(sp in species[1:2]) {
  dets <- str_c("detections.", sp) %>% # Get raw detections
    as.name %>% eval %>%
    arrange(Site, Season, anillo, DOS) %>%
    distinct(Site, Season, anillo, DOS, .keep_all = T)
  
  obsRaw <- dets %>% slice(1:(nrow(dets) - 1)) %>% # Put detections into interval format (intermediate processing step)
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
    arrange(Site, Season, anillo, DOS) %>%
    rename(DOSst = DOS, survive = survive1)
  
  # Build observation ID tables and data matrices for each species.
  Covs <- obsRaw %>% select(Site, SiteInd, Season, SeasonInd, anillo, Indiv) %>% unique
  ymat <- matrix(NA, nrow = nrow(Covs), ncol = max(dets$DOS))
  for(i in 1:nrow(Covs)) {
    obs <- obsRaw %>% filter(Indiv == Covs$Indiv[i] &
                               SiteInd == Covs$SiteInd[i] &
                               SeasonInd == Covs$SeasonInd[i] &
                               survive != 9)
    if(nrow(obs) > 0) {
      firstDay <- min(obs$DOSst)
      penultimateDay <- max(obs$DOSst)
      lastDay <- max(obs$DOSst) + obs$IntLength[which(obs$DOSst == max(obs$DOSst))]
      if(!any(obs$survive == 0)) ymat[i, firstDay:lastDay] <- 1
      if(any(obs$survive == 0)) {
        if(sum(obs$survive == 0) == 1 &
           which(obs$survive == 0) == which(obs$DOSst == max(obs$DOSst))) {
          ymat[i, firstDay:penultimateDay] <- 1
          ymat[i, lastDay] <- 0
          } else {
            stop("Error in survival history - more than one mortality or mortality not at the end.")
          }
      }
    # Stick 9s into the data matrix where present.
    obs9s <- obsRaw %>% filter(Indiv == Covs$Indiv[i] &
                                 SiteInd == Covs$SiteInd[i] &
                                 SeasonInd == Covs$SeasonInd[i] &
                                 survive == 9)
    if(nrow(obs9s) > 0) for(ii in nrow(obs9s):1) {
      firstDay9 <- obs9s$DOSst[ii]
      lastDay9 <- obs9s$DOSst[ii] + obs9s$IntLength[ii]
      if(lastDay9 == lastDay) {
        ymat[i, (firstDay9 + 1):lastDay9] <- 9
        lastDay <- firstDay9
      }
    }
    }
  }
  
  # Add tagging day #
  Covs <- Covs %>% left_join(dat.banding %>%
                               distinct(Site, Season, anillo, DOS, .keep_all = T) %>%
                               select(Site, Season, anillo, DOSdepl, grasa, peso),
                               by = c("Site", "Season", "anillo")) %>%
    left_join(dat.veg, by = c("Site", "Season", "anillo")) %>%
    left_join(dat.drone, by = c("Site", "Season", "anillo"))
  Covs$DOSdepl[which(is.na(Covs$DOSdepl))] <- 999

  # Remove observations with zero active monitoring days (0s or 1s).
  ind.rm <- which(apply(ymat, 1, function(x) any(x %in% c(0, 1))) == F)
  ymat <- ymat[-ind.rm, ]
  Covs <- Covs %>% slice(-ind.rm)
  rm(ind.rm)
  
  # Identify and store first and last days seen for each individual in each season.
  Covs <- Covs %>%
    mutate(firstDay = apply(ymat, 1, function(x) which(!is.na(x))[1]),
           lastDay = apply(ymat, 1, function(x) {
             active <- which(!is.na(x))
             return(active[length(active)])
             }))
  ymat[which(ymat == 9)] <- NA
  Covs$firstDay <- apply(ymat, 1, function(x) which(!is.na(x))[1]) # Recalculate so that first days do not precede the first active day.
  
  # Gather final data objects and store.
  dat <- list(Covs = Covs, ymat = ymat)
  assign(str_c("data.", sp), dat)
}

rm(dets, sp, ind.afterD31, ind.beforeD31, obs, obs9s, dat, Covs, obsRaw,
   ymat, i, ii, firstDay, lastDay, penultimateDay, firstDay9, lastDay9)
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
