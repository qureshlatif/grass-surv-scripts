library(tidyverse)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

minDOS <- 317 # Sets first day of season - currently corresponds with 11/12/2012

dat.banding <- read.csv("Banding_data_plus.csv", header = T, stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday) %>%
  filter(captura != 6) %>%
  arrange(Site, Season, anillo, DOY)# %>%
  #distinct(Site, Season, anillo, .keep_all = T) # I think I need the recapture dates now....

dat.locations <- read.csv("wintersurvival_accessDBs/ExportedTables/Locaciones_Todos_all_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(DOY = fecha %>% mdy %>% yday)
dat.trans <- read.csv("wintersurvival_accessDBs/ExportedTables/Transmisores_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(fecha_recup = ifelse(fecha_recup == "8/8/8888", "9999", fecha_recup)) %>%
  mutate(DOYdepl = fecha_depl %>% mdy %>% yday) %>%
  mutate(DOYultima = ultima_fecha %>% mdy %>% yday) %>%
  mutate(DOYrecup = fecha_recup %>% mdy %>% yday) %>%
  distinct(Site, Season, anillo, frecuencia, .keep_all = T)
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

dat.trans$DOSultima <- dat.trans$DOYultima
ind.beforeD31 <- which(dat.trans$DOYultima > 200)
ind.afterD31 <- which(dat.trans$DOYultima < 200)
dat.trans$DOSultima[ind.beforeD31] <- dat.trans$DOSultima[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.trans$ultima_fecha %>% str_sub(-4, -1) %>% str_c("12/31/", .) %>% mdy %>% yday)  - (minDOS - 1)
dat.trans$DOSultima[ind.afterD31] <- dat.trans$DOSultima[ind.afterD31] + D31[ind.afterD31]

dat.trans$DOSrecup <- dat.trans$DOYrecup
ind.beforeD31 <- which(dat.trans$DOYrecup > 200)
ind.afterD31 <- which(dat.trans$DOYrecup < 200)
dat.trans$DOSrecup[ind.beforeD31] <- dat.trans$DOSrecup[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.trans$fecha_recup %>% str_sub(-4, -1) %>% str_c("12/31/", .) %>% mdy %>% yday)  - (minDOS - 1)
dat.trans$DOSrecup[ind.afterD31] <- dat.trans$DOSrecup[ind.afterD31] + D31[ind.afterD31]
rm(D31)

# ***Temp fix mismatched species (remove after fixing in raw data)*** #
dat.locations$especie[which(dat.locations$anillo == 272137988)] <- "GRSP"
dat.trans$especie[which(dat.trans$anillo == 272137988)] <- "GRSP"

dat.locations$especie[which(dat.locations$anillo == 262124826)] <- "BAIS"
dat.trans$especie[which(dat.trans$anillo == 262124826)] <- "BAIS"

dat.locations$especie[which(dat.locations$anillo == 240112984)] <- "BAIS"
dat.trans$especie[which(dat.trans$anillo == 240112984)] <- "BAIS"

dat.locations$especie[which(dat.locations$anillo == 281110946 )] <- "BAIS"
dat.trans$especie[which(dat.trans$anillo == 281110946 )] <- "BAIS"

# (Temp) Export transmitters with 8/8/8888s for fecha recup #
# dat.export <- dat.trans %>% filter(fecha_recup == "8/8/8888")
# for(i in 1:nrow(dat.export)) {
#   add <- dat.trans %>% filter(Site == dat.export$Site[i] &
#                                 Season == dat.export$Season[i] &
#                                 frecuencia == dat.export$frecuencia[i] &
#                                 fecha_recup != "8/8/8888")
#   if(nrow(add) > 0) dat.export <- dat.export %>% bind_rows(add)
# }
# dat.export %>% arrange(Site, Season, frecuencia) %>%
#   write.csv("Transmitters_with_fechaRecup8.csv", row.names = F)
# Fill weird recovery date so that capture history is censored at when bird was last alive:
dat.trans$DOSrecup[which(dat.trans$frecuencia == 4.047 & dat.trans$DOSultima==91)] <- 91

# Get tagging statuses into banding data table.
dat.banding <- dat.banding %>%
  left_join( # If transmitter replaced, uses first time transmitter was deployed for date of deployment.
    dat.trans %>%
      select(Site, Season, anillo, DOSdepl) %>%
      group_by(Site, Season, anillo) %>%
      summarise(DOSdepl = min(DOSdepl)),
    by = c("Site", "Season", "anillo")) %>%
  mutate(Tagged = !is.na(DOSdepl))

# Calculate max tracking date given 55-day battery life #
maxNoDeployments <- dat.trans %>%
  mutate(ID = str_c(Site, Season, frecuencia, sep = "_")) %>%
  pull(ID) %>% tapply(., ., length) %>% max
trans.maxDOS <- dat.trans %>%
  group_by(Site, Season, frecuencia) %>%
  summarise(trans_pequena = first(trans_pequena),
            firstDOS = min(DOSdepl)) %>%
  mutate(maxDOS = ifelse(trans_pequena == 1,
                         firstDOS + 30, # Default battery life for small transmitters is 30 days.
                         firstDOS + 55)) # Default max is 55 days after first deployment. Subsequent loop adjusts for multiple deployments.
countUnexpectedMissingDOSrecup <- 0
dat.trans <- dat.trans %>% arrange(Site, Season, frecuencia, DOSdepl)
for(i in 1:nrow(trans.maxDOS)) {
  deployments <- dat.trans %>%
    filter(Site == trans.maxDOS$Site[i] &
             Season == trans.maxDOS$Season[i] &
             frecuencia == trans.maxDOS$frecuencia[i]) %>%
    arrange(DOSdepl)
  if(nrow(deployments) > 1) {
    if(sum(is.na(deployments$DOSrecup)) > 1)
      stop(str_c("Transmitter ", trans.maxDOS$frecuencia[i],
                 " at ", trans.maxDOS$Site[i],
                 " in ", trans.maxDOS$Season[i],
                 " has too many missing recovery dates."))
    if(sum(is.na(deployments$DOSrecup)) == 1)
       if(any(deployments$DOSdepl[which(is.na(deployments$DOSrecup))] != max(deployments$DOSdepl))) {
         warning(str_c("Transmitter ", trans.maxDOS$frecuencia[i],
                    " at ", trans.maxDOS$Site[i],
                    " in ", trans.maxDOS$Season[i],
                    " has at least one missing recovery date that precedes final deployment. Imputing with date preceding subsequent deployment."))
         ind.missing <- which(is.na(deployments$DOSrecup) & deployments$DOSdepl != max(deployments$DOSdepl))
         countUnexpectedMissingDOSrecup <- countUnexpectedMissingDOSrecup +
           length(ind.missing)
         deployments$DOSrecup[ind.missing] <- deployments$DOSdepl[ind.missing + 1]
         ind.fill <- which(dat.trans$Site == deployments$Site[ind.missing] &
                             dat.trans$Season == deployments$Season[ind.missing] &
                             dat.trans$frecuencia == deployments$frecuencia[ind.missing])
         dat.trans[ind.fill[ind.missing], "DOSrecup"] <- deployments$DOSrecup[ind.missing]
       }
    downDays <- sum(deployments$DOSdepl[2:nrow(deployments)] - deployments$DOSrecup[1:(nrow(deployments) - 1)])
    trans.maxDOS$maxDOS[i] <- trans.maxDOS$maxDOS[i] + downDays
  }
}

# Remove individuals never tagged.
dat.banding <- dat.banding %>% filter(!is.na(DOSdepl))

# Temp corrections #
dat.locations <- dat.locations %>%
  filter(!(anillo == 111111312 & Site == "VACO" & Season == "2018-2019" & DOS > 89)) # Remove extraneous mortality records following the initial observation.

# Get all detection histories by species #
species <- unique(dat.locations$especie)
for(sp in species) {
  dlocs <- dat.locations %>% filter(especie == sp) %>%
    select(anillo, Site, Season, DOS, survive) %>%
    mutate(Dataset = "Locations") %>% unique
  dbands <- dat.banding %>% filter(especie == sp) %>%
    select(anillo, Site, Season, DOSdepl) %>%
    rename(DOS = DOSdepl) %>%
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

# Check for individuals in location data but not in banding data table #
# dat.locations %>%
#  filter(!str_c(anillo, Site, Season, sep = "_") %in%
#           str_c(dat.banding$anillo, dat.banding$Site, dat.banding$Season, sep = "_")) %>%
#  select(anillo, Site, Season) %>% distinct %>%
#  write.csv("Located individuals missing from banding data.csv", row.names = F)

# Compile capture histories and corresponding table of covariates and indices for analysis #
#dat.trans %>% group_by(Site, Season, anillo) %>% summarise(n = n()) %>% pull(n) %>% max # Set number of columns for transmitter history fields based on this.
for(sp in species[1:2]) {
  dets <- str_c("detections.", sp) %>% # Get raw detections
    as.name %>% eval %>%
    arrange(Site, Season, anillo, DOS) %>%
    distinct(Site, Season, anillo, DOS, survive, .keep_all = T)
  
  Covs <- dets %>% # Set up data frame for covariate values
    select(Site, Season, anillo) %>%
    distinct() %>%
    arrange(Site, Season, anillo) %>%
    mutate(firstDay = NA, lastAlive = NA, lastDay = NA,
           freq1 = NA, break1Start = NA, break1End = NA,
           freq2 = NA, break2Start = NA, break2End = NA,
           freq.final = NA, resultado.final = NA) %>% 
    mutate(SiteInd = as.factor(Site) %>% as.integer,
           SeasonInd = as.factor(Season) %>% as.integer,
           Indiv = str_c(Site, Season, anillo, sep = "_") %>%
             as.factor() %>% as.integer) %>% unique %>%
    as.data.frame()
  ymat <- matrix(NA, nrow = nrow(Covs), ncol = max(dets$DOS)) # Set up data matrix
  for(i in 1:nrow(Covs)) {
    trans <- dat.trans %>%
      filter(Site == Covs$Site[i] &
               Season == Covs$Season[i] &
               anillo == Covs$anillo[i]) %>%
      arrange(DOSdepl)
    obs <- dets %>% filter(Site == Covs$Site[i] &
                             Season == Covs$Season[i] &
                             anillo == Covs$anillo[i] &
                             survive != 9)
    Covs$firstDay[i] <- min(trans$DOSdepl)
    Covs$lastAlive[i] <- max(obs %>% filter(survive == 1) %>% pull(DOS))
    if(sum(obs$survive == 0) > 1) stop(str_c("More than one mortality detection for band number ", Covs$anillo[i], " at ", Covs$Site[i], " in ", Covs$Season[i]))
    if(nrow(trans) == 1) { # If only one transmitter was deployed on the individual,...
      if(any(obs$survive == 0) & !is.na(trans$DOSrecup)) # If died, check that transmitter recovery date matches mortality date.
        if(trans$DOSrecup != max(obs %>% filter(survive == 0) %>% pull(DOS)))
          warning(str_c("Mortality does not match final transmitter recovery date for band number ", Covs$anillo[i], " at ", Covs$Site[i], " in ", Covs$Season[i]))
      maxDOS <- min(trans.maxDOS %>% filter(Site == trans$Site &
                                          Season == trans$Season &
                                          frecuencia == trans$frecuencia) %>%
        pull(maxDOS), max(dets$DOS))
      Covs$lastDay[i] <- ifelse(any(obs$survive == 0), # Set last possible day the individual could have been encountered (before accounting for when surveyors were in the field).
                                obs %>% filter(survive == 0) %>% pull(DOS), # If it was confirmed dead, set last day to confirmed death date.
                                ifelse(is.na(trans$DOSrecup),
                                       maxDOS, # If the transmitter was not recovered and death was not confirmed, set max tracking date based on transmitter battery life.
                                       trans$DOSrecup)) # If death was not confirmed, but the transmitter was recovered....
      Covs$freq.final[i] <- trans$frecuencia
      ymat[i, Covs$firstDay[i]:Covs$lastDay[i]] <- 0 # Set all possible detection days to zero (before accounting for when surveyors were in the field).
      ymat[i, (obs %>% filter(survive == 1) %>% pull(DOS))] <- 1 # Set y = 1 when observed alive.
      if(any(obs$survive == 0)) ymat[i, (obs %>% filter(survive == 0) %>% pull(DOS))] <- 2 # Set y = 2 when mortality confirmed.
    } else { # If more than one transmitter was deployed on the individual,...
      if(any(obs$survive == 0) & !is.na(trans$DOSrecup[nrow(trans)])) # If died, check that final transmitter recovery date matches mortality date.
        if(trans$DOSrecup[nrow(trans)] != max(obs %>% filter(survive == 0) %>% pull(DOS)))
          warning(str_c("Mortality does not match final transmitter recovery date for band number ", Covs$anillo[i], " at ", Covs$Site[i], " in ", Covs$Season[i]))
      maxDOS <- min(trans.maxDOS %>% filter(Site == trans$Site[nrow(trans)] &
                                          Season == trans$Season[nrow(trans)] &
                                          frecuencia == trans$frecuencia[nrow(trans)]) %>%
        pull(maxDOS), max(dets$DOS))
      Covs$lastDay[i] <- ifelse(any(obs$survive == 0), # Sets last possible day the individual could have been encountered (before accounting for when surveyors were in the field)
                                obs %>% filter(survive == 0) %>% pull(DOS), # If it was confirmed dead, set last day to confirmed death date.
                                ifelse(is.na(trans$DOSrecup[nrow(trans)]),
                                       maxDOS, # If the transmitter was not recovered and death was not confirmed, set max tracking date based on transmitter battery life.
                                       trans$DOSrecup[nrow(trans)])) # If death was not confirmed, but the transmitter was recovered, set to recovery date.
      Covs$freq.final[i] <- trans$frecuencia[nrow(trans)]
      ymat[i, Covs$firstDay[i]:Covs$lastDay[i]] <- 0 # Set all possible detection days to zero (before accounting for when surveyors were in the field and breaks due to transmitter replacement).
      ymat[i, (obs %>% filter(survive == 1) %>% pull(DOS))] <- 1 # Set y = 1 when observed alive.
      if(any(obs$survive == 0)) ymat[i, (obs %>% filter(survive == 0) %>% pull(DOS))] <- 2 # Set y = 2 when mortality confirmed.
      for(tr in 1:(nrow(trans)-1)) {
        Covs[i, str_c("freq", tr)] <- trans$frecuencia[tr]
        Covs[i, str_c("break", tr,"Start")] <- trans$DOSrecup[tr]
        Covs[i, str_c("break", tr,"End")] <- trans$DOSdepl[(tr + 1)]
        if((Covs[i, str_c("break", tr,"End")] - Covs[i, str_c("break", tr,"Start")]) > 1) {
          st <- (Covs[i, str_c("break", tr,"Start")] + 1) %>% as.integer
          end <- (Covs[i, str_c("break", tr,"End")]) %>% as.integer
          ymat[i, st:end] <- NA
          rm(st, end)
        }
      }
    }
    if(any(ymat[i, ] == 1, na.rm = T)) {
      if(Covs$firstDay[i] != min(which(ymat[i, ] == 1))) {
        warning(str_c("Deployment day does not match first observation day for band number ", Covs$anillo[i], " at ", Covs$Site[i], " in ", Covs$Season[i]))
        Covs$firstDay[i] <- min(which(ymat[i, ] == 1))
      }
    }
    if(Covs$lastDay[i] < Covs$lastAlive[i] & # For cases where bird lived and the battery lasted longer than the expected period.
       !any(ymat[i,] == 2, na.rm = T)) Covs$lastDay[i] <- Covs$lastAlive[i]
    
    if((trans %>%
      filter(DOSdepl == max(DOSdepl)) %>%
      pull(resultado) %>%
      length) > 1) stop("More than one final resultado.")
    Covs$resultado.final[i] <- trans %>%
      filter(DOSdepl == max(DOSdepl)) %>%
      pull(resultado)
  }
  
  # Add covariate values #
  Covs <- Covs %>% left_join(dat.banding %>%
                               distinct(Site, Season, anillo, DOS, .keep_all = T) %>%
                               filter(especie == sp) %>%
                               select(Site, Season, anillo, grasa, peso, female, adult) %>%
                               mutate(grasa = ifelse(grasa == -9, NA, grasa),
                                      peso = ifelse(peso == -999, NA, peso)) %>%
                               group_by(Site, Season, anillo) %>%
                               summarise(grasa = mean(grasa, na.rm = T), # If there > 1 capture with a value, calculate mean.
                                         peso = mean(peso, na.rm = T), # If there > 1 capture with a value, calculate mean.
                                         female = mean(female, na.rm = T), # If there > 1 capture with a value, calculate mean.
                                         adult = mean(adult, na.rm = T)),  # If there > 1 capture with a value, calculate mean.
                               by = c("Site", "Season", "anillo")) %>%
    left_join(dat.veg, by = c("Site", "Season", "anillo")) %>%
    left_join(dat.drone, by = c("Site", "Season", "anillo"))
  
  # Remove individuals with no live detections #
  ind.remove <- which(apply(ymat, 1, function(x) !any(x == 1, na.rm = T)))
  if(length(ind.remove) > 0) {
    ymat <- ymat[-ind.remove,]
    Covs <- Covs %>% slice(-ind.remove)
    rm(ind.remove)
  }

  # Gather final data objects and store.
  dat <- list(Covs = Covs, ymat = ymat)
  assign(str_c("data.", sp), dat)
}

# Insert NAs for days nothing was detected #
SiteSeasons <- dat.banding %>% select(Site, Season) %>% distinct()
for(ss in 1:nrow(SiteSeasons)) {
  ymat <- rbind(data.BAIS$ymat, data.GRSP$ymat)
  Covs <- rbind(data.BAIS$Covs, data.GRSP$Covs)
  ind.ss <- which(Covs$Site == SiteSeasons$Site[ss] &
                    Covs$Season == SiteSeasons$Season[ss])
  ind.nosurv <- which(apply(ymat[ind.ss, ], 2, function(x) !any(x %in% c(1, 2))))
  ymat[ind.ss, ind.nosurv] <- NA
  data.BAIS$ymat <- ymat[1:nrow(data.BAIS$ymat), ]
  data.GRSP$ymat <- ymat[(nrow(data.BAIS$ymat)+1):(nrow(data.BAIS$ymat)+nrow(data.GRSP$ymat)), ]
}

# Trim last days where field seasons ended before DOS = 124
for(sp in species[1:2]) {
  data.spp <- str_c("data.", sp) %>% as.name %>% eval
  data.spp$Covs$lastDay <- apply(data.spp$ymat, 1, function(x) max(which(!is.na(x))))
  assign(str_c("data.", sp), data.spp)
}

rm(dets, sp, ind.afterD31, ind.beforeD31, ind.ss, ss, data.spp, obs, dat, Covs,
   ymat, i, countUnexpectedMissingDOSrecup, downDays, ind.fill, ind.nosurv,
   ind.missing, maxDOS, maxNoDeployments, tr, trans, deployments)
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
