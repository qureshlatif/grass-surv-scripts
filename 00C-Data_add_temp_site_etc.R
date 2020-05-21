library(tidyverse)
library(QSLpersonal)
library(corrplot)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")

## Temperature ##
dat.temp <- read.csv("weather/Data_temperature_all.csv", header = T, stringsAsFactors = F) %>%
  rename(Site = site) %>%
  mutate(Site = ifelse(Site == "cuza", "CUZA",
                       ifelse(Site == "janos", "Janos",
                              ifelse(Site == "vaco", "VACO", "Marfa")))) %>%
  mutate(Mo = str_sub(date, 6, 7)) %>%
  mutate(Season = ifelse(Mo %in% c("11", "12"),
                         str_c(str_sub(date, 1, 4), "-", as.integer(str_c(str_sub(date, 1, 4))) + 1),
                         str_c(as.integer(str_sub(date, 1, 4)) - 1, "-", str_sub(date, 1, 4)))) %>%
  select(Site, Season, date, DOS, temp.min, temp.prec7, temp.diff)
  

## Predator and prey density ##
dat.dens <- read.csv("PLMX density estimates all species 2019.csv", header = T, stringsAsFactors = F) %>%
  filter(Metric == "Density") %>%
  select(Stratum, Year, BirdCode, Median) %>%
  filter(Stratum %in% c("SRV-CUZA", "SRV-JANO", "SRV-VACO", "MIM")) %>%
  bind_rows(
    read.csv("Nonbreeding density estimates_REVISED.csv", header = T, stringsAsFactors = F) %>%
      filter(Metric == "Density") %>%
      select(Stratum, Year, BirdCode, Median) %>%
      filter(Stratum == "NB-TX35-MI")
  ) %>%
  mutate(Site = ifelse(Stratum == "SRV-CUZA", "CUZA",
                       ifelse(Stratum == "SRV-JANO", "Janos",
                              ifelse(Stratum == "SRV-VACO", "VACO", "Marfa")))) %>%
  filter(Year %in% 2013:2019) %>%
  mutate(Season = str_c(Year - 1, "-", Year)) %>%
  mutate(group = ifelse(BirdCode %in% c("HOLA", "SPPI", "CCLO", "BRSP", "VESP", "LARB",
                                       "SAVS", "GRSP", "BAIS", "EAME", "AMPA", "AMSP"),
                       "prey",
                       ifelse(BirdCode == "LOSH", "LOSH",
                              ifelse(BirdCode %in% c("NOHA", "AMKE", "MERL"), "raptor", "NONE")))) %>%
  filter(group != "NONE") %>%
  select(Site, Season, group, Median) %>%
  dplyr::group_by(Site, Season, group) %>%
  summarise(Density = sum(Median, na.rm = T)) %>%
  spread(key = group, value = Density)

## NDVI ##
dat.NDVI <- read.csv("NDVI_Nonbreeding_SRV_sites.csv", header = T, stringsAsFactors = F) %>%
  select(Site, NDVI_2012_NB:NDVI_2018_NB) %>%
  gather(key = Year, value = NDVI, -Site) %>%
  mutate(Season = str_c(str_sub(Year, 6, 9), "-", as.integer(str_sub(Year, 6, 9)) + 1)) %>%
  mutate(Site = ifelse(Site == "JANO", "Janos", Site),
         Site = ifelse(Site == "MARFA", "Marfa", Site)) %>%
  select(Site, Season, NDVI)

## Consolidate site-level covariates ##
dat.site <- SiteSeasons %>%
  left_join(dat.dens, by = c("Site", "Season")) %.%
  left_join(dat.NDVI, by = c("Site", "Season"))
  

# Fill missing values with mean of other values at the same site #
dat.site$prey[which(is.na(dat.site$prey) & dat.site$Site == "Janos")] <-
  mean(dat.site$prey[which(dat.site$Site == "Janos")], na.rm = T)
dat.site$LOSH[which(is.na(dat.site$LOSH) & dat.site$Site == "Janos")] <-
  mean(dat.site$LOSH[which(dat.site$Site == "Janos")], na.rm = T)
dat.site$raptor[which(is.na(dat.site$raptor) & dat.site$Site == "Janos")] <-
  mean(dat.site$raptor[which(dat.site$Site == "Janos")], na.rm = T)

rm(dat.NDVI, dat.dens)

## Add site-level covariate values to species data for analysis ## 
data.BAIS$Covs <- data.BAIS$Covs %>%
  left_join(dat.site, by = c("Site", "Season"))
data.GRSP$Covs <- data.GRSP$Covs %>%
  left_join(dat.site, by = c("Site", "Season"))

save.image("Data_compiled_MissingCovsImputed.RData")
