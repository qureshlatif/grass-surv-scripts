library(tidyverse)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
rm.caps.mort <- T # If true, remove all capture, mortality, and transmitter lost locations

# Attach drone variables
dat.drone <- read.csv("drone_data/GPS Point Characteristics.csv", header = T, stringsAsFactors = F) %>%
  filter(!is.na(Distance_to_Fence)) %>% # Removes records with no data at all.
  select(Site, Season, anillo, estatus, contains("m_Mesquite"), contains("m_Mimosa"), contains("m_Juniper"), contains("m_Yucca"),
         contains("m_Max_Height_Mesquite"), contains("m_Max_Height_Yucca"), contains("m_Max_Height_Juniper"),
         contains("m_Mean_Height_Mesquite"), contains("m_Mean_Height_Yucca"), contains("m_Mean_Height_Juniper"),
         contains("m_Shrub_All_."), contains("m_Max_Shrub_Height"), contains("m_Mean_Shrub_Height"), Distance_to_Fence) %>%
  # Merge Mimosa and Mesquite - essentially the same but named differently at different sites
  mutate(X5m_Mesquite = ifelse(is.na(X5m_Mesquite), X5m_Mimosa, X5m_Mesquite)) %>% select(-X5m_Mimosa) %>%
  mutate(X25m_Mesquite = ifelse(is.na(X25m_Mesquite), X25m_Mimosa, X25m_Mesquite)) %>% select(-X25m_Mimosa) %>%
  mutate(X50m_Mesquite = ifelse(is.na(X50m_Mesquite), X50m_Mimosa, X50m_Mesquite)) %>% select(-X50m_Mimosa) %>%
  mutate(X100m_Mesquite = ifelse(is.na(X100m_Mesquite), X100m_Mimosa, X100m_Mesquite)) %>% select(-X100m_Mimosa) %>%
  mutate(X200m_Mesquite = ifelse(is.na(X200m_Mesquite), X200m_Mimosa, X200m_Mesquite)) %>% select(-X200m_Mimosa) %>%
  mutate(X300m_Mesquite = ifelse(is.na(X300m_Mesquite), X300m_Mimosa, X300m_Mesquite)) %>% select(-X300m_Mimosa) %>%
  mutate(X500m_Mesquite = ifelse(is.na(X500m_Mesquite), X500m_Mimosa, X500m_Mesquite)) %>% select(-X500m_Mimosa) %>%
  mutate(X5m_Max_Shrub_Height = ifelse(X5m_Max_Shrub_Height == -Inf & Site == "Marfa", 0,
                                       ifelse(X5m_Max_Shrub_Height == -Inf & Site != "Marfa", NA, X5m_Max_Shrub_Height))) %>%
  mutate(X25m_Max_Shrub_Height = ifelse(X25m_Max_Shrub_Height == -Inf & Site == "Marfa", 0,
                                        ifelse(X25m_Max_Shrub_Height == -Inf & Site != "Marfa", NA, X25m_Max_Shrub_Height))) %>%
  mutate_all((function(x) ifelse(is.na(x), 0, x)))
names.to.change <- names(dat.drone)[-c(1:4, ncol(dat.drone))]
names.new <- str_split(names.to.change, "m_", simplify = T)[,2] %>%
  str_c("_",
        str_split(names.to.change, "m_", simplify = T)[,1] %>% str_sub(2, -1),
        "m")
names.new <- str_replace(names.new, "_._", "_")
names(dat.drone)[-c(1:4, ncol(dat.drone))] <- names.new

if(rm.caps.mort) {
  rm.codes <- c("C", "M", "R", "R1", "R2", "RF", "RC", "U")
  dat.drone <- dat.drone %>% filter(!estatus %in% rm.codes)
}
dat.drone <- dat.drone %>%
  select(-estatus)
# sum(dat.drone$estatus %in% rm.codes)
# dat.drone.sum <- dat.drone %>% dplyr::group_by(anillo) %>%
#   summarise(n.CM = sum(estatus %in% rm.codes, na.rm = T), n = n()) %>%
#   mutate(Prp_CM = n.CM / n)

dat.drone.means <- dat.drone %>% group_by(Site, Season, anillo) %>%
  select(Site, Season, anillo, Mesquite_5m:Yucca_500m, Shrub_All_5m:Shrub_All_500m, Distance_to_Fence) %>%
  summarise_all(mean, na.rm = T) %>%
  left_join(dat.drone %>% group_by(Site, Season, anillo) %>%
              summarise(Max_Shrub_Height_5m = ifelse(any(Shrub_All_5m > 0), sum(Max_Shrub_Height_5m * Shrub_All_5m) / sum(Shrub_All_5m), NA),
                        Max_Shrub_Height_50m = ifelse(any(Shrub_All_50m > 0), sum(Max_Shrub_Height_50m * Shrub_All_50m) / sum(Shrub_All_50m), NA),
                        Max_Shrub_Height_500m = ifelse(any(Shrub_All_500m > 0), sum(Max_Shrub_Height_500m * Shrub_All_500m) / sum(Shrub_All_500m), NA),
                        Mean_Shrub_Height_5m = ifelse(any(Shrub_All_5m > 0), sum(Mean_Shrub_Height_5m * Shrub_All_5m) / sum(Shrub_All_5m), NA),
                        Mean_Shrub_Height_50m = ifelse(any(Shrub_All_50m > 0), sum(Mean_Shrub_Height_50m * Shrub_All_50m) / sum(Shrub_All_50m), NA),
                        Mean_Shrub_Height_500m = ifelse(any(Shrub_All_500m > 0), sum(Mean_Shrub_Height_500m * Shrub_All_500m) / sum(Shrub_All_500m), NA)) %>%
              select(Site, Season, anillo, Max_Shrub_Height_5m, Max_Shrub_Height_50m, Max_Shrub_Height_500m,
                     Mean_Shrub_Height_5m, Mean_Shrub_Height_50m, Mean_Shrub_Height_500m),
              by = c("Site", "Season", "anillo")) %>%
  select(Site:Shrub_All_500m, Max_Shrub_Height_5m:Mean_Shrub_Height_500m, Distance_to_Fence) %>%
  ungroup

dat.drone.CVs <- dat.drone %>% group_by(Site, Season, anillo) %>%
  select(Site, Season, anillo, Mesquite_5m:Yucca_500m, Shrub_All_5m:Shrub_All_500m) %>%
  summarise_all((function(x) sd(x, na.rm = T) / mean(x, na.rm = T))) %>%
  left_join(
    dat.drone %>% group_by(Site, Season, anillo) %>%
      summarise(Max_Shrub_Height_5m = ifelse(any(Shrub_All_5m > 0), sqrt(sum(((Max_Shrub_Height_5m - mean(Max_Shrub_Height_5m)) ^ 2) * Shrub_All_5m) /
                                                                           ((n() - 1) * sum(Shrub_All_5m)) / n()), NA),
                Max_Shrub_Height_50m = ifelse(any(Shrub_All_50m > 0), sqrt(sum(((Max_Shrub_Height_50m - mean(Max_Shrub_Height_50m)) ^ 2) * Shrub_All_50m) /
                                                                            ((n() - 1) * sum(Shrub_All_50m)) / n()), NA),
                Max_Shrub_Height_500m = ifelse(any(Shrub_All_500m > 0), sqrt(sum(((Max_Shrub_Height_500m - mean(Max_Shrub_Height_500m)) ^ 2) * Shrub_All_500m) /
                                                                             ((n() - 1) * sum(Shrub_All_500m)) / n()), NA),
                Mean_Shrub_Height_5m = ifelse(any(Shrub_All_5m > 0), sqrt(sum(((Mean_Shrub_Height_5m - mean(Mean_Shrub_Height_5m)) ^ 2) * Shrub_All_5m) /
                                                                            ((n() - 1) * sum(Shrub_All_5m)) / n()), NA),
                Mean_Shrub_Height_50m = ifelse(any(Shrub_All_50m > 0), sqrt(sum(((Mean_Shrub_Height_50m - mean(Mean_Shrub_Height_50m)) ^ 2) * Shrub_All_50m) /
                                                                             ((n() - 1) * sum(Shrub_All_50m)) / n()), NA),
                Mean_Shrub_Height_500m = ifelse(any(Shrub_All_500m > 0), sqrt(sum(((Mean_Shrub_Height_500m - mean(Mean_Shrub_Height_500m)) ^ 2) * Shrub_All_500m) /
                                                                              ((n() - 1) * sum(Shrub_All_500m)) / n()), NA)) %>%
      select(Site, Season, anillo, Max_Shrub_Height_5m, Max_Shrub_Height_50m, Max_Shrub_Height_500m,
             Mean_Shrub_Height_5m, Mean_Shrub_Height_50m, Mean_Shrub_Height_500m),
    by = c("Site", "Season", "anillo")
  ) %>%
  ungroup# %>%
  #mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x))) # I don't think we want to do this here. Instead, we want to do this for each dataset right before analysis.
names(dat.drone.CVs)[-c(1:3)] <- str_c(names(dat.drone.CVs)[-c(1:3)], "_CV")

dat.drone.n <- dat.drone %>% group_by(Site, Season, anillo) %>%
  summarise_all((function(x) sum(!is.na(x))))
names(dat.drone.n)[-c(1:3)] <- str_c(names(dat.drone.n)[-c(1:3)], "_n")

dat.drone.sum <- dat.drone.means %>%
  left_join(dat.drone.CVs, by = c("Site", "Season", "anillo")) %>%
  left_join(dat.drone.n, by = c("Site", "Season", "anillo")) %>%
  mutate(Max_Shrub_Height_5m_CV = Max_Shrub_Height_5m_CV / Max_Shrub_Height_5m,
         Max_Shrub_Height_50m_CV = Max_Shrub_Height_50m_CV / Max_Shrub_Height_50m,
         Max_Shrub_Height_500m_CV = Max_Shrub_Height_500m_CV / Max_Shrub_Height_500m,
         Mean_Shrub_Height_5m_CV = Mean_Shrub_Height_5m_CV / Mean_Shrub_Height_5m,
         Mean_Shrub_Height_50m_CV = Mean_Shrub_Height_50m_CV / Mean_Shrub_Height_50m,
         Mean_Shrub_Height_500m_CV = Mean_Shrub_Height_500m_CV / Mean_Shrub_Height_500m)

if(rm.caps.mort) {
  write.csv(dat.drone.sum, "Drone_individual_noCM.csv", row.names = F)
} else {
  write.csv(dat.drone.sum, "Drone_individual.csv", row.names = F)
}
