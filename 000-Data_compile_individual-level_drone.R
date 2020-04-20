library(tidyverse)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

# 7. Attach drone variables
dat.drone <- read.csv("drone_data/GPS Point Characteristics.csv", header = T, stringsAsFactors = F) %>%
  filter(!is.na(Distance_to_Fence)) %>% # Removes records with no data at all.
  select(Site, Season, anillo, contains("m_Mesquite"), contains("m_Mimosa"), contains("m_Juniper"), contains("m_Yucca"),
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
names.to.change <- names(dat.drone)[-c(1:3, ncol(dat.drone))]
names.new <- str_split(names.to.change, "m_", simplify = T)[,2] %>%
  str_c("_",
        str_split(names.to.change, "m_", simplify = T)[,1] %>% str_sub(2, -1),
        "m")
names.new <- str_replace(names.new, "_._", "_")
names(dat.drone)[-c(1:3, ncol(dat.drone))] <- names.new

dat.drone.means <- dat.drone %>% group_by(Site, Season, anillo) %>%
  summarise_all(mean, na.rm = T)

dat.drone.CVs <- dat.drone %>% group_by(Site, Season, anillo) %>%
  summarise_all((function(x) sd(x, na.rm = T) / mean(x, na.rm = T))) %>%
  ungroup %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x)))
names(dat.drone.CVs)[-c(1:3)] <- str_c(names(dat.drone.CVs)[-c(1:3)], "_CV")

dat.drone.n <- dat.drone %>% group_by(Site, Season, anillo) %>%
  summarise_all((function(x) sum(!is.na(x))))
names(dat.drone.n)[-c(1:3)] <- str_c(names(dat.drone.n)[-c(1:3)], "_n")

dat.drone.sum <- dat.drone.means %>%
  left_join(dat.drone.CVs, by = c("Site", "Season", "anillo")) %>%
  left_join(dat.drone.n, by = c("Site", "Season", "anillo"))

write.csv(dat.drone.sum, "Drone_individual.csv", row.names = F)
