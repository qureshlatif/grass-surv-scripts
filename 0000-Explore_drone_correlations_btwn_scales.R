library(tidyverse)
library(geosphere)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

dat <- read.csv("drone_data/GPS Point Characteristics.csv", header = T, stringsAsFactors = F) %>%
  filter(!is.na(Distance_to_Fence)) %>% # Removes records with no data at all.
  select(Site, Season, anillo, contains("m_Mesquite"), contains("m_Mimosa"), contains("m_Juniper"), contains("m_Yucca"),
         contains("m_Max_Height_Mesquite"), contains("m_Max_Height_Yucca"), contains("m_Max_Height_Juniper"),
         contains("m_Mean_Height_Mesquite"), contains("m_Mean_Height_Yucca"), contains("m_Mean_Height_Juniper"),
         contains("m_Shrub_All_."), contains("m_Max_Shrub_Height"), contains("m_Mean_Shrub_Height")) %>%
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
                                        ifelse(X25m_Max_Shrub_Height == -Inf & Site != "Marfa", NA, X25m_Max_Shrub_Height)))

cor(dat %>% select(X5m_Mesquite:X500m_Mesquite), use = "complete")
cor(dat %>% select(X5m_Juniper:X500m_Juniper), use = "complete")
cor(dat %>% select(X5m_Yucca:X500m_Yucca), use = "complete")
cor(dat %>% select(X5m_Max_Height_Mesquite:X500m_Max_Height_Mesquite), use = "complete")
cor(dat %>% select(X5m_Max_Height_Juniper:X500m_Max_Height_Juniper), use = "complete")
cor(dat %>% select(X5m_Max_Height_Yucca:X500m_Max_Height_Yucca), use = "complete")
cor(dat %>% select(X5m_Mean_Height_Mesquite:X500m_Mean_Height_Mesquite), use = "complete")
cor(dat %>% select(X5m_Mean_Height_Juniper:X500m_Mean_Height_Juniper), use = "complete")
cor(dat %>% select(X5m_Shrub_All_.:X500m_Shrub_All_.), use = "complete")
cor(dat %>% select(X5m_Max_Shrub_Height:X500m_Max_Shrub_Height), use = "complete")
cor(dat %>% select(X5m_Mean_Shrub_Height:X500m_Mean_Shrub_Height), use = "complete")
