library(tidyverse)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

# Attach drone variables
dat.drone <- read.csv("drone_data/GPS Point Characteristics.csv", header = T, stringsAsFactors = F) %>%
  filter(!is.na(Distance_to_Fence) & Season == "2017-2018") %>% # Removes records with no data at all.  & Season == "2018-2019"
  select(Site, Season, anillo, contains("X5m_"), contains("X50m_"), contains("X500m_")) %>%
  select(Site, Season, anillo, contains("m_Tumbleweed"), contains("m_Bare.Ground"), contains("m_Other.Grass"), contains("m_Tobosa"),
         contains("m_Curly.Mesquite"), contains("m_Rock"), contains("m_Amaranth"), contains("m_Grass_.")) %>%
  mutate_at(vars(contains("m_")), (function(x) ifelse(is.na(x), 0, x))) %>%
  # Merge Tobosa and Curly Mesquite into Hilaria
  mutate(X5m_Hilaria = X5m_Tobosa + X5m_Curly.Mesquite) %>% select(-X5m_Tobosa) %>% select(-X5m_Curly.Mesquite) %>%
  mutate(X50m_Hilaria = X50m_Tobosa + X50m_Curly.Mesquite) %>% select(-X50m_Tobosa)  %>% select(-X50m_Curly.Mesquite) %>%
  mutate(X500m_Hilaria = X500m_Tobosa + X500m_Curly.Mesquite) %>% select(-X500m_Tobosa)  %>% select(-X500m_Curly.Mesquite)

names.to.change <- names(dat.drone)[-c(1:3)]
names.new <- str_split(names.to.change, "m_", simplify = T)[,2] %>%
  str_c("_",
        str_split(names.to.change, "m_", simplify = T)[,1] %>% str_sub(2, -1),
        "m")
names.new <- str_replace(names.new, "\\.", "_")
names.new <- str_replace(names.new, "Grass__5", "Grass_5")
names(dat.drone)[-c(1:3)] <- names.new

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

write.csv(dat.drone.sum, "Drone_individual_nonShrub2018.csv", row.names = F)
