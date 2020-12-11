library(tidyverse)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

dat.drone.0 <- read.csv("Drone_individual.csv", header = T, stringsAsFactors = F)
dat.drone.1 <- read.csv("Drone_individual_noCM.csv", header = T, stringsAsFactors = F)

vars <- c("Mesquite_5m", "Mesquite_500m", "Juniper_5m", "Juniper_500m",
          "Yucca_5m", "Yucca_500m", "Shrub_All_5m", "Max_Shrub_Height_5m",
          "Distance_to_Fence", "Shrub_All_5m_CV", "Shrub_All_50m_CV", "Shrub_All_500m_CV")

dat.drone <- dat.drone.0 %>%
  select(Site, Season, anillo, one_of(vars)) %>%
  left_join(
    dat.drone.1 %>%
      select(Site, Season, anillo, one_of(vars)),
    by = c("Site", "Season", "anillo")
  )

cols <- c("mean_original", "mean_new", "mean_dropped", "correlation")
out.tab <- matrix(NA, nrow = length(vars), ncol = length(cols),
                  dimnames = list(vars, cols))
for(v in vars) {
  out.tab[v, "mean_original"] <- mean(dat.drone[, str_c(v, ".x")], na.rm = T) %>% round(digits = 4)
  out.tab[v, "mean_new"] <- mean(dat.drone[, str_c(v, ".y")], na.rm = T) %>% round(digits = 4)
  ind.dropped <- which(!is.na(dat.drone[, str_c(v, ".x")]) & is.na(dat.drone[, str_c(v, ".y")])) %>% round(digits = 4)
  out.tab[v, "mean_dropped"] <- mean(dat.drone[ind.dropped, str_c(v, ".x")], na.rm = T) %>% round(digits = 4)
  out.tab[v, "correlation"] <- cor(dat.drone[, str_c(v, ".x")], dat.drone[, str_c(v, ".y")], use = "complete") %>% round(digits = 4)
}

write.csv(out.tab, "Compare_DroneVals_after_remove_CM.csv")
