library(tidyverse)
library(R.utils)
library(QSLpersonal)

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")

## Functions ##
expit <- function(x) exp(x) / (1 + exp(x))

sum.fn <- function (x, ndig = 2) {
  mn <- mean(x)
  se <- sd(x) / sqrt(length(x))
  CI95.lo <- mn - 1.96 * se
  CI95.hi <- mn + 1.96 * se
  #q05 <- quantile(x, prob = 0.05, type = 8)
  #q95 <- quantile(x, prob = 0.95, type = 8)
  x.sum <- str_c(round(mn, digits = ndig),
                        " (", round(CI95.lo, digits = ndig),
                        ",", round(CI95.hi, digits = ndig),
                 ")")#,
                 #"; q95:(",
                 #round(q05, digits = ndig),
                 #",",
                 #round(q95, digits = ndig),
                 #")")
  return(x.sum)
}

#________________ Load data ____________________#
load("Data_compiled_MissingCovsImputed.RData")
dat.BAIS <- read.csv("BAIS_30_worst_and_best_shrubs.csv", header = T, stringsAsFactors = F) %>%
  mutate(PredSurvLevel = c(rep("low", 30), rep("high", 31)))
dat.GRSP <- read.csv("GRSP_30_worst_and_best_shrubs.csv", header = T, stringsAsFactors = F) %>%
  mutate(PredSurvLevel = c(rep("low", 30), rep("high", 31)))
dat.individual.shrubsums <- read.csv("Shrub_summaries_for_high-low_survival_individuals.csv",
                                     header = T, stringsAsFactors = F) %>%
  select(-X) %>%
  rename(anillo = band) %>%
  filter(!is.na(mean_height_m)) # Not sure yet what the story is with this one record.
#_______________________________________________#

dat.sum <- dat.BAIS %>%
  mutate(Species = "BAIS") %>%
  bind_rows(
    dat.GRSP %>%
      mutate(Species = "GRSP")
  ) %>%
  left_join(dat.individual.shrubsums %>% select(-site),
            by = "anillo") %>%
  filter(!is.na(number_shrubs)) %>%
  mutate(Shrub_density = number_shrubs / 8) %>%
  select(Site:Species, Shrub_density,
         shrub_density_pct:average_minimum_distance_btwn_shrubs_m) %>%
  rename(Shrub_cover = shrub_density_pct)

sum.tab <- dat.sum %>%
  select(Species, PredSurvLevel,
         Shrub_density:average_minimum_distance_btwn_shrubs_m) %>%
  group_by(Species, PredSurvLevel) %>%
  summarise_all(function(x) sum.fn(x)) %>%
  mutate(Site = "All") %>%
  select(Species, Site, PredSurvLevel:average_minimum_distance_btwn_shrubs_m) %>%
  bind_rows(
    dat.sum %>%
      select(Species, Site, PredSurvLevel,
             Shrub_density:average_minimum_distance_btwn_shrubs_m) %>%
      group_by(Species, Site, PredSurvLevel) %>%
      summarise_all(function(x) sum.fn(x))
  )

write.csv(sum.tab, "Shrub_sums_high_vs_low.csv", row.names = F)

t.test.fn <- function(x, lab) {
  if(length(x) != length(lab)) stop("Error: Lengths of x and lab differ.")
  if(length(unique(lab)) == 2 &
     !any(tapply(x, lab, length) < 2)) {
    v1 <- unique(lab)[1]
    v2 <- unique(lab)[2]
    x1 <- x[which(lab == v1)]
    x2 <- x[which(lab == v2)]
    tst <- t.test(x1, x2)
    return(tst$p.value)
  } else {
    return(NA)
  }
}

t.test.tab <- dat.sum %>%
  select(Species, Site) %>%
  distinct() %>%
  mutate(Shrub_density = as.numeric(NA),
         Shrub_cover = as.numeric(NA),
         mean_shrub_size_sqm = as.numeric(NA),
         median_shrub_size_sq_m = as.numeric(NA),
         mean_height_m = as.numeric(NA),
         median_height_m = as.numeric(NA),
         average_minimum_distance_btwn_shrubs_m = as.numeric(NA))
vars <- names(t.test.tab)[-c(1:2)]
for(i in 1:nrow(t.test.tab)) {
  for(j in 1:length(vars)) {
    v <- vars[j]
    row.ind <- which(dat.sum$Site == t.test.tab$Site[i] &
                       dat.sum$Species == t.test.tab$Species[i])
    x <- dat.sum[row.ind,v]
    lab <- dat.sum$PredSurvLevel[row.ind]
    t.test.tab[i, v] <- t.test.fn(x, lab)
  }
}

t.test.tab <- t.test.tab %>%
  bind_rows(t.test.tab %>% slice(1:2) %>%
              mutate(Species = c("BAIS", "GRSP"),
                     Site = "All",
                     Shrub_density = as.numeric(NA),
                     Shrub_cover = as.numeric(NA),
                     mean_shrub_size_sqm = as.numeric(NA),
                     median_shrub_size_sq_m = as.numeric(NA),
                     mean_height_m = as.numeric(NA),
                     median_height_m = as.numeric(NA),
                     average_minimum_distance_btwn_shrubs_m = as.numeric(NA)))
Spp <- c("BAIS", "GRSP")
for(i in 1:length(Spp)) {
  for(j in 1:length(vars)) {
    v <- vars[j]
    row.ind <- which(dat.sum$Species == Spp[i])
    x <- dat.sum[row.ind,v]
    lab <- dat.sum$PredSurvLevel[row.ind]
    row.ind <- which(t.test.tab$Species == Spp[i] &
                       t.test.tab$Site == "All")
    t.test.tab[row.ind, v] <- t.test.fn(x, lab)
  }
}

write.csv(t.test.tab, "Shrub_sum_p_values.csv", row.names = F)
