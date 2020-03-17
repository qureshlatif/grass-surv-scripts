library(dplyr)
library(stringr)
library(lubridate)
library(geosphere)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

dat <- read.csv("wintersurvival_accessDBs/ExportedTables/Vegetacion_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  distinct(Site, Season, waypoint, tipo, Latitude, Longitude, .keep_all = T) %>% # Dumping duplicates. Will need to investigate these at some point. Most of these are 
  rename(arbusto_ht = estatura_promedia_arbusto) %>%
  filter(tipo == "grid") %>%
  select(Site, Season, waypoint, Latitude, Longitude, arbusto, arbusto_ht) %>%
  mutate(arbusto = replace(arbusto, which(arbusto == -999), NA),
         arbusto_ht = replace(arbusto_ht, which(arbusto_ht == -999), NA))

key <- dat %>% select(Site, waypoint, Latitude, Longitude) %>%
  distinct(Site, waypoint, .keep_all = T)

Seasons <- sort(unique(dat$Season))
Sites <- sort(unique(dat$Site))

wp.mat <- matrix("", nrow = nrow(key), ncol = length(Seasons),
                 dimnames = list(NULL, Seasons))
for(seas in 1:length(Seasons)) {
  for(sit in Sites) {
    dat.sub <- dat %>% filter(Season == Seasons[seas] & Site == sit) %>%
      select(Site, waypoint, Latitude, Longitude)
    if(nrow(dat.sub) > 0) {
      D <- distm(key %>% filter(Site == sit) %>% select("Longitude", "Latitude"),
                 dat.sub[, c("Longitude", "Latitude")])
      minD <- apply(D, 1, min)
      minD.ind <- apply(D, 1, function(x) which(x == min(x))[1])
      wp.mat[which(key$Site == sit), seas][which(minD < 1)] <- dat.sub$waypoint[minD.ind[which(minD < 1)]]
    }
  }
}

#sum(apply(wp.mat, 1, function(x) sum(x != "")) > 1) # Number of points resurveyed
#time.between.resurveys <-
#  apply(wp.mat, 1, function(x) max(which(x != ""))) - apply(wp.mat, 1, function(x) min(which(x != "")))
#hist(time.between.resurveys[which(time.between.resurveys>0)])
#time.between.resurveys[which(time.between.resurveys>0)] %>% tapply(., ., length)

keep <- which(apply(wp.mat, 1, function(x) sum(x != "")) > 1)
key <- key %>% slice(keep)
wp.mat <- wp.mat[keep, ]
ShrubCov.mat <- ShrubHt.mat <- matrix(NA, nrow = nrow(key), ncol = 3,
                                      dimnames = list(NULL, c("first", "last", "time")))
for(i in 1:nrow(key)) {
  wp.vec <- wp.mat[i,]
  wp.ind <- which(wp.vec != "")
  shcov.vec <- shht.vec <- rep(NA, length(wp.vec))
  for(j in wp.ind) {
    shcov.vec[j] <- dat %>%
      filter(Site == key$Site[i] & Season == names(wp.vec)[j] & waypoint == wp.vec[j]) %>%
      pull(arbusto)
    shht.vec[j] <- dat %>%
      filter(Site == key$Site[i] & Season == names(wp.vec)[j] & waypoint == wp.vec[j]) %>%
      pull(arbusto_ht)
  }
  shcov.ind <- which(!is.na(shcov.vec))
  if(length(shcov.ind) > 1) {
    ShrubCov.mat[i, "first"] <- shcov.vec[min(shcov.ind)]
    ShrubCov.mat[i, "last"] <- shcov.vec[max(shcov.ind)]
    ShrubCov.mat[i, "time"] <- max(shcov.ind) - min(shcov.ind)
  }
  shht.ind <- which(!is.na(shht.vec))
  if(length(shht.ind) > 1) {
    ShrubHt.mat[i, "first"] <- shht.vec[min(shht.ind)]
    ShrubHt.mat[i, "last"] <- shht.vec[max(shht.ind)]
    ShrubHt.mat[i, "time"] <- max(shht.ind) - min(shht.ind)
  }
}
keep <- apply(ShrubCov.mat, 1, function(x) !any(is.na(x)))
ShrubCov.mat <- ShrubCov.mat[which(keep), ]
keep <- apply(ShrubHt.mat, 1, function(x) !any(is.na(x)))
ShrubHt.mat <- ShrubHt.mat[which(keep), ]
rm(keep)

# Analysis of shrub cover change #
ShrubCov.diff <- ShrubCov.mat[, "last"] - ShrubCov.mat[, "first"]
mean(ShrubCov.diff)
plot(ShrubCov.mat[, "time"], ShrubCov.diff)
cor(ShrubCov.mat[, "time"], ShrubCov.diff)
mod <- lm(ShrubCov.diff ~ ShrubCov.mat[, "time"])
length(ShrubCov.diff) # sample size

# Analysis of shrub height change #
ShrubHt.diff <- ShrubHt.mat[, "last"] - ShrubHt.mat[, "first"]
mean(ShrubHt.diff)
plot(ShrubHt.mat[, "time"], ShrubHt.diff)
cor(ShrubHt.mat[, "time"], ShrubHt.diff)
mod <- lm(ShrubHt.diff ~ ShrubHt.mat[, "time"])
length(ShrubHt.diff) # sample size

## Focused on CUZA ##
ind.site <- which(key$Site == "CUZA")
# Analysis of shrub cover change #
ShrubCov.diff <- ShrubCov.mat[ind.site, "last"] - ShrubCov.mat[ind.site, "first"]
mean(ShrubCov.diff)
plot(ShrubCov.mat[ind.site, "time"], ShrubCov.diff)
cor(ShrubCov.mat[ind.site, "time"], ShrubCov.diff)
mod <- lm(ShrubCov.diff ~ ShrubCov.mat[ind.site, "time"])
length(ShrubCov.diff) # sample size

# Analysis of shrub height change #
ShrubHt.diff <- ShrubHt.mat[ind.site, "last"] - ShrubHt.mat[ind.site, "first"]
mean(ShrubHt.diff)
plot(ShrubHt.mat[ind.site, "time"], ShrubHt.diff)
cor(ShrubHt.mat[ind.site, "time"], ShrubHt.diff)
mod <- lm(ShrubHt.diff ~ ShrubHt.mat[ind.site, "time"])
length(ShrubHt.diff) # sample size

## Focused on Janos ##
ind.site <- which(key$Site == "Janos")
# Analysis of shrub cover change #
ShrubCov.diff <- ShrubCov.mat[ind.site, "last"] - ShrubCov.mat[ind.site, "first"]
mean(ShrubCov.diff)
plot(ShrubCov.mat[ind.site, "time"], ShrubCov.diff)
cor(ShrubCov.mat[ind.site, "time"], ShrubCov.diff)
mod <- lm(ShrubCov.diff ~ ShrubCov.mat[ind.site, "time"])
length(ShrubCov.diff) # sample size

# Analysis of shrub height change #
ShrubHt.diff <- ShrubHt.mat[ind.site, "last"] - ShrubHt.mat[ind.site, "first"]
mean(ShrubHt.diff)
plot(ShrubHt.mat[ind.site, "time"], ShrubHt.diff)
cor(ShrubHt.mat[ind.site, "time"], ShrubHt.diff)
mod <- lm(ShrubHt.diff ~ ShrubHt.mat[ind.site, "time"])
length(ShrubHt.diff) # sample size
