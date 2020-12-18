# Detection data #
data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- data.spp$Covs$firstDay
last <- data.spp$Covs$lastDay
nBird <- nrow(ymat)
nDOS <- ncol(ymat)
SeasonInd <- data.spp$Covs$SeasonInd
nSeason <- max(SeasonInd)
SiteInd <- data.spp$Covs$SiteInd
nSite <- max(SiteInd)

## Covariates ##
# Date #
DOS <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
DOSdepl <- first
time_since_depl <- DOS - DOSdepl
after_depl <- (time_since_depl > 0)*1

X.mn <- mean(DOS[which(!is.na(ymat))]); X.sd <- sd(DOS[which(!is.na(ymat))])
names(X.mn) <- names(X.sd) <- "DOS"
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])
DOS2 <- DOS^2

# Temperature #
temp.min <- temp.prec7 <- array(NA, dim = dim(DOS))
for(i in 1:nBird) {
  ind <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(DOS)
  temp.min[i, ind] <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(temp.min)
  temp.prec7[i, ind] <- dat.temp %>%
    filter(Site == data.spp$Covs$Site[i] &
             Season == data.spp$Covs$Season[i]) %>%
    pull(temp.prec7)
}
X.mn <- c(X.mn, temp.min = mean(temp.min[which(!is.na(ymat))]), temp.prec7 = mean(temp.prec7[which(!is.na(ymat))]))
X.sd <- c(X.sd, temp.min = sd(temp.min[which(!is.na(ymat))]), temp.prec7 = sd(temp.prec7[which(!is.na(ymat))]))
temp.min <- (temp.min - mean(temp.min[which(!is.na(ymat))])) / sd(temp.min[which(!is.na(ymat))])
temp.min[which(is.na(temp.min))] <- 0
temp.prec7 <- (temp.prec7 - mean(temp.prec7[which(!is.na(ymat))])) / sd(temp.prec7[which(!is.na(ymat))])
temp.prec7[which(is.na(temp.prec7))] <- 0

X.nams <- c("DOS", "DOS2", "temp.min", "temp.prec7")

# Vegetation #
Veg <- data.spp$Covs %>% ungroup() %>%
  select(hierbas, hierba_ht, pastos, pasto_ht, salsola, otra,
         Shrub_All_5m, Max_Shrub_Height_5m, Distance_to_Fence)
X.add <- Veg %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Veg)
X.mn <- c(X.mn, X.add)
X.add <- Veg %>%
  summarise_all(function(x) sd(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Veg)
X.sd <- c(X.sd, X.add)
Veg.z <- Veg %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(Veg.z), str_c(names(Veg.z)[-ncol(Veg.z)], "2"))
Veg.z <- Veg.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

Veg2.z <- Veg.z[,,-dim(Veg.z)[3]] ^ 2

VegCV <- data.spp$Covs %>%
  select(hierbas_cv, Shrub_All_5m_CV)
X.add <- VegCV %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(VegCV)
X.mn <- c(X.mn, X.add)
X.add <- VegCV %>%
  summarise_all(function(x) sd(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(VegCV)
X.sd <- c(X.sd, X.add)
VegCV.z <- VegCV %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(VegCV.z))
VegCV.z <- VegCV.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

# Individual #
Ind <- data.spp$Covs %>% ungroup() %>%
  select(peso)
X.add <- Ind %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Ind)
X.mn <- c(X.mn, X.add)
X.add <- Ind %>%
  summarise_all(function(x) sd(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Ind)
X.sd <- c(X.sd, X.add)
Ind.z <- Ind %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T))) %>%
  mutate_all((function(x) ifelse(is.na(x), mean(x, na.rm = T), x)))
X.nams <- c(X.nams, names(Ind.z))
Ind.z <- Ind.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

# Site #
Site <- data.spp$Covs %>%
  select(prey, LOSH, raptor, NDVI)
X.add <- Site %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Site)
X.mn <- c(X.mn, X.add)
X.add <- Site %>%
  summarise_all(function(x) sd(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.add) <- names(Site)
X.sd <- c(X.sd, X.add)
Site.z <- Site %>% mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(Site.z))
Site.z <- Site.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

X <- abind::abind(DOS, DOS2, temp.min, temp.prec7, Veg.z, Veg2.z, VegCV.z, Ind.z, Site.z, along = 3)
dimnames(X)[[3]] <- X.nams

# # Add shrub cover X height interaction #
# X <- abind::abind(X, array(NA, dim = c(dim(X)[1:2], 1)))
# X.nams <- c(X.nams, "Max_Shrub_Height_5mXShrub_All_5m")
# dimnames(X)[[3]] <- X.nams
# X[,,"Max_Shrub_Height_5mXShrub_All_5m"] <- X[,,"Max_Shrub_Height_5m"]*X[,,"Shrub_All_5m"]

ncovs <- dim(X)[3]
Y.alive <- (ymat == 1)*1
Y.dead <- (ymat == 2)*1
z.init <- (ymat == 1)*1
for(i in 1:length(first)) {
  z.init[i, (first[i]+1):data.spp$Covs$lastAlive[i]] <- 1
  z.init[i, first[i]] <- NA
  if(any(ymat[i, ] == 2, na.rm = T)) z.init[i, which(ymat[i, ] == 2)] <- 0
}

rm(i, DOS, DOS2, Veg, Veg.z, Veg2.z, VegCV, VegCV.z, X.add, Site, Site.z)
