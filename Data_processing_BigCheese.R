data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- data.spp$Covs$firstDay
last <- data.spp$Covs$lastDay
last.alive <- data.spp$Covs$lastAlive
nBird <- nrow(ymat)
nDOS <- ncol(ymat)
SeasonInd <- data.spp$Covs$SeasonInd
nSeason <- max(SeasonInd)
SiteInd <- data.spp$Covs$SiteInd
nSite <- max(SiteInd)

## Covariates ##
# Date #
DOS <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
X.mn <- c(DOS = mean(DOS[which(!is.na(ymat))]))
X.sd <- c(DOS = sd(DOS[which(!is.na(ymat))]))
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

X.nams <- c("Intercept", "DOS", "DOS2", "temp.min", "temp.prec7")

# Vegetation #
Veg <- data.spp$Covs %>% ungroup() %>%
  select(hierbas, hierba_ht, pastos, pasto_ht, salsola, otra, #arbusto,
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
  select(hierbas_cv, otra_cv, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV)
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
  select(peso, female, adult)
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
if(spp == "GRSP") {
  Ind.z <- Ind.z %>% select(-adult)
} else {
  Ind.z <- Ind.z
}
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

X <- abind::abind(array(1, dim = dim(DOS)), DOS, DOS2, temp.min, temp.prec7,
                  Veg.z, Veg2.z, VegCV.z, Ind.z, Site.z, along = 3)
Y <- ymat

n=dim(Y)[1]
J=dim(Y)[2]

# Add interactions #
X <- abind::abind(X, array(NA, dim = c(dim(X)[1:2], 1)))
X.nams <- c(X.nams, "Max_Shrub_Height_5mXShrub_All_5m")
dimnames(X)[[3]] <- X.nams
X[,,"Max_Shrub_Height_5mXShrub_All_5m"] <- X[,,"Max_Shrub_Height_5m"]*X[,,"Shrub_All_5m"]

# # Add interactions #
# X <- abind::abind(X, array(NA, dim = c(dim(X)[1:2], 9)))
# X.nams <- c(X.nams, "pesoXtemp.min", "pesoXtemp.p7", "pastosXtemp.min", "pastosXtemp.p7",
#             "pasto_ht_cvXtemp.min", "pasto_ht_cvXtemp.p7", "Shrub_All_5mXtemp.min",
#             "Shrub_All_5mXtemp.p7", "Mean_Shrub_Height_5mXLOSH")
# dimnames(X)[[3]] <- X.nams
# X[,,"pesoXtemp.min"] <- X[,,"peso"]*X[,,"temp.min"]
# X[,,"pesoXtemp.p7"] <- X[,,"peso"]*X[,,"temp.prec7"]
# X[,,"pastosXtemp.min"] <- X[,,"pastos"]*X[,,"temp.min"]
# X[,,"pastosXtemp.p7"] <- X[,,"pastos"]*X[,,"temp.prec7"]
# X[,,"pasto_ht_cvXtemp.min"] <- X[,,"pasto_ht_cv"]*X[,,"temp.min"]
# X[,,"pasto_ht_cvXtemp.p7"] <- X[,,"pasto_ht_cv"]*X[,,"temp.prec7"]
# X[,,"Shrub_All_5mXtemp.min"] <- X[,,"Shrub_All_5m"]*X[,,"temp.min"]
# X[,,"Shrub_All_5mXtemp.p7"] <- X[,,"Shrub_All_5m"]*X[,,"temp.prec7"]
# X[,,"Mean_Shrub_Height_5mXLOSH"] <- X[,,"Mean_Shrub_Height_5m"]*X[,,"LOSH"]

rm(i, DOS, DOS2, temp.min, temp.prec7, Veg, Veg.z, Veg2.z, VegCV, VegCV.z, Ind, Ind.z, X.add, Site, Site.z)
