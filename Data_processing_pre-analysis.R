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

# Covariates #
DOS <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
DOS <- (DOS - mean(DOS[which(!is.na(ymat))])) / sd(DOS[which(!is.na(ymat))])

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
temp.min <- (temp.min - mean(temp.min[which(!is.na(ymat))])) / sd(temp.min[which(!is.na(ymat))])
temp.min[which(is.na(temp.min))] <- 0
temp.prec7 <- (temp.prec7 - mean(temp.prec7[which(!is.na(ymat))])) / sd(temp.prec7[which(!is.na(ymat))])
temp.prec7[which(is.na(temp.prec7))] <- 0

X.nams <- c("Intercept", "DOS", "temp.min", "temp.prec7")

Veg.z <- data.spp$Covs %>% ungroup() %>%
  mutate(OtherShrubs_5m = Shrub_All_5m - (Mesquite_5m + Juniper_5m + Yucca_5m),
         OtherShrubs_50m = Shrub_All_50m - (Mesquite_50m + Juniper_50m + Yucca_50m),
         OtherShrubs_500m = Shrub_All_500m - (Mesquite_500m + Juniper_500m + Yucca_500m)) %>%
  select(hierbas:otra, desnudo, Mesquite_5m, Mesquite_50m, Mesquite_500m,
         Juniper_5m, Juniper_50m, Juniper_500m, Yucca_5m, Yucca_50m, Yucca_500m,
         OtherShrubs_5m, OtherShrubs_50m, OtherShrubs_500m,
         Shrub_All_5m, Shrub_All_50m, Shrub_All_500m, Max_Shrub_Height_5m,
         Max_Shrub_Height_50m, Max_Shrub_Height_500m, Distance_to_Fence) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(Veg.z))
Veg.z <- Veg.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

VegCV.z <- data.spp$Covs %>%
  select(hierbas_cv:desnudo_cv, Shrub_All_5m_CV, Shrub_All_50m_CV, Shrub_All_500m_CV,
         Max_Shrub_Height_5m_CV, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(VegCV.z))
VegCV.z <- VegCV.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

Ind.z <- data.spp$Covs %>% ungroup() %>%
  select(peso, female, adult) %>%
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

Site.z <- data.spp$Covs %>%
  select(prey, LOSH, raptor, NDVI) %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
X.nams <- c(X.nams, names(Site.z))
Site.z <- Site.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

X <- abind::abind(array(1, dim = dim(DOS)), DOS, temp.min, temp.prec7,
                  Veg.z, VegCV.z, Ind.z, Site.z, along = 3)
Y <- ymat

n=dim(Y)[1]
J=dim(Y)[2]