data.spp <- str_c("data.", spp) %>% as.name %>% eval
ymat <- data.spp$ymat
first <- (data.spp$Covs$firstDay)
last <- data.spp$Covs$lastDay
last.alive <- data.spp$Covs$lastAlive
nBird <- nrow(ymat)
nDOS <- ncol(ymat)

# Vegetation #
lin.only <- c("Tumbleweed_50m", "Rock_50m", "Rock_500m", "Amaranth_5m", "Amaranth_500m")
Veg <- data.spp$Covs %>% ungroup() %>%
  select(Shrub_All_5m, Max_Shrub_Height_5m, Juniper_5m, Mesquite_5m, Yucca_5m,
         Tumbleweed_50m, Hilaria_50m, Rock_50m, Rock_500m, Amaranth_5m, Amaranth_500m, Grass_50m)
X.mn <- Veg %>%
  summarise_all(function(x) mean(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.mn) <- names(Veg)
X.sd <- Veg %>%
  summarise_all(function(x) sd(x, na.rm = T)) %>% data.matrix() %>% as.numeric()
names(X.sd) <- names(Veg)
Veg.z <- Veg %>%
  mutate_all((function(x) (x - mean(x, na.rm = T)) / sd(x, na.rm = T)))
ind.lin <- which(names(Veg.z) %in% lin.only)
X.nams <- c("Intercept", names(Veg.z), str_c(names(Veg.z)[-ind.lin], "2"))
Veg.z <- Veg.z %>% data.matrix() %>%
  array(., dim = c(dim(.), nDOS)) %>%
  aperm(c(1, 3, 2))

Veg2.z <- Veg.z[,,-ind.lin] ^ 2

VegCV <- data.spp$Covs %>%
  select(Shrub_All_5m_CV, Shrub_All_50m_CV, Max_Shrub_Height_50m_CV, Max_Shrub_Height_500m_CV,
         Bare_Ground_50m_CV, Hilaria_5m_CV, Hilaria_50m_CV, Hilaria_500m_CV,
         Grass_5m_CV, Grass_500m_CV)
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

X <- abind::abind(array(1, dim = dim(Veg.z)[c(1, 2)]), Veg.z, Veg2.z, VegCV.z, along = 3)
dimnames(X)[[3]] <- X.nams
if(spp == "BAIS") {# Dump Rock and Amaranth for BAIS
  ind.rm <- which(X.nams %in% c("Rock_50m", "Rock_500m", "Amaranth_5m", "Amaranth_500m"))
  X <- X[,,-ind.rm]
  X.nams <- X.nams[-ind.rm]
  rm(ind.rm)
  }

Y <- ymat

n=dim(Y)[1]
J=dim(Y)[2]

# Add interactions #
X <- abind::abind(X, array(NA, dim = c(dim(X)[1:2], 1)))
X.nams <- c(X.nams, "Max_Shrub_Height_5mXShrub_All_5m")
dimnames(X)[[3]] <- X.nams
X[,,"Max_Shrub_Height_5mXShrub_All_5m"] <- X[,,"Max_Shrub_Height_5m"]*X[,,"Shrub_All_5m"]

rm(Veg, Veg.z, Veg2.z, VegCV, VegCV.z, X.add, lin.only, ind.lin)
