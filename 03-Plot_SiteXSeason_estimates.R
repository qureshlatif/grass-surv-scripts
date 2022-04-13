library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")
load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"
chop.init <- 8
chop.CUZA1516 <- F

###################
# Baird's Sparrow #
###################

## Plot siteXseason estimates after accounting for covariate effects (i.e., with mean covariate effects averaged across sites and seasons) ##

spp <- "BAIS" # BAIS or GRSP
source(str_c(scripts.loc, "Data_processing_BigCheese.R"))
st <- 1:(J-89)
end <- 90:J
source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))
dimnames(X)[[3]] <- X.nams
modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason_covs", "_", spp))

#_Calculate offset (run once and cache)_#
nind <- dim(X)[1]
nday <- dim(X)[2]
nsim <- dim(modSS$sims.list$B)[1]
BX <- matrix(0, nrow = nsim, ncol = nday)
B <- modSS$sims.list$B
B <- B %>% array(dim = c(dim(B), dim(X)[2])) %>% aperm(c(1, 3, 2))
for(i in 1:nind) {
  x <- X[i,,]
  x.arr <- x %>% array(dim = c(dim(x), nsim)) %>% aperm(c(3, 1, 2))
  BX <- BX + apply(B * x.arr, c(1, 2), sum)
}
BX <- BX / nind
saveObject(BX, str_c("BX_offset_SiteSeasonPlots_", spp))
#_______________________________________#
BX <- loadObject(str_c("BX_offset_SiteSeasonPlots_", spp))

SeasonRef <- data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Season, SeasonInd) %>%
  unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Site, SiteInd) %>%
  unique %>% arrange(SiteInd)

dat.plot <- SiteSeasons %>%
  left_join(
    data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  ) %>%
  mutate(PSR.est = as.numeric(NA),
         PSR.est.lo = as.numeric(NA),
         PSR.est.hi = as.numeric(NA))

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + BX[,d])
    PSR <- apply(DSR, 1, function(x) {
      v <- numeric(length = length(st))
      for(k in 1:length(st)) v[k] <- prod(x[st[k]:end[k]])
      return(mean(v))
    })
    dat.plot[row.ind, "PSR.est"] <- median(PSR)
    dat.plot[row.ind, "PSR.est.lo"] <- quantile(PSR, probs = 0.05, type = 8)
    dat.plot[row.ind, "PSR.est.hi"] <- quantile(PSR, probs = 0.95, type = 8)
  }
}

dat.plot <- dat.plot %>%
  mutate(Site = as.factor(factor(Site)),
         Season = as.factor(Season))

p.BAIS <- ggplot(dat.plot, aes(x = Season, y = PSR.est, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.est.lo, ymax = PSR.est.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.est), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  guides(fill = FALSE) +
  ylim(0,1) +
  ylab(NULL) + xlab("Winter (Nov-Mar)")

#######################
# Grasshopper Sparrow #
#######################

## Plot predictions ##

spp <- "GRSP" # BAIS or GRSP
source(str_c(scripts.loc, "Data_processing_BigCheese.R"))
st <- 1:(J-89)
end <- 90:J
source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))
dimnames(X)[[3]] <- X.nams
modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason_covs", "_", spp))

#_Calculate offset (run once and cache)_#
nind <- dim(X)[1]
nday <- dim(X)[2]
nsim <- dim(modSS$sims.list$B)[1]
BX <- matrix(0, nrow = nsim, ncol = nday)
B <- modSS$sims.list$B
B <- B %>% array(dim = c(dim(B), dim(X)[2])) %>% aperm(c(1, 3, 2))
for(i in 1:nind) {
  x <- X[i,,]
  x.arr <- x %>% array(dim = c(dim(x), nsim)) %>% aperm(c(3, 1, 2))
  BX <- BX + apply(B * x.arr, c(1, 2), sum)
}
BX <- BX / nind
saveObject(BX, str_c("BX_offset_SiteSeasonPlots_", spp))
#_______________________________________#
BX <- loadObject(str_c("BX_offset_SiteSeasonPlots_", spp))

SeasonRef <- data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Season, SeasonInd) %>%
  unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Site, SiteInd) %>%
  unique %>% arrange(SiteInd)

dat.plot <- SiteSeasons %>%
  left_join(
    data.spp$Covs %>% slice(-ind.rows.chop) %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  ) %>%
  mutate(PSR.est = as.numeric(NA),
         PSR.est.lo = as.numeric(NA),
         PSR.est.hi = as.numeric(NA))

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + BX[,d])
    PSR <- apply(DSR, 1, function(x) {
      v <- numeric(length = length(st))
      for(k in 1:length(st)) v[k] <- prod(x[st[k]:end[k]])
      return(mean(v))
    })
    dat.plot[row.ind, "PSR.est"] <- median(PSR)
    dat.plot[row.ind, "PSR.est.lo"] <- quantile(PSR, probs = 0.05, type = 8)
    dat.plot[row.ind, "PSR.est.hi"] <- quantile(PSR, probs = 0.95, type = 8)
  }
}

dat.plot <- dat.plot %>%
  mutate(Site = as.factor(factor(Site)),
         Season = as.factor(Season))

p.GRSP <- ggplot(dat.plot, aes(x = Season, y = PSR.est, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.est.lo, ymax = PSR.est.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.est), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  ylim(0,1) +
  ylab(NULL) + xlab("Winter (Nov-Mar)")

p <- ggdraw() +
  draw_plot(p.BAIS, x = 0.05, y = 0, width = 0.44, height = 0.95) +
  draw_plot(p.GRSP, x = 0.49, y = 0, width = 0.51, height = 0.95) +
  draw_plot_label(c("Baird's Sparrow", "Grasshopper Sparrow", "Survival over 90 days"),
                  x = c(0.15, 0.65, 0), y = c(0.97, 0.97, 0.5), size = c(15, 15, 25),
                  hjust = c(0.5, 0.5, 0.5), angle = c(0, 0, 90))

save_plot("Figure_SiteXYear_Rates.jpg", p, ncol = 2.5, nrow = 2, dpi = 600)

