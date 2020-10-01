library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"

###################
# Baird's Sparrow #
###################

## Plot predictions ##

spp <- "BAIS" # BAIS or GRSP
source(str_c(scripts.loc, "Data_processing_BigCheese.R"))
st <- 1:(J-89)
end <- 90:J
source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))
dimnames(X)[[3]] <- X.nams
modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason_Transmitter", "_", spp))
Site_Season <- str_c(data.spp$Covs$Site, data.spp$Covs$Season, sep = "_")
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
X.sum <- X.sum[,,X.nams]
SeasonRef <- data.spp$Covs %>% select(Season, SeasonInd) %>% unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% select(Site, SiteInd) %>% unique %>% arrange(SiteInd)

dat.plot <- SiteSeasons %>%
  left_join(
    data.spp$Covs %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  ) %>%
  mutate(PSR.est = NA,
         PSR.est.lo = NA,
         PSR.est.hi = NA)

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + apply(t(t(modSS$sims.list$B[,1:2]) * X.sum[row.ind,d,1:2]), 1, sum))
    PSR <- apply(DSR, 1, function(x) {
      v <- numeric(length = length(st))
      for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
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
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
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
modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason_Transmitter", "_", spp))
Site_Season <- str_c(data.spp$Covs$Site, data.spp$Covs$Season, sep = "_")
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
X.sum <- X.sum[,,X.nams]
SeasonRef <- data.spp$Covs %>% select(Season, SeasonInd) %>% unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% select(Site, SiteInd) %>% unique %>% arrange(SiteInd)

dat.plot <- SiteSeasons %>%
  left_join(
    data.spp$Covs %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  ) %>%
  mutate(PSR.est = NA,
         PSR.est.lo = NA,
         PSR.est.hi = NA)

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + apply(t(t(modSS$sims.list$B[,1:2]) * X.sum[row.ind,d,1:2]), 1, sum))
    PSR <- apply(DSR, 1, function(x) {
      v <- numeric(length = length(st))
      for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
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
  draw_plot(p.BAIS, x = 0.05,  y = 0, width = 0.475, height = 0.95) +
  draw_plot(p.GRSP, x = 0.525, y = 0, width = 0.475, height = 0.95) +
  draw_plot_label(c("Baird's Sparrow", "Grasshopper Sparrow", "Survival over 90 days"),
                  x = c(0.15, 0.65, 0), y = c(0.97, 0.97, 0.5), size = c(15, 15, 25),
                  hjust = c(0.5, 0.5, 0.5), angle = c(0, 0, 90))

save_plot("Figure_SiteXYear_Rates_abridged.jpg", p, ncol = 3.8, nrow = 1.5, dpi = 600)

