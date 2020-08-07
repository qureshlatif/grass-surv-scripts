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
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
nsims.samp <- sample(nsims, 5000)
Site_Season <- str_c(data.spp$Covs$Site, data.spp$Covs$Season, sep = "_")
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
X.sum <- X.sum %>% array(dim = c(dim(X.sum), length(nsims.samp))) %>% aperm(c(4, 1:3))
npar <- dim(mod$sims.concat)[1]
B <- mod$sims.concat[1:(npar-2),nsims.samp] %>% array(dim = dim(X.sum)[c(4, 1, 2, 3)]) %>% aperm(c(2, 3, 4, 1))
DSR <- expit(apply(B * X.sum, c(1, 2, 3), sum))

st <- 1:(J-89)
end <- 90:J
PSR <- apply(DSR, c(1, 2), function(x) {
  v <- numeric(length = length(st))
  for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
  return(mean(v))
})
dat.plot <- SiteSeasons %>%
  mutate(PSR.md = apply(PSR, 2, median),
         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95))) %>%
  left_join(
    data.spp$Covs %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  )

## Compare with Site X Season estimates ###

modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason", "_", spp))
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
source(str_c("grass-surv-scripts/Data_processing_JAGS_", spp, ".r"))
X.sum <- X.sum[,,X.nams]
SeasonRef <- data.spp$Covs %>% select(Season, SeasonInd) %>% unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% select(Site, SiteInd) %>% unique %>% arrange(SiteInd)
dat.plot <- dat.plot %>%
  mutate(PSR.est = NA,
         PSR.est.lo = NA,
         PSR.est.hi = NA)

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + apply(t(t(modSS$sims.list$B) * X.sum[row.ind,d,]), 1, sum))
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

p.BAIS.pred <- ggplot(dat.plot, aes(x = Season, y = PSR.md, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.lo, ymax = PSR.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.md), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  ylab("Predicted survival over 90 days") + xlab("Winter (Nov-Mar)") +
  annotate("text", x = 1, y = 0.75, label = "Regularized covariate model", hjust = 0)

p.BAIS.est <- ggplot(dat.plot, aes(x = Season, y = PSR.est, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.est.lo, ymax = PSR.est.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.est), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  ylab("Proportion survived over 90 days") + xlab("Winter (Nov-Mar)") +
  annotate("text", x = 1, y = 0.75, label = "Site X Season random effect model", hjust = 0)

#######################
# Grasshopper Sparrow #
#######################

## Plot predictions ##

spp <- "GRSP" # GRSP or GRSP
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
nsims.samp <- sample(nsims, 5000)
Site_Season <- str_c(data.spp$Covs$Site, data.spp$Covs$Season, sep = "_")
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
X.sum <- X.sum %>% array(dim = c(dim(X.sum), length(nsims.samp))) %>% aperm(c(4, 1:3))
npar <- dim(mod$sims.concat)[1]
B <- mod$sims.concat[1:(npar-2),nsims.samp] %>% array(dim = dim(X.sum)[c(4, 1, 2, 3)]) %>% aperm(c(2, 3, 4, 1))
DSR <- expit(apply(B * X.sum, c(1, 2, 3), sum))

st <- 1:(J-89)
end <- 90:J
PSR <- apply(DSR, c(1, 2), function(x) {
  v <- numeric(length = length(st))
  for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
  return(mean(v))
})
dat.plot <- SiteSeasons %>%
  mutate(PSR.md = apply(PSR, 2, median),
         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95))) %>%
  left_join(
    data.spp$Covs %>% select(Site, Season) %>%
      dplyr::group_by(Site, Season) %>%
      summarise(n = n()),
    by = c("Site", "Season")
  )

## Compare with Site X Season estimates ###

modSS <- loadObject(str_c("mod_CJSRL_SiteXSeason", "_", spp))
X.sum <- X %>% apply(c(2, 3), function(x) tapply(x, Site_Season, mean))
source(str_c("grass-surv-scripts/Data_processing_JAGS_", spp, ".r"))
X.sum <- X.sum[,,X.nams]
SeasonRef <- data.spp$Covs %>% select(Season, SeasonInd) %>% unique %>% arrange(SeasonInd)
SiteRef <- data.spp$Covs %>% select(Site, SiteInd) %>% unique %>% arrange(SiteInd)
dat.plot <- dat.plot %>%
  mutate(PSR.est = NA,
         PSR.est.lo = NA,
         PSR.est.hi = NA)

for(j in 1:nrow(SiteRef)) for(t in 1:nrow(SeasonRef)) {
  row.ind <- which(dat.plot$Season == SeasonRef$Season[t] & dat.plot$Site == SiteRef$Site[j])
  if(length(row.ind) == 1) {
    B0 <- modSS$sims.list$B0[, SiteRef$SiteInd[j], SeasonRef$SeasonInd[t]]
    DSR <- array(NA, dim = c(length(B0), J))
    for(d in 1:J) DSR[,d] <- expit(B0 + apply(t(t(modSS$sims.list$B) * X.sum[row.ind,d,]), 1, sum))
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

p.GRSP.pred <- ggplot(dat.plot, aes(x = Season, y = PSR.md, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.lo, ymax = PSR.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.md), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  ylab("Predicted survival over 90 days") + xlab("Winter (Nov-Mar)") +
  annotate("text", x = 1, y = 0.75, label = "Regularized covariate model", hjust = 0)

p.GRSP.est <- ggplot(dat.plot, aes(x = Season, y = PSR.est, fill = Site, shape = Site)) +
  geom_errorbar(aes(ymin = PSR.est.lo, ymax = PSR.est.hi), width = 0.2, position = position_dodge(0.8)) +
  geom_label(aes(label = n, x = Season, y = PSR.est), position = position_dodge(0.8), angle = 90) +
  scale_discrete_manual(aesthetics = "fill", labels = c("Cuza", "Janos", "Marfa", "Vaco"), values = c("#E69F00", "#56B4E9", "#009E73", "#F0E442")) +
  ylab("Proportion survived over 90 days") + xlab("Winter (Nov-Mar)") +
  annotate("text", x = 1, y = 0.75, label = "Site X Season random effect model", hjust = 0)

p <- ggdraw() +
  draw_plot(p.BAIS.pred, x = 0,   y = 0.475, width = 0.5,  height = 0.475) +
  draw_plot(p.GRSP.pred, x = 0.5, y = 0.475, width = 0.5,  height = 0.475) +
  draw_plot(p.BAIS.est,  x = 0,   y = 0,     width = 0.5,  height = 0.475) +
  draw_plot(p.GRSP.est,  x = 0.5, y = 0,     width = 0.5,  height = 0.475) +
  draw_plot_label(c("Baird's Sparrow", "Grasshopper Sparrow"),
                  x = c(0.15, 0.65), y = c(0.97, 0.97), size = c(15, 15))

save_plot("Figure_SiteXYear_Rates.tiff", p, ncol = 3.8, nrow = 3.8, dpi = 600)
