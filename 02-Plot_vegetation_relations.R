library(jagsUI)
library(stringr)
library(dplyr)
library(R.utils)
library(QSLpersonal)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Baird's Sparrow relations________#
spp <- "BAIS" # BAIS or GRSP
mod <- loadObject(str_c("mod_prototype_Veg_", spp))

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

# Compile DOS values for entire season #
DOS.x <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
DOS.mn <- mean(DOS.x[which(!is.na(ymat))])
DOS.sd <- sd(DOS.x[which(!is.na(ymat))])
DOS.x <- seq(min(DOS.x[which(!is.na(ymat))]), max(DOS.x[which(!is.na(ymat))]))
DOS.z <- (DOS.x - DOS.mn) / DOS.sd
DOS.z <- DOS.z %>% array(dim = c(length(DOS.z), mod$mcmc.info$n.samples, 20)) %>% aperm(c(2, 1, 3))

# Plots for forbe height, shrub cover, tumbleweed height, and variability in shrub cover #
  # Forbe height #
var <- data.spp$Covs$hierba_ht
min.val <- min(var, na.rm = T)
max.val <- 60
B <- mod$sims.list$B.hierba_ht

B <- B %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS <- mod$sims.list$B.DOS %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS2 <- mod$sims.list$B.DOS2 %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
x <- seq(min.val, max.val, length.out = 20)
z <- (x - mean(var, na.rm = T)) / sd(var, na.rm = T)
z <- z %>% array(dim = c(length(z), length(DOS.x), mod$mcmc.info$n.samples)) %>% aperm(c(3, 2, 1))
B0 <- mod$sims.list$B0.mean %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
DS <- expit(B0 + B * z + B.DOS * DOS.z + B.DOS2 * DOS.z^2)
S <- apply(DS, c(1, 3), prod)
dat.plt <- data.frame(x = x,
                      y.md = apply(S, 2, median),
                      y.lo = apply(S, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(S, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.hierba_ht <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Forbe height (cm)") + ylab(NULL)

  # Shrub cover #
var <- data.spp$Covs$arbusto
min.val <- min(var, na.rm = T)
max.val <- 7
B <- mod$sims.list$B.arbusto

B <- B %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS <- mod$sims.list$B.DOS %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS2 <- mod$sims.list$B.DOS2 %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
x <- seq(min.val, max.val, length.out = 20)
z <- (x - mean(var, na.rm = T)) / sd(var, na.rm = T)
z <- z %>% array(dim = c(length(z), length(DOS.x), mod$mcmc.info$n.samples)) %>% aperm(c(3, 2, 1))
B0 <- mod$sims.list$B0.mean %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
DS <- expit(B0 + B * z + B.DOS * DOS.z + B.DOS2 * DOS.z^2)
S <- apply(DS, c(1, 3), prod)
dat.plt <- data.frame(x = x,
                      y.md = apply(S, 2, median),
                      y.lo = apply(S, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(S, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.arbusto <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub cover (%)") + ylab(NULL)

  # Tumbleweed height #
var <- data.spp$Covs$salsola_ht
min.val <- min(var, na.rm = T)
max.val <- max(var, na.rm = T)
B <- mod$sims.list$B.salsola_ht

B <- B %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS <- mod$sims.list$B.DOS %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS2 <- mod$sims.list$B.DOS2 %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
x <- seq(min.val, max.val, length.out = 20)
z <- (x - mean(var, na.rm = T)) / sd(var, na.rm = T)
z <- z %>% array(dim = c(length(z), length(DOS.x), mod$mcmc.info$n.samples)) %>% aperm(c(3, 2, 1))
B0 <- mod$sims.list$B0.mean %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
DS <- expit(B0 + B * z + B.DOS * DOS.z + B.DOS2 * DOS.z^2)
S <- apply(DS, c(1, 3), prod)
dat.plt <- data.frame(x = x,
                      y.md = apply(S, 2, median),
                      y.lo = apply(S, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(S, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.salsola_ht <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Tumbleweed height (cm)") + ylab(NULL)

  # Variability in shrub cover #
var <- data.spp$Covs$arbusto_cv
min.val <- min(var, na.rm = T)
max.val <- 6
B <- mod$sims.list$B.arbusto_cv

B <- B %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS <- mod$sims.list$B.DOS %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
B.DOS2 <- mod$sims.list$B.DOS2 %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
x <- seq(min.val, max.val, length.out = 20)
z <- (x - mean(var, na.rm = T)) / sd(var, na.rm = T)
z <- z %>% array(dim = c(length(z), length(DOS.x), mod$mcmc.info$n.samples)) %>% aperm(c(3, 2, 1))
B0 <- mod$sims.list$B0.mean %>% array(dim = c(mod$mcmc.info$n.samples, length(DOS.x), 20))
DS <- expit(B0 + B * z + B.DOS * DOS.z + B.DOS2 * DOS.z^2)
S <- apply(DS, c(1, 3), prod)
dat.plt <- data.frame(x = x,
                      y.md = apply(S, 2, median),
                      y.lo = apply(S, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(S, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.arbusto_cv <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.hierba_ht, x = 0.05, y = 0.5, width = 0.425, height = 0.5) +
  draw_plot(p.salsola_ht, x = 0.525, y = 0.5, width = 0.425, height = 0.5) +
  draw_plot(p.arbusto, x = 0.05, y = 0, width = 0.425, height = 0.5) +
  draw_plot(p.arbusto_cv, x = 0.525, y = 0, width = 0.425, height = 0.5) +
  draw_plot_label("Probability of survival", x = 0, y = 0.3, angle = 90)

save_plot("Survival_relations_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

#__________________________________________#
