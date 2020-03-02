library(jagsUI)
library(stringr)
library(dplyr)
library(R.utils)
library(QSLpersonal)
library(ggplot2)
library(cowplot)
theme_set(theme_cowplot())

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

#________ Script inputs________#
spp <- "GRSP" # BAIS or GRSP
mod <- loadObject(str_c("mod_prototype_", spp))
DS.lbnd <- 0.96
#______________________________#

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

# Daily survival
hist(expit(mod$sims.list$B0.mean))
median(expit(mod$sims.list$B0.mean))
quantile(expit(mod$sims.list$B0.mean), prob = 0.025, type = 8)
quantile(expit(mod$sims.list$B0.mean), prob = 0.975, type = 8)

# Mean seasonal survival rate #
daysMonitored <- which(apply(data.spp$ymat, 2, function(x) any(!is.na(x))))
SeasLength <- max(daysMonitored) - min(daysMonitored)
dailySurv <- expit(mod$sims.list$B0.mean)
seasonSurv <- dailySurv^SeasLength
median(seasonSurv)
quantile(seasonSurv, prob = 0.025, type = 8)
quantile(seasonSurv, prob = 0.975, type = 8)

## Covariates ##
# Day of season #
DOS.x <- t(matrix(1:nDOS, nrow = nDOS, ncol = nBird))
x <- seq(min(DOS.x[which(!is.na(ymat))]), max(DOS.x[which(!is.na(ymat))]))
z <- (x - mean(DOS.x[which(!is.na(ymat))])) / sd(DOS.x[which(!is.na(ymat))])
DS <- expit(mod$sims.list$B0.mean + mod$sims.list$B.DOS %*% t(z) + mod$sims.list$B.DOS2 %*% t(z)^2)
dat.plt <- data.frame(x = x,
                      y.md = apply(DS, 2, median),
                      y.lo = apply(DS, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(DS, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.DOS <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(DS.lbnd, 1) +
  xlab("Day of season") + ylab(str_c("Daily survival (",spp,")"))

# Days since transmitter deployment #
DaysSinceDepl.x <- 0:20
DS <- matrix(NA, nrow = mod$mcmc.info$n.samples, ncol = length(DaysSinceDepl.x))
for(i in 1:length(DaysSinceDepl.x)) DS[, i] <-
  expit(mod$sims.list$B0.mean + mod$sims.list$B.trans * (DaysSinceDepl.x[i] < mod$sims.list$P.trans))
dat.plt <- data.frame(x = DaysSinceDepl.x,
                      y.md = apply(DS, 2, median),
                      y.lo = apply(DS, 2, function(x) quantile(x, prob = 0.025, type = 8)),
                      y.hi = apply(DS, 2, function(x) quantile(x, prob = 0.975, type = 8)))
p.DaysSinceDepl <- ggplot(dat.plt, aes(x = x, y = y.md)) +
  geom_ribbon(aes(ymin = y.lo, ymax = y.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(DS.lbnd, 1) +
  xlab("Days since deployment") + ylab(str_c("Daily survival (",spp,")"))

p <- ggdraw() +
  draw_plot(p.DOS, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p.DaysSinceDepl, x = 0.5, y = 0, width = 0.5, height = 1)

save_plot(str_c(str_c("figure_prototype_",spp,".tiff")), p, ncol = 2, nrow = 1, dpi = 200)
