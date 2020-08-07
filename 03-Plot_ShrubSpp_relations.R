library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
scripts.loc <- "grass-surv-scripts/"

###################
# Baird's Sparrow #
###################

load("ShrubSpp_BAIS_cache.RData")

# Plots for time-varying covariates #
p.DOS.DSR <- ggplot(dat.plot.DOS, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(20, 50, 81, 109), labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1")) +
  xlab("Day of season") + ylab("Daily survival probability")

p.temp.prec7.DSR <- ggplot(dat.plot.temp.prec7, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab(expression('Mean temp prior 7 days ('*degree*C*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.DOS.DSR,        x = 0, y = 0, width = 0.525, height = 0.95) +
  draw_plot(p.temp.prec7.DSR, x = 0.525, y = 0, width = 0.475, height = 0.95) +
  draw_plot_label("Baird's Sparrow", x = 0.45, y = 1, hjust = 0, size = 15)
save_plot("Figure_ShrubSpp_BAIS_time-varying.tiff", p, ncol = 2, nrow = 1, dpi = 200)

# Plots for seasonally fixed covariates #
p.pastos.PSR <- ggplot(dat.plot.pastos, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Grass cover 5m (%)") + ylab(NULL)

p.Juniper_5m.PSR <- ggplot(dat.plot.Juniper_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Juniper cover 5m (%)") + ylab(NULL)

p.Yucca_5m.PSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Yucca cover 5m (%)") + ylab(NULL)

p.pasto_ht_cv.PSR <- ggplot(dat.plot.pasto_ht_cv, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV grass height 5m") + ylab(NULL)

p.Shrub_All_5m_CV.PSR <- ggplot(dat.plot.Shrub_All_5m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover 5m") + ylab(NULL)

p.Max_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Max_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV max shrub height 50m") + ylab(NULL)

p.Max_Shrub_Height_500m_CV.PSR <- ggplot(dat.plot.Max_Shrub_Height_500m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV max shrub height 500m") + ylab(NULL)

p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Bird density (per '*km^2*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.pastos.PSR,                   x = 0.05,   y = 0.475, width = 0.2375, height = 0.475) +
  draw_plot(p.Juniper_5m.PSR,               x = 0.2875, y = 0.475, width = 0.2375, height = 0.475) +
  draw_plot(p.Yucca_5m.PSR,                 x = 0.5250, y = 0.475, width = 0.2375, height = 0.475) +
  draw_plot(p.pasto_ht_cv.PSR,              x = 0.7625, y = 0.475, width = 0.2375, height = 0.475) +
  draw_plot(p.Shrub_All_5m_CV.PSR,          x = 0.05,   y = 0,     width = 0.2375, height = 0.475) +
  draw_plot(p.Max_Shrub_Height_50m_CV.PSR,  x = 0.2875, y = 0,     width = 0.2375, height = 0.475) +
  draw_plot(p.Max_Shrub_Height_500m_CV.PSR, x = 0.5250, y = 0,     width = 0.2375, height = 0.475) +
  draw_plot(p.prey.PSR,                     x = 0.7625, y = 0,     width = 0.2375, height = 0.475) +
  draw_plot_label(c("Survival over 90 days", "Baird's Sparrow"),
                  x = c(0, 0.5), y = c(0.37, 1), angle = c(90, 0), hjust = c(0, 0), size = c(20, 20))

save_plot(str_c("Figure_PSR_", mod.nam, "_BAIS.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

#__________________________________________#

#######################
# Grasshopper Sparrow #
#######################

rm(list = ls())
load("ShrubSpp_GRSP_cache.RData")

# Plots for individual covariates #
p.otra.PSR <- ggplot(dat.plot.otra, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Other cover 5m (%)") + ylab(NULL)

p.hierbas_cv.PSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV Forb cover 5m") + ylab(NULL)

p.pasto_ht_cv.PSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV grass height 5m") + ylab(NULL)

p.Mesquite_5m.PSR <- ggplot(dat.plot.Mesquite_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Mesquite cover 5m (%)") + ylab(NULL)

p.Juniper_500m.PSR <- ggplot(dat.plot.Juniper_500m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Juniper cover 500m (%)") + ylab(NULL)

p.Yucca_5m.PSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Yucca cover 5m (%)") + ylab(NULL)

p.Shrub_All_5m_CV.PSR <- ggplot(dat.plot.Shrub_All_5m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover 5m") + ylab(NULL)

p.Shrub_All_50m_CV.PSR <- ggplot(dat.plot.Shrub_All_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover 50m") + ylab(NULL)

p.Max_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Max_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV max shrub height 50m") + ylab(NULL)

p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Bird density (per '*km^2*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.otra.PSR,                     x = 0.05, y = 0.475, width = 0.19, height = 0.475) +
  draw_plot(p.hierbas_cv.PSR,               x = 0.24, y = 0.475, width = 0.19, height = 0.475) +
  draw_plot(p.pasto_ht_cv.PSR,              x = 0.43, y = 0.475, width = 0.19, height = 0.475) +
  draw_plot(p.Mesquite_5m.PSR,              x = 0.62, y = 0.475, width = 0.19, height = 0.475) +
  draw_plot(p.Juniper_500m.PSR,             x = 0.81, y = 0.475, width = 0.19, height = 0.475) +
  draw_plot(p.Yucca_5m.PSR,                 x = 0.05, y = 0,     width = 0.19, height = 0.475) +
  draw_plot(p.Shrub_All_5m_CV.PSR,          x = 0.24, y = 0,     width = 0.19, height = 0.475) +
  draw_plot(p.Shrub_All_50m_CV.PSR,         x = 0.43, y = 0,     width = 0.19, height = 0.475) +
  draw_plot(p.Max_Shrub_Height_50m_CV.PSR,  x = 0.62, y = 0,     width = 0.19, height = 0.475) +
  draw_plot(p.prey.PSR,                     x = 0.81, y = 0,     width = 0.19, height = 0.475) +
  draw_plot_label(c("Survival over 90 days", "Grasshopper Sparrow"),
                  x = c(0, 0.4), y = c(0.37, 1), angle = c(90, 0), hjust = c(0, 0), size = c(20, 20))

save_plot(str_c("Figure_PSR_",mod.nam,"_GRSP.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

#__________________________________________#
