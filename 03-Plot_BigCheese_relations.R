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

load("Big_cheese_BAIS_cache.RData")

# Plots for time-varying covariates #
p.temp.prec7.DSR <- ggplot(dat.plot.temp.prec7, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab(expression('Mean temp prior 7 days ('*degree*C*')')) + ylab("Daily survival probability")

p <- ggdraw() +
  draw_plot(p.temp.prec7.DSR, x = 0, y = 0, width = 1, height = 0.95) +
  draw_plot_label("Baird's Sparrow", x = 0.35, y = 1, hjust = 0, size = 20)
save_plot("Figure_BigCheese_BAIS_time-varying.tiff", p, ncol = 1, nrow = 1, dpi = 200)

# Plots for seasonally fixed covariates #
p.hierbas.PSR <- ggplot(dat.plot.hierbas, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Forb cover 5m (%)") + ylab(NULL)

p.pastos.PSR <- ggplot(dat.plot.pastos, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Grass cover 5m (%)") + ylab(NULL)

p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height 5m (m)") + ylab(NULL)

p.Mean_Shrub_Height_5m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height 5m") + ylab(NULL)

p.Mean_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height 50m") + ylab(NULL)

p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Bird density (per '*km^2*')')) + ylab(NULL)

p.LOSH.PSR <- ggplot(dat.plot.LOSH, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Shrike density (per '*km^2*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.hierbas.PSR,                  x = 0.0500000, y = 0.6333333, width = 0.3166667, height = 0.3166667) +
  draw_plot(p.pastos.PSR,                   x = 0.3666667, y = 0.6333333, width = 0.3166667, height = 0.3166667) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.6833333, y = 0.6333333, width = 0.3166667, height = 0.3166667) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.0500000, y = 0.3166667, width = 0.3166667, height = 0.3166667) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.3666667, y = 0.3166667, width = 0.3166667, height = 0.3166667) +
  draw_plot(p.prey.PSR,                     x = 0.0500000, y = 0,         width = 0.3166667, height = 0.3166667) +
  draw_plot(p.LOSH.PSR,                     x = 0.3666667, y = 0,         width = 0.3166667, height = 0.3166667) +
  draw_plot_label(c("Survival over 90 days", "Baird's Sparrow"),
                  x = c(0, 0.4), y = c(0.37, 1), angle = c(90, 0), hjust = c(0, 0), size = c(15, 20))

save_plot("Figure_BigCheese_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

#__________________________________________#

#######################
# Grasshopper Sparrow #
#######################

rm(list = ls())
load("Big_cheese_GRSP_cache.RData")

# Plot time-varying covariate relationship #
p.DOS.DSR <- ggplot(dat.plot.DOS, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = c(20, 50, 81, 109), labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1")) +
  xlab("Day of season") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.DOS.DSR, x = 0, y = 0, width = 1, height = 0.95) +
  draw_plot_label("Grasshopper Sparrow", x = 0.35, y = 1, hjust = 0, size = 20)
save_plot("Figure_BigCheese_GRSP_time-varying.tiff", p, ncol = 1, nrow = 1, dpi = 200)

# Plot seasonally fixed relationships #
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

p.Shrub_All_5m.PSR <- ggplot(dat.plot.Shrub_All_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub cover 5m (%)") + ylab(NULL)

p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height 5m (m)") + ylab(NULL)

p.Shrub_All_500m_CV.PSR <- ggplot(dat.plot.Shrub_All_500m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover 500m") + ylab(NULL)

p.Mean_Shrub_Height_5m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height 5m") + ylab(NULL)

p.Mean_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height 50m") + ylab(NULL)

p.peso.PSR <- ggplot(dat.plot.peso, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Body mass (g)") + ylab(NULL)

p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Bird density (per '*km^2*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.otra.PSR,                     x = 0.05, y = 0.475, width = 0.19,  height = 0.475) +
  draw_plot(p.hierbas_cv.PSR,               x = 0.24, y = 0.475, width = 0.19,  height = 0.475) +
  draw_plot(p.Shrub_All_5m.PSR,             x = 0.43, y = 0.475, width = 0.19,  height = 0.475) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.62, y = 0.475, width = 0.19,  height = 0.475) +
  draw_plot(p.Shrub_All_500m_CV.PSR,        x = 0.05, y = 0,     width = 0.19,  height = 0.475) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.24, y = 0,     width = 0.19,  height = 0.475) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.43, y = 0,     width = 0.19,  height = 0.475) +
  draw_plot(p.peso.PSR,                     x = 0.62, y = 0,     width = 0.19,  height = 0.475) +
  draw_plot(p.prey.PSR,                     x = 0.81, y = 0,     width = 0.185, height = 0.475) +
  draw_plot_label(c("Survival over 90 days", "Grasshopper Sparrow"),
                  x = c(0, 0.4), y = c(0.3, 1), angle = c(90, 0), hjust = c(0, 0), size = c(20, 20))

save_plot("Figure_BigCheese_GRSP.tiff", p, ncol = 3, nrow = 1.5, dpi = 200)

#__________________________________________#