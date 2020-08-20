library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
scripts.loc <- "grass-surv-scripts/"

load("ShrubSpp_cache.RData")
colors.spp <- c("saddlebrown", "#009E73")

## Plots for time-varying covariates ##
# Mean temperature prior 7 days #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.temp.prec7.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.temp.prec7.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  #ylim(0.94, 1) +
  labs(color = "Species", fill = "Species", linetype = "Species") +
  xlab(expression('Mean temp prior 7 days ('*degree*C*')')) + ylab("Survival over 90 days")

save_plot("Figure_ShrubSpp_time-varying.jpg", p, ncol = 1.3, nrow = 1, dpi = 600)
#save_plot("Figure_ShrubSpp-varying.tiff", p, ncol = 3, nrow = 1, dpi = 600)

# Plots for seasonally fixed covariates #
  # Grass cover #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.pastos.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.pastos.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.pastos.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Grass cover 5m (%)") + ylab(NULL)

  # Juniper 5m #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Juniper_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Juniper_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100) # convert to percent
p.Juniper_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  labs(color = "Species", fill = "Species", linetype = "Species") +
  xlab("Juniper cover 5m (%)") + ylab(NULL)

  # Juniper 500m #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Juniper_500m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Juniper_500m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100) # convert to percent
p.Juniper_500m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Juniper cover 500m (%)") + ylab(NULL)

  # Yucca 5m #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Yucca_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Yucca_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100) # convert to percent
p.Yucca_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Yucca cover 5m (%)") + ylab(NULL)

  # CV forb cover #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.hierbas_cv.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.hierbas_cv.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.hierbas_cv.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV forb cover 5m") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.pastos.PSR,       x = 0.05,      y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.Juniper_5m.PSR,   x = 0.3666667, y = 0.5, width = 0.4,       height = 0.5) +
  draw_plot(p.Juniper_500m.PSR, x = 0.05,      y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.Yucca_5m.PSR,     x = 0.3666667, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.hierbas_cv.PSR,   x = 0.6833333, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot_label("Survival over 90 days", x = 0, y = 0.5, angle = 90, hjust = 0.5)

save_plot(str_c("Figure_PSR_", mod.nam, ".jpg"), p, ncol = 3, nrow = 2, dpi = 200)
