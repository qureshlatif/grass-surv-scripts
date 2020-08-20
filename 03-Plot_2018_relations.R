library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
scripts.loc <- "grass-surv-scripts/"

load("2018_cache.RData")
colors.spp <- c("saddlebrown", "#009E73")

# Plots for seasonally fixed covariates #
  # Grass cover 50m #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Grass_50m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Grass_50m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100)
p.Grass_50m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.35) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Grass cover 50m (%)") + ylab(NULL)

  # CV Max Shrub Height 500m #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Max_Shrub_Height_500m_CV.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Max_Shrub_Height_500m_CV.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.Max_Shrub_Height_500m_CV.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.35) +
  labs(color = "Species", fill = "Species", linetype = "Species") +
  xlab("CV Max Shrub Height 500m") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.Grass_50m.PSR,                x = 0.05, y = 0, width = 0.42, height = 1) +
  draw_plot(p.Max_Shrub_Height_500m_CV.PSR, x = 0.48,  y = 0, width = 0.53,  height = 1) +
  draw_plot_label("Survival over 90 days", x = 0, y = 0.5, angle = 90, hjust = 0.5)

save_plot(str_c("Figure_PSR_", mod.nam, ".jpg"), p, ncol = 2.3, nrow = 1, dpi = 200)
