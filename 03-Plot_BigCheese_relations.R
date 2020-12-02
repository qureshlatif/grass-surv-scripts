library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
scripts.loc <- "grass-surv-scripts/"

#########################
# Both species together #
#########################

load(str_c("Big_cheese_cache.RData"))
colors.spp <- c("saddlebrown", "#009E73")

## Plots for time-varying covariates ##
# Day of season #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.DOS.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.DOS.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.DOS <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  scale_x_continuous(breaks = c(20, 50, 81, 109), labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1")) +
  ylim(0.93, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Day of season") + ylab("Daily survival")

# Minimum temperature #
supp <- c(F, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.temp.min.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.temp.min.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.temp.min <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0.93, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab(expression('Min temp ('*degree*C*')')) + ylab(NULL)

# Mean temperature prior 7 days #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.temp.prec7.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.temp.prec7.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.temp.prec7 <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp),
              alpha = 0.3, show.legend = F) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0.93, 1) +
  labs(color = "Species") +
  theme(legend.position = c(1,0), legend.justification = c(1,0)) +
  xlab(expression('Mean temp prior 7 days ('*degree*C*')')) + ylab(NULL)

# Put everything together
# p <- ggdraw() +
#   draw_plot(p.DOS,        x = 0,         y = 0, width = 0.3466667, height = 1) +
#   draw_plot(p.temp.min,   x = 0.3666667, y = 0, width = 0.3266667, height = 1) +
#   draw_plot(p.temp.prec7, x = 0.6833334, y = 0, width = 0.3266667, height = 1)
p <- ggdraw() +
  draw_plot(p.DOS,        x = 0,    y = 0, width = 0.52, height = 1) +
  draw_plot(p.temp.prec7, x = 0.52, y = 0, width = 0.48, height = 1)

#save_plot("Figure_BigCheese_time-varying.tiff", p, ncol = 3, nrow = 1, dpi = 600)
save_plot("Figure_BigCheese_time-varying.jpg", p, ncol = 2, nrow = 1, dpi = 600)

## Plots for seasonally fixed covariates ##
# Forb cover #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.hierbas.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.hierbas.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.hierbas.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Forb cover 5m (%)") + ylab(NULL)

# Forb height #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.hierba_ht.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.hierba_ht.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.hierba_ht.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Forb height 5m (cm)") + ylab(NULL)

# Grass height #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.pasto_ht.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.pasto_ht.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.pasto_ht.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Grass height 5m (cm)") + ylab(NULL)

# Other cover #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.otra.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.otra.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.otra.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp),
              alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  labs(color = "Species") +
  theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Other cover 5m (%)") + ylab(NULL)

# Forb cover CV #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.hierbas_cv.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.hierbas_cv.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.hierbas_cv.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV Forb cover 5m (%)") + ylab(NULL)

# Other cover CV #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.otra_cv.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.otra_cv.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.otra_cv.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp),
              alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV other cover 5m (%)") + ylab(NULL)

# Shrub cover 5m #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Shrub_All_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Shrub_All_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.Shrub_All_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Shrub cover 5m (%)") + ylab(NULL)

# Shrub height 5m #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Max_Shrub_Height_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Max_Shrub_Height_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.Max_Shrub_Height_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Shrub height 5m (m)") + ylab(NULL)

# Shrub cover 5m CV #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Shrub_All_5m_CV.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Shrub_All_5m_CV.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.Shrub_All_5m_CV.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV shrub cover 5m (%)") + ylab(NULL)

# Shrub cover 50m CV #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Shrub_All_50m_CV.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Shrub_All_50m_CV.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.Shrub_All_50m_CV.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV shrub cover 50m (%)") + ylab(NULL)

# Shrub cover 500m CV #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Shrub_All_500m_CV.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Shrub_All_500m_CV.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x * 100)
p.Shrub_All_500m_CV.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("CV shrub cover 500m (%)") + ylab(NULL)

# Distance to Fence #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Distance_to_Fence.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Distance_to_Fence.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.Distance_to_Fence.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Distance to fence (m)") + ylab(NULL)

# Mass #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.peso.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.peso.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.peso.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Body mass (g)") + ylab(NULL)

# Passerine density #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.prey.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.prey.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.prey.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  guides(color = F, fill = F, linetype = F) +
  xlab(expression('Passerine density (per '*km^2*')')) + ylab(NULL)

# Raptor density #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.raptor.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.raptor.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p.raptor.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3,
              show.legend = F) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 1) +
  labs(color = "Species") +
  #theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  #guides(color = F, fill = F, linetype = F) +
  xlab(expression('Raptor density (per '*km^2*')')) + ylab(NULL)

# Put everything together
p <- ggdraw() +
  draw_plot(p.hierbas.PSR,             x = 0,    y = 0.75, width = 0.25, height = 0.25) +
  draw_plot(p.hierba_ht.PSR,           x = 0.25, y = 0.75, width = 0.25, height = 0.25) +
  draw_plot(p.hierbas_cv.PSR,          x = 0.5,  y = 0.75, width = 0.25, height = 0.25) +
  draw_plot(p.pasto_ht.PSR,            x = 0.75, y = 0.75, width = 0.25, height = 0.25) +
  
  draw_plot(p.otra.PSR,                x = 0,    y = 0.5,  width = 0.25, height = 0.25) +
  draw_plot(p.otra_cv.PSR,             x = 0.25, y = 0.5,  width = 0.25, height = 0.25) +
  draw_plot(p.Shrub_All_5m.PSR,        x = 0.5,  y = 0.5,  width = 0.25, height = 0.25) +
  draw_plot(p.Max_Shrub_Height_5m.PSR, x = 0.75, y = 0.5,  width = 0.25, height = 0.25) +
  
  draw_plot(p.Shrub_All_5m_CV.PSR,     x = 0,    y = 0.25, width = 0.25, height = 0.25) +
  draw_plot(p.Shrub_All_50m_CV.PSR,    x = 0.25, y = 0.25, width = 0.25, height = 0.25) +
  draw_plot(p.Shrub_All_500m_CV.PSR,   x = 0.5,  y = 0.25, width = 0.25, height = 0.25) +
  draw_plot(p.Distance_to_Fence.PSR,   x = 0.75, y = 0.25, width = 0.25, height = 0.25) +
  
  draw_plot(p.peso.PSR,                x = 0,    y = 0,    width = 0.25, height = 0.25) +
  draw_plot(p.prey.PSR,                x = 0.25, y = 0,    width = 0.25, height = 0.25) +
  draw_plot(p.raptor.PSR,              x = 0.5,  y = 0,    width = 0.35,  height = 0.25)
p <- ggdraw() +
  draw_plot(p, x = 0.05, y = 0, width = 0.95, height = 1) +
  draw_plot_label("Survival over 90 days", x = 0, y = 0.4, angle = 90, hjust = 0, size = 20)

#save_plot("Figure_BigCheese_additive.tiff", p, ncol = 3, nrow = 2.5, dpi = 600)
save_plot("Figure_BigCheese_additive.jpg", p, ncol = 2.5, nrow = 2.5, dpi = 600)

# Shrub cover X height #
# BAIS #
dat.plot.noshrubs <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS %>%
  filter(x2 == 0) %>%
  mutate(x1 = 0)
min.shrubs <- min(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS$x2[which(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS$x2 > 0)])
max.shrubs <- max(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS$x2[which(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS$x2 > 0)])
dat.plot.shrubs <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS %>%
  filter(x2 %in% c(min.shrubs, max.shrubs)) %>%
  mutate(x2 = round(x2, digits = 5) %>% as.factor)
p.height.BAIS <- ggplot(dat.plot.shrubs, aes(x = x1, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = x2), alpha = 0.3) +
  geom_line(aes(color = x2), size = 1) +
  geom_errorbar(data = dat.plot.noshrubs, aes(x = x1, ymin = PSR.lo, ymax = PSR.hi), width = 0.01) +
  geom_point(data = dat.plot.noshrubs, aes(x = x1, y = PSR.md), size = 2) +
  scale_color_manual(labels = c("0.006", "15"), values = c("#999999", "#D55E00")) + #009E73
  scale_fill_manual(labels = c("0.006", "15"), values = c("#999999", "#D55E00")) + #009E73
  annotate("text", x = -0.03, y = 0.3, label = "No shrubs", size = 4, angle = 90) +
  geom_vline(xintercept = 0.055, linetype = "dashed") +
  labs(fill = "Shrub cover (%)", color = "Shrub cover (%)") + 
  ylim(0, 0.35) +
  xlab("Max shrub height 5m (m)") + ylab("Survival over 90 days")

dat.plot <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.BAIS %>%
  filter(x2 > 0) %>%
  mutate(precision = 1/(PSR.hi - PSR.lo)) %>%
  mutate(x2 = x2*100)
p.heat.BAIS <- ggplot(dat.plot, aes(x1, x2)) +
  #geom_tile(aes(fill = PSR.md), color = "white") +
  geom_raster(aes(fill = PSR.md)) +
  geom_contour(aes(z = PSR.md),
               color = "white", linetype = 2, size = 0.8, binwidth = 0.02) +
  #scale_fill_gradient(low = "#0072B2", high = "#009E73", #F0E442
  #                    breaks = seq(0, 0.14, by = 0.02)) +
  scale_fill_gradient(low = "blue", high = "yellow", ##F0E442
                      breaks = seq(0, 0.22, by = 0.04)) +
  #scale_fill_gradient(colors = viridis(5)) +
  ylab("Shrub cover (%)") +
  xlab("Shrub height (m)") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(fill = "Survival over\n90 days")

# p.scatter.BAIS <- ggplot(data.BAIS$Covs %>% filter(Shrub_All_5m > 0 & Max_Shrub_Height_5m != mean(Max_Shrub_Height_5m)),
#                          aes(x = Max_Shrub_Height_5m, y = Shrub_All_5m)) +
#   geom_point(alpha = 0.1) +
#   ylab("Shrub cover (%)") +
#   xlab("Shrub height (m)")

# GRSP #
dat.plot.noshrubs <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP %>%
  filter(x2 == 0) %>%
  mutate(x1 = 0)
min.shrubs <- min(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP$x2[which(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP$x2 > 0)])
max.shrubs <- max(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP$x2[which(dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP$x2 > 0)])
dat.plot.shrubs <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP %>%
  filter(x2 %in% c(min.shrubs, max.shrubs)) %>%
  mutate(x2 = round(x2, digits = 5) %>% as.factor)
p.height.GRSP <- ggplot(dat.plot.shrubs, aes(x = x1, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, color = x2), alpha = 0.3, fill = "white", linetype = "dashed") +
  geom_line(aes(color = x2), size = 1) +
  geom_errorbar(data = dat.plot.noshrubs, aes(x = x1, ymin = PSR.lo, ymax = PSR.hi), width = 0.01) +
  geom_point(data = dat.plot.noshrubs, aes(x = x1, y = PSR.md), size = 2) +
  scale_color_manual(labels = c("0.02", "18"), values = c("#999999", "#D55E00")) +  #009E73
  annotate("text", x = -0.03, y = 0.3, label = "No shrubs", size = 4, angle = 90) +
  geom_vline(xintercept = 0.055, linetype = "dashed") +
  labs(color = "Shrub cover (%)") + 
  ylim(0, 0.35) +
  xlab("Max shrub height 5m (m)") + ylab("Survival over 90 days")

dat.plot <- dat.plot.Max_Shrub_Height_5mXShrub_All_5m.GRSP %>%
  filter(x2 > 0) %>%
  mutate(precision = 1/(PSR.hi - PSR.lo)) %>%
  mutate(x2 = x2*100)
p.heat.GRSP <- ggplot(dat.plot, aes(x1, x2)) +
  #geom_tile(aes(fill = PSR.md), color = "white") +
  geom_raster(aes(fill = PSR.md)) +
  geom_contour(aes(z = PSR.md),
               color = "white", linetype = 2, size = 0.8, binwidth = 0.04) +
#  scale_fill_gradient(low = "#0072B2", high = "#009E73", ##F0E442
#                      breaks = seq(0, 0.22, by = 0.04)) +
  scale_fill_gradient(low = "blue", high = "yellow", ##F0E442
                        breaks = seq(0, 0.22, by = 0.04)) +
  #scale_fill_gradient(colors = viridis(5)) +
  ylab("Shrub cover (%)") +
  xlab("Shrub height (m)") +
  theme(legend.title = element_text(size = 10),
        legend.text = element_text(size = 8)) +
  labs(fill = "Survival over\n90 days")

# Put interactions together #
p <- ggdraw() +
  draw_plot(p.height.BAIS, x = 0,    y = 0.5, width = 0.5,  height = 0.45) +
  draw_plot(p.heat.BAIS,   x = 0.5,  y = 0.5, width = 0.5,  height = 0.45) +
  draw_plot(p.height.GRSP, x = 0,    y = 0,   width = 0.5,  height = 0.45) +
  draw_plot(p.heat.GRSP,   x = 0.5,  y = 0,   width = 0.5,  height = 0.45) +
  draw_plot_label(c("Baird's Sparrow", "Grasshopper Sparrow"),
                  x = c(0.35, 0.28), y = c(1, 0.5))
  
#save_plot("Figure_BigCheese_interactions.tiff", p, ncol = 2.5, nrow = 2, dpi = 600)
save_plot("Figure_BigCheese_interactions.jpg", p, ncol = 2.5, nrow = 2, dpi = 600)

#################################
# Select plots for presentation #
#################################

# Bird density #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.prey.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.prey.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  labs(color = "Species", fill = "Species", linetype = "Species") +
  xlab(expression('Bird density (per '*km^2*')')) + ylab(NULL)
save_plot("Figure_presentation_bird_density_BigCheese.tiff", p, ncol = 1, nrow = 1, dpi = 600)

# Shrike density #
supp <- c(T, F)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.LOSH.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.LOSH.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
p <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, 0.5) +
  labs(color = "Species", fill = "Species", linetype = "Species") +
  xlab(expression('Shrike density (per '*km^2*')')) + ylab(NULL)
save_plot("Figure_presentation_shrike_density_BigCheese.tiff", p, ncol = 1, nrow = 1, dpi = 600)
