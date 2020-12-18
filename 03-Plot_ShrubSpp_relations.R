library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
scripts.loc <- "grass-surv-scripts/"

load("ShrubSpp_cache.RData")
colors.spp <- c("saddlebrown", "#009E73")

# ## Plots for time-varying covariates ##
# # Day of season #
# supp <- c(F, T)
# fill.spp <- ifelse(supp, colors.spp, "white")
# linetype.spp <- ifelse(supp, "solid", "dashed")
# dat.plot <- dat.plot.DOS.BAIS %>%
#   mutate(spp = "BAIS") %>%
#   bind_rows(dat.plot.DOS.GRSP %>%
#               mutate(spp = "GRSP")) %>%
#   mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
# p.DOS <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
#   geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3) +
#   geom_line(aes(color = spp), size = 1) +
#   scale_color_manual(values = colors.spp) +
#   scale_fill_manual(values = fill.spp) +
#   scale_linetype_manual(values = linetype.spp) +
#   scale_x_continuous(breaks = c(20, 50, 81, 109), labels = c("Dec 1", "Jan 1", "Feb 1", "Mar 1")) +
#   ylim(0.95, 1) +
#   guides(color = F, fill = F, linetype = F) +
#   xlab('Day of season') + ylab("Daily survival")
# 
# # Min temperature #
# supp <- c(F, T)
# fill.spp <- ifelse(supp, colors.spp, "white")
# linetype.spp <- ifelse(supp, "solid", "dashed")
# dat.plot <- dat.plot.temp.min.BAIS %>%
#   mutate(spp = "BAIS") %>%
#   bind_rows(dat.plot.temp.min.GRSP %>%
#               mutate(spp = "GRSP")) %>%
#   mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
# p.temp.min <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
#   geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3,
#               show.legend = F) +
#   geom_line(aes(color = spp), size = 1) +
#   scale_color_manual(values = colors.spp) +
#   scale_fill_manual(values = fill.spp) +
#   scale_linetype_manual(values = linetype.spp) +
#   ylim(0.95, 1) +
#   labs(color = "Species") +
#   xlab(expression('Min temp ('*degree*C*')')) + ylab(NULL)
# 
# # Prec 7 mean temperature #
# supp <- c(T, T)
# fill.spp <- ifelse(supp, colors.spp, "white")
# linetype.spp <- ifelse(supp, "solid", "dashed")
# dat.plot <- dat.plot.temp.prec7.BAIS %>%
#   mutate(spp = "BAIS") %>%
#   bind_rows(dat.plot.temp.prec7.GRSP %>%
#               mutate(spp = "GRSP")) %>%
#   mutate(spp = factor(spp, levels = c("BAIS", "GRSP")))
# p.temp.prec7 <- ggplot(dat.plot, aes(x = x, y = DSR.md)) +
#   geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3,
#               show.legend = F) +
#   geom_line(aes(color = spp), size = 1) +
#   scale_color_manual(values = colors.spp) +
#   scale_fill_manual(values = fill.spp) +
#   scale_linetype_manual(values = linetype.spp) +
#   ylim(0.95, 1) +
#   labs(color = "Species") +
#   xlab(expression('Prior 7 day mean temp ('*degree*C*')')) + ylab(NULL)
# 
# p <- ggdraw() +
#   draw_plot(p.DOS,      x = 0,    y = 0, width = 0.45, height = 1) +
#   draw_plot(p.temp.min, x = 0.45, y = 0, width = 0.55, height = 1)
# 
# save_plot("Figure_ShrubSpp_time-varying.jpg", p, ncol = 2, nrow = 1, dpi = 600)
# #save_plot("Figure_ShrubSpp-varying.tiff", p, ncol = 3, nrow = 1, dpi = 600)

# Plots for seasonally fixed covariates #
max.y <- 0.6
  # Juniper 5m #
supp <- c(T, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Juniper_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Juniper_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100) # convert to percent
p.Juniper_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3,
              show.legend = F) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, max.y) +
  theme(legend.position = c(0,1), legend.justification = c(0,1)) +
  labs(color = "Species") +
  #guides(color = F, fill = F, linetype = F) +
  xlab("Juniper cover 5m (%)") + ylab(NULL)

  # Juniper 500m #
supp <- c(T, T)
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
  ylim(0, max.y) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Juniper cover 500m (%)") + ylab(NULL)

  # Mesquite 5m #
supp <- c(F, T)
fill.spp <- ifelse(supp, colors.spp, "white")
linetype.spp <- ifelse(supp, "solid", "dashed")
dat.plot <- dat.plot.Mesquite_5m.BAIS %>%
  mutate(spp = "BAIS") %>%
  bind_rows(dat.plot.Mesquite_5m.GRSP %>%
              mutate(spp = "GRSP")) %>%
  mutate(spp = factor(spp, levels = c("BAIS", "GRSP"))) %>%
  mutate(x = x*100) # convert to percent
p.Mesquite_5m.PSR <- ggplot(dat.plot, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = spp, color = spp, linetype = spp), alpha = 0.3,
              show.legend = F) +
  geom_line(aes(color = spp), size = 1) +
  scale_color_manual(values = colors.spp) +
  scale_fill_manual(values = fill.spp) +
  scale_linetype_manual(values = linetype.spp) +
  ylim(0, max.y) +
  guides(color = F, fill = F, linetype = F) +
  xlab("Mesquite 5m (%)") + ylab(NULL)

# p <- ggdraw() +
#   draw_plot(p.pastos.PSR,       x = 0,    y = 0.5, width = 0.33, height = 0.5) +
#   draw_plot(p.Juniper_5m.PSR,   x = 0.33, y = 0.5, width = 0.33, height = 0.5) +
#   draw_plot(p.Juniper_500m.PSR, x = 0.67, y = 0.5, width = 0.33, height = 0.5) +
#   draw_plot(p.Yucca_5m.PSR,     x = 0,    y = 0,   width = 0.33, height = 0.5) +
#   draw_plot(p.Mesquite_5m.PSR,  x = 0.33, y = 0,   width = 0.42, height = 0.5)
p <- ggdraw() +
  draw_plot(p.Juniper_5m.PSR,   x = 0,      y = 0, width = 0.3333, height = 1) +
  draw_plot(p.Juniper_500m.PSR, x = 0.3333, y = 0, width = 0.3333, height = 1) +
  draw_plot(p.Mesquite_5m.PSR,  x = 0.6667, y = 0, width = 0.3333, height = 1)
p <- ggdraw() +
  draw_plot(p, x = 0.05, y = 0, width = 0.95, height = 1) +
  draw_plot_label("Survival over 90 days", x = 0, y = 0.5, angle = 90, hjust = 0.5)

save_plot(str_c("Figure_PSR_", mod.nam, ".jpg"), p, ncol = 3, nrow = 1, dpi = 200)
