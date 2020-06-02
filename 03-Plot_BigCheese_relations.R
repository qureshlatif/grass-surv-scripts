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

spp <- "BAIS" # BAIS or GRSP
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot <- c("temp.prec7", "hierbas", "arbusto", "pastos", "Mean_Shrub_Height_5m",
                  "Mean_Shrub_Height_5m_CV", "Mean_Shrub_Height_50m_CV",
                  "prey", "LOSH")
vars.quad <- c(F, T, T, T, T,
               F, F,
               F, F)

source(str_c(scripts.loc, "Data_processing_BigCheese.R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
min.DSR <- max.DSR <- min.PSR <- max.PSR <- c()
for(v in 1:length(vars.to.plot)) {
  z <- seq(quantile(X[,,vars.to.plot[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
           quantile(X[,,vars.to.plot[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
           length.out = 20)
  x <- z * X.sd[vars.to.plot[v]] + X.mn[vars.to.plot[v]]
  z.arr <- z %>%
    array(., dim = c(length(x), nsims)) %>%
    aperm(c(2, 1))
  B0 <- mod$sims.concat["B.Intercept", ] %>% array(., dim = c(nsims, length(x)))
  B1 <- mod$sims.concat[str_c("B.", vars.to.plot[v]), ] %>% array(., dim = c(nsims, length(x)))
  if(vars.quad[v]) {
    B2 <- mod$sims.concat[str_c("B.", vars.to.plot[v], "2"), ] %>% array(., dim = c(nsims, length(x)))
    } else {
      B2 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x)))
    }
  DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2))
  PSR <- DSR ^ 90
  dat.plot <- data.frame(x, z, DSR.md = apply(DSR, 2, median),
                         DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.025)),
                         DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.975)),
                         PSR.md = apply(PSR, 2, median),
                         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.025)),
                         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.975)))
  min.DSR <- c(min.DSR, min(dat.plot$DSR.lo))
  max.DSR <- c(max.DSR, max(dat.plot$DSR.hi))
  min.PSR <- c(min.PSR, min(dat.plot$PSR.lo))
  max.PSR <- c(max.PSR, max(dat.plot$PSR.hi))
  assign(str_c("dat.plot.", vars.to.plot[v]), dat.plot)
}

min.DSR <- min(min.DSR)
max.DSR <- max(max.DSR)
min.PSR <- min(min.PSR)
max.PSR <- max(max.PSR)

# Plots for individual covariates #
p.temp.prec7.DSR <- ggplot(dat.plot.temp.prec7, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Temp prior 7 days") + ylab(NULL)
p.temp.prec7.PSR <- ggplot(dat.plot.temp.prec7, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Temp prior 7 days") + ylab(NULL)

p.hierbas.DSR <- ggplot(dat.plot.hierbas, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Forbe cover") + ylab(NULL)
p.hierbas.PSR <- ggplot(dat.plot.hierbas, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Forbe cover") + ylab(NULL)

p.arbusto.DSR <- ggplot(dat.plot.arbusto, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub cover (field)") + ylab(NULL)
p.arbusto.PSR <- ggplot(dat.plot.arbusto, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub cover (field)") + ylab(NULL)

p.pastos.DSR <- ggplot(dat.plot.pastos, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Grass cover") + ylab(NULL)
p.pastos.PSR <- ggplot(dat.plot.pastos, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Grass cover") + ylab(NULL)

p.Mean_Shrub_Height_5m.DSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub height (5m)") + ylab(NULL)
p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height (5m)") + ylab(NULL)

p.Mean_Shrub_Height_5m_CV.DSR <- ggplot(dat.plot.Mean_Shrub_Height_5m_CV, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV shrub height (5m)") + ylab(NULL)
p.Mean_Shrub_Height_5m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height (5m)") + ylab(NULL)

p.Mean_Shrub_Height_50m_CV.DSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV shrub height (50m)") + ylab(NULL)
p.Mean_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height (50m)") + ylab(NULL)

p.prey.DSR <- ggplot(dat.plot.prey, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Prey density") + ylab(NULL)
p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Prey density") + ylab(NULL)

p.LOSH.DSR <- ggplot(dat.plot.LOSH, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrike density") + ylab(NULL)
p.LOSH.PSR <- ggplot(dat.plot.LOSH, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrike density") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.temp.prec7.DSR,               x = 0.0500000, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.hierbas.DSR,                  x = 0.3666667, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.arbusto.DSR,                  x = 0.6833333, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.pastos.DSR,                   x = 0.0500000, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m.DSR,     x = 0.3666667, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.DSR,  x = 0.6833333, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.0500000, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.prey.DSR,                     x = 0.3666667, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.LOSH.DSR,                     x = 0.6833333, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot_label("Daily survival probability", x = 0, y = 0.27, angle = 90)

save_plot("Figure_DSR_BigCheese_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

p <- ggdraw() +
  draw_plot(p.temp.prec7.PSR,               x = 0.0500000, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.hierbas.PSR,                  x = 0.3666667, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.arbusto.PSR,                  x = 0.6833333, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.pastos.PSR,                   x = 0.0500000, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.3666667, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.6833333, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.0500000, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.prey.PSR,                     x = 0.3666667, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.LOSH.PSR,                     x = 0.6833333, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot_label("Seasonal survival probability", x = 0, y = 0.27, angle = 90)

save_plot("Figure_PSR_BigCheese_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

#__________________________________________#

#######################
# Grasshopper Sparrow #
#######################

spp <- "GRSP" # BAIS or GRSP
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot <- c("Shrub_All_5m", "otra", "Mean_Shrub_Height_5m", "hierbas_cv",
                  "Mean_Shrub_Height_50m_CV", "prey")
vars.quad <- c(T, T, T, F,
               F, F)

source(str_c(scripts.loc, "Data_processing_BigCheese.R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
min.DSR <- max.DSR <- min.PSR <- max.PSR <- c()
for(v in 1:length(vars.to.plot)) {
  z <- seq(quantile(X[,,vars.to.plot[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
           quantile(X[,,vars.to.plot[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
           length.out = 20)
  x <- z * X.sd[vars.to.plot[v]] + X.mn[vars.to.plot[v]]
  z.arr <- z %>%
    array(., dim = c(length(x), nsims)) %>%
    aperm(c(2, 1))
  B0 <- mod$sims.concat["B.Intercept", ] %>% array(., dim = c(nsims, length(x)))
  B1 <- mod$sims.concat[str_c("B.", vars.to.plot[v]), ] %>% array(., dim = c(nsims, length(x)))
  if(vars.quad[v]) {
    B2 <- mod$sims.concat[str_c("B.", vars.to.plot[v], "2"), ] %>% array(., dim = c(nsims, length(x)))
  } else {
    B2 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x)))
  }
  DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2))
  PSR <- DSR ^ 90
  dat.plot <- data.frame(x, z, DSR.md = apply(DSR, 2, median),
                         DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.025)),
                         DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.975)),
                         PSR.md = apply(PSR, 2, median),
                         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.025)),
                         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.975)))
  min.DSR <- c(min.DSR, min(dat.plot$DSR.lo))
  max.DSR <- c(max.DSR, max(dat.plot$DSR.hi))
  min.PSR <- c(min.PSR, min(dat.plot$PSR.lo))
  max.PSR <- c(max.PSR, max(dat.plot$PSR.hi))
  assign(str_c("dat.plot.", vars.to.plot[v]), dat.plot)
}

min.DSR <- min(min.DSR)
max.DSR <- max(max.DSR)
min.PSR <- min(min.PSR)
max.PSR <- max(max.PSR)

# Plots for individual covariates #
p.otra.DSR <- ggplot(dat.plot.otra, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Other cover") + ylab(NULL)
p.otra.PSR <- ggplot(dat.plot.otra, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Other cover") + ylab(NULL)

p.hierbas_cv.DSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV Forbe cover") + ylab(NULL)
p.hierbas_cv.PSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV Forbe cover") + ylab(NULL)

p.Shrub_All_5m.DSR <- ggplot(dat.plot.Shrub_All_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub cover (5m)") + ylab(NULL)
p.Shrub_All_5m.PSR <- ggplot(dat.plot.Shrub_All_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub cover (5m)") + ylab(NULL)

p.Mean_Shrub_Height_5m.DSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub height (5m)") + ylab(NULL)
p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height (5m)") + ylab(NULL)

p.Mean_Shrub_Height_50m_CV.DSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV shrub height (50m)") + ylab(NULL)
p.Mean_Shrub_Height_50m_CV.PSR <- ggplot(dat.plot.Mean_Shrub_Height_50m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub height (50m)") + ylab(NULL)

p.prey.DSR <- ggplot(dat.plot.prey, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Prey density") + ylab(NULL)
p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Prey density") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.otra.DSR,                     x = 0.0500000, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.hierbas_cv.DSR,               x = 0.3666667, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.Shrub_All_5m.DSR,             x = 0.6833333, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m.DSR,     x = 0.0500000, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.3666667, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.prey.DSR,                     x = 0.6833333, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot_label("Daily survival probability", x = 0, y = 0.15, angle = 90)

save_plot("Figure_DSR_BigCheese_GRSP.tiff", p, ncol = 2, nrow = 1.5, dpi = 200)

p <- ggdraw() +
  draw_plot(p.otra.PSR,                     x = 0.0500000, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.hierbas_cv.PSR,               x = 0.3666667, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.Shrub_All_5m.PSR,             x = 0.6833333, y = 0.5, width = 0.3166667, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.0500000, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.3666667, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot(p.prey.PSR,                     x = 0.6833333, y = 0,   width = 0.3166667, height = 0.5) +
  draw_plot_label("Seasonal survival probability", x = 0, y = 0.08, angle = 90)

save_plot("Figure_PSR_BigCheese_GRSP.tiff", p, ncol = 2, nrow = 1.5, dpi = 200)

#__________________________________________#
