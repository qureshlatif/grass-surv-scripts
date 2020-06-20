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
mod.nam <- "ShrubSpp"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot <- c("temp.prec7", "pastos", "Juniper_5m", "Yucca_5m",
                  "Mean_Shrub_Height_5m_CV", "Mean_Shrub_Height_50m_CV",
                  "prey", "LOSH")
vars.quad <- c(F, T, T, T,
               F, F,
               F, F)

source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
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
                         DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.05)),
                         DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.95)),
                         PSR.md = apply(PSR, 2, median),
                         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
                         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
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
  xlab(expression('Temp prior 7 days ('*degree*C*')')) + ylab(NULL)
p.temp.prec7.PSR <- ggplot(dat.plot.temp.prec7, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Temp prior 7 days ('*degree*C*')')) + ylab(NULL)

p.pastos.DSR <- ggplot(dat.plot.pastos, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Grass cover (%)") + ylab(NULL)
p.pastos.PSR <- ggplot(dat.plot.pastos, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Grass cover (%)") + ylab(NULL)

p.Juniper_5m.DSR <- ggplot(dat.plot.Juniper_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Juniper cover (%, 5m)") + ylab(NULL)
p.Juniper_5m.PSR <- ggplot(dat.plot.Juniper_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Juniper cover (%, 5m)") + ylab(NULL)

p.Yucca_5m.DSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Yucca cover (%, 5m)") + ylab(NULL)
p.Yucca_5m.PSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Yucca cover (%, 5m)") + ylab(NULL)

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
  xlab(expression('Prey density (per '*km^2*')')) + ylab(NULL)
p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Prey density (per '*km^2*')')) + ylab(NULL)

p.LOSH.DSR <- ggplot(dat.plot.LOSH, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab(expression('Shrike density (per '*km^2*')')) + ylab(NULL)
p.LOSH.PSR <- ggplot(dat.plot.LOSH, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab(expression('Shrike density (per '*km^2*')')) + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.temp.prec7.DSR,               x = 0.05, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.pastos.DSR,                   x = 0.24, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Juniper_5m.DSR,               x = 0.43, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Yucca_5m.DSR,                 x = 0.62, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.DSR,  x = 0.05, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.24, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.prey.DSR,                     x = 0.43, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.LOSH.DSR,                     x = 0.62, y = 0,   width = 0.19, height = 0.5) +
  draw_plot_label("Daily survival probability (BAIS)", x = 0, y = 0.15, angle = 90)

save_plot(str_c("Figure_DSR_", mod.nam, "_BAIS.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

p <- ggdraw() +
  draw_plot(p.temp.prec7.PSR,               x = 0.05, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.pastos.PSR,                   x = 0.24, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Juniper_5m.PSR,               x = 0.43, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Yucca_5m.PSR,                 x = 0.62, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.05, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.24, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.prey.PSR,                     x = 0.43, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.LOSH.PSR,                     x = 0.62, y = 0,   width = 0.19, height = 0.5) +
  draw_plot_label("Seasonal survival probability (BAIS)", x = 0, y = 0.1, angle = 90)

save_plot(str_c("Figure_PSR_", mod.nam, "_BAIS.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

#__________________________________________#

#######################
# Grasshopper Sparrow #
#######################

spp <- "GRSP" # BAIS or GRSP
mod.nam <- "ShrubSpp"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot <- c("DOS", "Mesquite_5m", "Yucca_5m", "otra", "hierbas_cv",
                  "otra_cv", "Shrub_All_500m_CV", "Mean_Shrub_Height_5m_CV",
                  "Mean_Shrub_Height_50m_CV", "prey")
vars.quad <- c(T, T, T, T, F,
               F, F, F,
               F, F)

source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
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
                         DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.05)),
                         DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.95)),
                         PSR.md = apply(PSR, 2, median),
                         PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
                         PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
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
  xlab("Other cover (%)") + ylab(NULL)
p.otra.PSR <- ggplot(dat.plot.otra, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Other cover (%)") + ylab(NULL)

p.hierbas_cv.DSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV Forb cover") + ylab(NULL)
p.hierbas_cv.PSR <- ggplot(dat.plot.hierbas_cv, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV Forb cover") + ylab(NULL)

p.Mesquite_5m.DSR <- ggplot(dat.plot.Mesquite_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Mesquite cover (%; 5m)") + ylab(NULL)
p.Mesquite_5m.PSR <- ggplot(dat.plot.Mesquite_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Mesquite cover (%; 5m)") + ylab(NULL)

p.Yucca_5m.DSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Yucca cover (%; 5m)") + ylab(NULL)
p.Yucca_5m.PSR <- ggplot(dat.plot.Yucca_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Yucca cover (%; 5m)") + ylab(NULL)

p.Shrub_All_500m_CV.DSR <- ggplot(dat.plot.Shrub_All_500m_CV, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("CV shrub cover (500m)") + ylab(NULL)
p.Shrub_All_500m_CV.PSR <- ggplot(dat.plot.Shrub_All_500m_CV, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("CV shrub cover (500m)") + ylab(NULL)

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
  xlab("Prey density (per 100 ha)") + ylab(NULL)
p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Prey density (per 100 ha)") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.otra.DSR,                     x = 0.05, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.hierbas_cv.DSR,               x = 0.24, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Mesquite_5m.DSR,              x = 0.43, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Yucca_5m.DSR,                 x = 0.62, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Shrub_All_500m_CV.DSR,        x = 0.05, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.DSR,  x = 0.24, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.43, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.prey.DSR,                     x = 0.62, y = 0,   width = 0.19, height = 0.5) +
  draw_plot_label("Daily survival probability (GRSP)", x = 0, y = 0.1, angle = 90)

save_plot(str_c("Figure_DSR_",mod.nam,"_GRSP.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

p <- ggdraw() +
  draw_plot(p.otra.PSR,                     x = 0.05, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.hierbas_cv.PSR,               x = 0.24, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Mesquite_5m.PSR,              x = 0.43, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Yucca_5m.PSR,                 x = 0.62, y = 0.5, width = 0.19, height = 0.5) +
  draw_plot(p.Shrub_All_500m_CV.PSR,        x = 0.05, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.24, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.43, y = 0,   width = 0.19, height = 0.5) +
  draw_plot(p.prey.PSR,                     x = 0.62, y = 0,   width = 0.19, height = 0.5) +
  draw_plot_label("Seasonal survival probability (GRSP)", x = 0, y = 0.1, angle = 90, hjust = 0)

save_plot(str_c("Figure_PSR_",mod.nam,"_GRSP.tiff"), p, ncol = 4, nrow = 2, dpi = 200)

#__________________________________________#
