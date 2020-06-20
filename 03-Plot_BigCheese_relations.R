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
vars.to.plot <- c("temp.prec7", "hierbas", "pastos", "Mean_Shrub_Height_5m", 
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

# min.DSR <- min(min.DSR)
# max.DSR <- max(max.DSR)
# min.PSR <- min(min.PSR)
# max.PSR <- max(max.PSR)

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

p.hierbas.DSR <- ggplot(dat.plot.hierbas, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Forb cover (%)") + ylab(NULL)
p.hierbas.PSR <- ggplot(dat.plot.hierbas, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Forb cover (%)") + ylab(NULL)

p.pastos.DSR <- ggplot(dat.plot.pastos, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Grass cover (%)") + ylab(NULL)
p.pastos.PSR <- ggplot(dat.plot.pastos, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Grass cover (%)") + ylab(NULL)

p.Mean_Shrub_Height_5m.DSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub height (m, 5m)") + ylab(NULL)
p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height (m, 5m)") + ylab(NULL)

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

p.LOSH.DSR <- ggplot(dat.plot.LOSH, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrike density (per 100 ha)") + ylab(NULL)
p.LOSH.PSR <- ggplot(dat.plot.LOSH, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrike density (per 100 ha)") + ylab(NULL)

p <- ggdraw() +
  draw_plot(p.temp.prec7.DSR,               x = 0.0500000, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.hierbas.DSR,                  x = 0.3666667, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.pastos.DSR,                   x = 0.6833333, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m.DSR,     x = 0.0500000, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.DSR,  x = 0.3666667, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.6833333, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.prey.DSR,                     x = 0.0500000, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.LOSH.DSR,                     x = 0.3666667, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot_label("Daily survival probability (BAIS)", x = 0, y = 0.15, angle = 90)

save_plot("Figure_DSR_BigCheese_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

p <- ggdraw() +
  draw_plot(p.temp.prec7.PSR,               x = 0.0500000, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.hierbas.PSR,                  x = 0.3666667, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.pastos.PSR,                   x = 0.6833333, y = 0.6666667, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.0500000, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.3666667, y = 0.3333333, width = 0.3166667, height = 0.3333333) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.6833333, y = 0.3333333, width = 0.3000000, height = 0.3333333) +
  draw_plot(p.prey.PSR,                     x = 0.0500000, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot(p.LOSH.PSR,                     x = 0.3666667, y = 0,         width = 0.3166667, height = 0.3333333) +
  draw_plot_label("Seasonal survival probability (BAIS)", x = 0, y = 0.1, angle = 90)

save_plot("Figure_PSR_BigCheese_BAIS.tiff", p, ncol = 2, nrow = 2, dpi = 200)

#__________________________________________#

#######################
# Grasshopper Sparrow #
#######################

spp <- "GRSP" # BAIS or GRSP
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot <- c("DOS", "Shrub_All_5m", "otra", "Mean_Shrub_Height_5m", "hierbas_cv",
                  "Shrub_All_500m_CV", "Mean_Shrub_Height_5m_CV", "Mean_Shrub_Height_50m_CV",
                  "peso", "prey")
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

# intxn.vars <- c("temp.min", "pastos")
# vars.quad <- c(F, T)
# intxn.term <- c("pastosXtemp.min")
# z1 <- c(seq(quantile(X[,,intxn.vars[1]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
#             quantile(X[,,intxn.vars[1]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),length.out = 20),
#         seq(quantile(X[,,intxn.vars[1]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
#             quantile(X[,,intxn.vars[1]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),length.out = 20))
# z2 <- c(rep(quantile(X[,,intxn.vars[2]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),20),
#         rep(quantile(X[,,intxn.vars[2]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T), 20))
# x1 <- z1 * X.sd[intxn.vars[1]] + X.mn[intxn.vars[1]]
# x2 <- z2 * X.sd[intxn.vars[2]] + X.mn[intxn.vars[2]]
# z1.arr <- z1 %>%
#   array(., dim = c(length(x1), nsims)) %>%
#   aperm(c(2, 1))
# z2.arr <- z2 %>%
#   array(., dim = c(length(x2), nsims)) %>%
#   aperm(c(2, 1))
# B0 <- mod$sims.concat["B.Intercept", ] %>% array(., dim = c(nsims, length(x1)))
# B1 <- mod$sims.concat[str_c("B.", intxn.vars[1]), ] %>% array(., dim = c(nsims, length(x1)))
# B2 <- mod$sims.concat[str_c("B.", intxn.vars[2]), ] %>% array(., dim = c(nsims, length(x1)))
# BX <- mod$sims.concat[str_c("B.", intxn.term), ] %>% array(., dim = c(nsims, length(x1)))
# if(vars.quad[1]) {
#   B12 <- mod$sims.concat[str_c("B.", intxn.vars[1], "2"), ] %>% array(., dim = c(nsims, length(x1)))
# } else {
#   B12 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x1)))
# }
# if(vars.quad[2]) {
#   B22 <- mod$sims.concat[str_c("B.", intxn.vars[2], "2"), ] %>% array(., dim = c(nsims, length(x1)))
# } else {
#   B22 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x1)))
# }
# DSR <- expit(B0 + B1*z1.arr + B12*(z1.arr^2) + B2*z2.arr + B22*(z2.arr^2) + BX*z1.arr*z2.arr)
# PSR <- DSR ^ 90
# dat.plot <- data.frame(x1, x2, z1, z2, DSR.md = apply(DSR, 2, median),
#                        DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.025)),
#                        DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.975)),
#                        PSR.md = apply(PSR, 2, median),
#                        PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.025)),
#                        PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.975)))
# dat.plot$x2 <- round(dat.plot$x2)
# dat.plot$x2 <- factor(dat.plot$x2)

# min.DSR <- c(min.DSR, min(dat.plot$DSR.lo))
# max.DSR <- c(max.DSR, max(dat.plot$DSR.hi))
# min.PSR <- c(min.PSR, min(dat.plot$PSR.lo))
# max.PSR <- c(max.PSR, max(dat.plot$PSR.hi))
# 
# assign(str_c("dat.plot.", intxn.term), dat.plot)

min.DSR <- min(min.DSR)
max.DSR <- max(max.DSR)
min.PSR <- min(min.PSR)
max.PSR <- max(max.PSR)

# Plots for individual covariates #
p.DOS.DSR <- ggplot(dat.plot.DOS, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Day of season") + ylab(NULL)
p.DOS.PSR <- ggplot(dat.plot.DOS, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Day of season") + ylab(NULL)

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

p.Shrub_All_5m.DSR <- ggplot(dat.plot.Shrub_All_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub cover (%; 5m)") + ylab(NULL)
p.Shrub_All_5m.PSR <- ggplot(dat.plot.Shrub_All_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub cover (%; 5m)") + ylab(NULL)

p.Mean_Shrub_Height_5m.DSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Shrub height (m; 5m)") + ylab(NULL)
p.Mean_Shrub_Height_5m.PSR <- ggplot(dat.plot.Mean_Shrub_Height_5m, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Shrub height (m; 5m)") + ylab(NULL)

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

p.peso.DSR <- ggplot(dat.plot.peso, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Mass (g)") + ylab(NULL)
p.peso.PSR <- ggplot(dat.plot.peso, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Mass (g)") + ylab(NULL)

p.prey.DSR <- ggplot(dat.plot.prey, aes(x = x, y = DSR.md)) +
  geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  xlab("Prey density (100 ha)") + ylab(NULL)
p.prey.PSR <- ggplot(dat.plot.prey, aes(x = x, y = PSR.md)) +
  geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi), alpha = 0.3) +
  geom_line(size = 1) +
  ylim(0, 1) +
  xlab("Prey density (100 ha)") + ylab(NULL)

# p.pastosXtempmin.DSR <- ggplot(dat.plot.pastosXtemp.min, aes(x = x1, y = DSR.md, color = x2)) +
#   geom_ribbon(aes(ymin = DSR.lo, ymax = DSR.hi, fill = x2), alpha = 0.3) +
#   geom_line(size = 1) +
#   xlab("Min temperature (deg C)") + ylab(NULL) +
#   labs(color = "Grass\ncover") + labs(fill = "Grass\ncover")
# p.pastosXtempmin.PSR <- ggplot(dat.plot.pastosXtemp.min, aes(x = x1, y = PSR.md, color = x2)) +
#   geom_ribbon(aes(ymin = PSR.lo, ymax = PSR.hi, fill = x2), alpha = 0.3) +
#   geom_line(size = 1) +
#   ylim(0, 1) +
#   xlab("Min temperature (deg C)") + ylab(NULL) +
#   labs(color = "Grass\ncover") + labs(fill = "Grass\ncover")

p <- ggdraw() +
  draw_plot(p.DOS.DSR,                      x = 0.05, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.otra.DSR,                     x = 0.24, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.hierbas_cv.DSR,               x = 0.43, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Shrub_All_5m.DSR,             x = 0.62, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m.DSR,     x = 0.81, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Shrub_All_500m_CV.DSR,        x = 0.05, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.DSR,  x = 0.24, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.DSR, x = 0.43, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.peso.DSR,                     x = 0.62, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.prey.DSR,                     x = 0.81, y = 0,   width = 0.185, height = 0.5) +
  draw_plot_label("Daily survival probability (GRSP)", x = 0, y = 0, angle = 90)

save_plot("Figure_DSR_BigCheese_GRSP.tiff", p, ncol = 3, nrow = 1.5, dpi = 200)

p <- ggdraw() +
  draw_plot(p.DOS.PSR,                      x = 0.05, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.otra.PSR,                     x = 0.24, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.hierbas_cv.PSR,               x = 0.43, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Shrub_All_5m.PSR,             x = 0.62, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m.PSR,     x = 0.81, y = 0.5, width = 0.19,  height = 0.5) +
  draw_plot(p.Shrub_All_500m_CV.PSR,        x = 0.05, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_5m_CV.PSR,  x = 0.24, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.Mean_Shrub_Height_50m_CV.PSR, x = 0.43, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.peso.PSR,                     x = 0.62, y = 0,   width = 0.19,  height = 0.5) +
  draw_plot(p.prey.PSR,                     x = 0.81, y = 0,   width = 0.185, height = 0.5) +
  draw_plot_label("Seasonal survival probability (GRSP)", x = 0, y = 0.2, angle = 90, hjust = 0)

save_plot("Figure_PSR_BigCheese_GRSP.tiff", p, ncol = 3, nrow = 1.5, dpi = 200)

#__________________________________________#
