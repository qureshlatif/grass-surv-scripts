library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
library(abind)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"

vars.to.plot.DSR <- c("DOS", "temp.min", "temp.prec7")
vars.quad.DSR <- c(T, F, F)
vars.to.plot.PSR <- c("hierbas", "hierba_ht", "pastos", "pasto_ht", "salsola", "otra", "otra_cv",
                      "Distance_to_Fence", "hierbas_cv", "Shrub_All_5m", "Max_Shrub_Height_5m",
                      "Shrub_All_5m_CV", "peso", "prey", "LOSH", "raptor", "NDVI")
vars.quad.PSR <- c(T, T, T, T, T, T, F,
                   F, F, T, T,
                   F, F, F, F, F, F)

spp.vec <- c("BAIS", "GRSP")
mod.nam <- "BigCheese"

for(spp in spp.vec) {
  
  mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
  nsims <- dim(mod$sims.concat)[2]
  
  source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
  dimnames(X)[[3]] <- X.nams
  
  # Time-varying covariates #
  for(v in 1:length(vars.to.plot.DSR)) {
    z <- seq(quantile(X[,,vars.to.plot.DSR[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
             quantile(X[,,vars.to.plot.DSR[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
             length.out = 20)
    x <- z * X.sd[vars.to.plot.DSR[v]] + X.mn[vars.to.plot.DSR[v]]
    z.arr <- z %>%
      array(., dim = c(length(x), nsims)) %>%
      aperm(c(2, 1))
    B0 <- mod$sims.concat["B.Intercept", ] %>% array(., dim = c(nsims, length(x)))
    B1 <- mod$sims.concat[str_c("B.", vars.to.plot.DSR[v]), ] %>% array(., dim = c(nsims, length(x)))
    if(vars.quad.DSR[v]) {
      B2 <- mod$sims.concat[str_c("B.", vars.to.plot.DSR[v], "2"), ] %>% array(., dim = c(nsims, length(x)))
    } else {
      B2 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x)))
    }
    DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2))
    dat.plot <- data.frame(x, z, DSR.md = apply(DSR, 2, median),
                           DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.05)),
                           DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.95)))
    assign(str_c("dat.plot.", vars.to.plot.DSR[v], ".", spp), dat.plot)
  }
  
  ## Seasonally fixed covariates ##
  st <- 1:(J-89)
  end <- 90:J
  nsims.samp <- sample(nsims, nsims/100, replace = F)
  z.DOS <- apply(X[,,"DOS"], 2, mean) %>% array(., dim = c(J, length(nsims.samp), 20)) %>%
    aperm(c(2, 3, 1))
  z.DOS2 <- z.DOS^2
  z.temp.min <- apply(X[,,"temp.min"], 2, mean) %>% array(., dim = c(J, length(nsims.samp), 20)) %>%
    aperm(c(2, 3, 1))
  z.temp.prec7 <- apply(X[,,"temp.prec7"], 2, mean) %>% array(., dim = c(J, length(nsims.samp), 20)) %>%
    aperm(c(2, 3, 1))
  B.DOS <- mod$sims.concat["B.DOS", nsims.samp] %>% array(., dim = c(length(nsims.samp), 20, J))
  B.DOS2 <- mod$sims.concat["B.DOS2", nsims.samp] %>% array(., dim = c(length(nsims.samp), 20, J))
  B.temp.min <- mod$sims.concat["B.temp.min", nsims.samp] %>% array(., dim = c(length(nsims.samp), 20, J))
  B.temp.prec7 <- mod$sims.concat["B.temp.prec7", nsims.samp] %>% array(., dim = c(length(nsims.samp), 20, J))
  
  # Non-interactive effects #
  for(v in 1:length(vars.to.plot.PSR)) {
    z <- seq(quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
             quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
             length.out = 20)
    x <- z * X.sd[vars.to.plot.PSR[v]] + X.mn[vars.to.plot.PSR[v]]
    z.arr <- z %>%
      array(., dim = c(length(x), J, length(nsims.samp))) %>%
      aperm(c(3, 1, 2))
    B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(length(nsims.samp), length(x), J))
    B1 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v]), nsims.samp] %>% array(., dim = c(length(nsims.samp), length(x), J))
    if(vars.quad.PSR[v]) {
      B2 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v], "2"), nsims.samp] %>% array(., dim = c(length(nsims.samp), length(x), J))
    } else {
      B2 <- rep(0, nsims/10) %>% array(., dim = c(length(nsims.samp), length(x), J))
    }
    
    DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2) + B.DOS*z.DOS + B.DOS2*z.DOS2 + B.temp.min*z.temp.min + B.temp.prec7*z.temp.prec7)
    PSR <- apply(DSR, c(1, 2), function(x) {
      v <- numeric(length = length(st))
      for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
      return(mean(v))
    })
    dat.plot <- data.frame(x, z, PSR.md = apply(PSR, 2, median),
                           PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
                           PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
    assign(str_c("dat.plot.", vars.to.plot.PSR[v], ".", spp), dat.plot)
  }
  
  # Interaction #
  z1 <- seq(quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
                     quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
                     length.out = 20)
  z2 <- X[,,"Shrub_All_5m"][!is.na(ymat)]
  z2.0 <- min(z2)
  z2.not0 <- z2[-which(z2 == min(z2))]
  z2 <- seq(quantile(z2.not0, prob = 0.01, type = 8, na.rm = T),
            quantile(z2.not0, prob = 0.99, type = 8, na.rm = T), length.out = 20)
  dat.plot <- rbind(c(z1 = 0, z2 = z2.0), expand.grid(z1 = z1, z2 = z2))
  dat.plot$x1 <- dat.plot$z1 * X.sd["Max_Shrub_Height_5m"] + X.mn["Max_Shrub_Height_5m"]
  dat.plot$x2 <- dat.plot$z2 * X.sd["Shrub_All_5m"] + X.mn["Shrub_All_5m"]
  z1.arr <- dat.plot$z1 %>%
    array(., dim = c(nrow(dat.plot), J, length(nsims.samp))) %>%
    aperm(c(3, 1, 2))
  z2.arr <- dat.plot$z2 %>%
    array(., dim = c(nrow(dat.plot), J, length(nsims.samp))) %>%
    aperm(c(3, 1, 2))
  B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  B1 <- mod$sims.concat["B.Max_Shrub_Height_5m", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  B12 <- mod$sims.concat["B.Max_Shrub_Height_5m2", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  B2 <- mod$sims.concat["B.Shrub_All_5m", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  B22 <- mod$sims.concat["B.Shrub_All_5m2", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  B3 <- mod$sims.concat["B.Max_Shrub_Height_5mXShrub_All_5m", nsims.samp] %>% array(., dim = c(length(nsims.samp), nrow(dat.plot), J))
  
  B.DOS <- B.DOS[,1,] %>% array(dim = c(dim(B.DOS)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  z.DOS <- z.DOS[,1,] %>% array(dim = c(dim(z.DOS)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  B.DOS2 <- B.DOS2[,1,] %>% array(dim = c(dim(B.DOS2)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  z.DOS2 <- z.DOS2[,1,] %>% array(dim = c(dim(z.DOS2)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  B.temp.min <- B.temp.min[,1,] %>% array(dim = c(dim(B.temp.min)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  z.temp.min <- z.temp.min[,1,] %>% array(dim = c(dim(z.temp.min)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  B.temp.prec7 <- B.temp.prec7[,1,] %>% array(dim = c(dim(B.temp.prec7)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  z.temp.prec7 <- z.temp.prec7[,1,] %>% array(dim = c(dim(z.temp.prec7)[c(1, 3)], nrow(dat.plot))) %>% aperm(c(1, 3, 2))
  DSR <- expit(B0 + B1*z1.arr + B12*(z1.arr^2) + B2*z2.arr + B22*(z2.arr^2) + B3*z1.arr*z2.arr +
                 B.DOS*z.DOS + B.DOS2*z.DOS2 + B.temp.min*z.temp.min + B.temp.prec7*z.temp.prec7)
  PSR <- apply(DSR, c(1, 2), function(x) {
    v <- numeric(length = length(st))
    for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
    return(mean(v))
  })
  dat.plot <- dat.plot %>%
    mutate(PSR.md = apply(PSR, 2, median),
           PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
           PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
  assign(str_c("dat.plot.Max_Shrub_Height_5mXShrub_All_5m.", spp), dat.plot)
}

rm(mod, B0, B1, B12, B2, B22, B3, B.DOS, B.DOS2, B.temp.min, B.temp.prec7, x, z, z1, z2, z.arr, z1.arr, z2.arr, z.DOS,
   z.DOS2, z.temp.min, z.temp.prec7, DSR, PSR, st, end, dat.plot, data.spp, v)
save.image("Big_cheese_cache.RData")
