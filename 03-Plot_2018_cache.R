library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_2018.RData")
scripts.loc <- "grass-surv-scripts/"

vars.to.plot.PSR <- c("Grass_50m", "Max_Shrub_Height_500m_CV")
vars.quad.PSR <- c(T, F)

spp.vec <- c("BAIS", "GRSP") # BAIS or GRSP
mod.nam <- "2018"

for(spp in spp.vec) {
  
  mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
  nsims <- dim(mod$sims.concat)[2]
  
  source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
  dimnames(X)[[3]] <- X.nams

  ## Seasonally fixed covariates ##
  st <- 1:(J-89)
  end <- 90:J
  nsims.samp <- sample(nsims, nsims/100, replace = F)

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
    
    DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2))
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
}

rm(mod, B0, B1, B2, x, z, z.arr, DSR, PSR, st, end, dat.plot, data.spp, v)
save.image("2018_cache.RData")
