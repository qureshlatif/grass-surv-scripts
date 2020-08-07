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
vars.to.plot.DSR <- c("DOS", "temp.prec7")
vars.quad.DSR <- c(T, F)
vars.to.plot.PSR <- c("pastos", "Juniper_5m", "Yucca_5m", "pasto_ht_cv",
                      "Shrub_All_5m_CV", "Max_Shrub_Height_50m_CV", "Max_Shrub_Height_500m_CV",
                      "peso", "prey")
vars.quad.PSR <- c(T, T, T, F,
                   F, F, F,
                   F, F)

source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
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
  assign(str_c("dat.plot.", vars.to.plot.DSR[v]), dat.plot)
}

st <- 1:(J-89)
end <- 90:J
nsims.samp <- sample(nsims, nsims/10, replace = F)
z.DOS <- apply(X[,,"DOS"], 2, mean) %>% array(., dim = c(J, nsims/10, length(x))) %>%
  aperm(c(2, 3, 1))
z.DOS2 <- z.DOS^2
z.temp.min <- apply(X[,,"temp.min"], 2, mean) %>% array(., dim = c(J, nsims/10, length(x))) %>%
  aperm(c(2, 3, 1))
z.temp.prec7 <- apply(X[,,"temp.prec7"], 2, mean) %>% array(., dim = c(J, nsims/10, length(x))) %>%
  aperm(c(2, 3, 1))
B.DOS <- mod$sims.concat["B.DOS", nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
B.DOS2 <- mod$sims.concat["B.DOS2", nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
B.temp.min <- mod$sims.concat["B.temp.min", nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
B.temp.prec7 <- mod$sims.concat["B.temp.prec7", nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))

for(v in 1:length(vars.to.plot.PSR)) {
  z <- seq(quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
           quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
           length.out = 20)
  x <- z * X.sd[vars.to.plot.PSR[v]] + X.mn[vars.to.plot.PSR[v]]
  z.arr <- z %>%
    array(., dim = c(length(x), J, nsims/10)) %>%
    aperm(c(3, 1, 2))
  B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
  B1 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v]), nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
  if(vars.quad.PSR[v]) {
    B2 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v], "2"), nsims.samp] %>% array(., dim = c(nsims/10, length(x), J))
  } else {
    B2 <- rep(0, nsims/10) %>% array(., dim = c(nsims/10, length(x), J))
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
  assign(str_c("dat.plot.", vars.to.plot.PSR[v]), dat.plot)
}

rm(mod, B0, B1, B2, B.DOS, B.DOS2, B.temp.min, B.temp.prec7, z.arr, z.DOS,
   z.DOS2, z.temp.min, z.temp.prec7, DSR, PSR, st, end, dat.plot, data.spp, v)
save.image("ShrubSpp_BAIS_cache.RData")

#######################
# Grasshopper Sparrow #
#######################
if(any(str_detect(ls(), "dat.plot"))) rm(list = ls()[which(str_detect(ls(), "dat.plot"))])

spp <- "GRSP" # BAIS or GRSP
mod.nam <- "ShrubSpp"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
vars.to.plot.PSR <- c("Mesquite_5m", "Juniper_500m", "Yucca_5m", "otra", "hierbas_cv",
                      "pasto_ht_cv", "otra_cv", "Shrub_All_5m_CV", "Shrub_All_50m_CV",
                      "Max_Shrub_Height_50m_CV", "prey")
vars.quad.PSR <- c(T, T, T, T, F,
                   F, F, F, F,
                   F, F)

source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
dimnames(X)[[3]] <- X.nams

nsims <- dim(mod$sims.concat)[2]
nx <- 20
st <- 1:(J-89)
end <- 90:J
nsims.samp <- sample(nsims, nsims/10, replace = F)
z.DOS <- apply(X[,,"DOS"], 2, mean) %>% array(., dim = c(J, nsims/10, nx)) %>%
  aperm(c(2, 3, 1))
z.DOS2 <- z.DOS^2
z.temp.min <- apply(X[,,"temp.min"], 2, mean) %>% array(., dim = c(J, nsims/10, nx)) %>%
  aperm(c(2, 3, 1))
z.temp.prec7 <- apply(X[,,"temp.prec7"], 2, mean) %>% array(., dim = c(J, nsims/10, nx)) %>%
  aperm(c(2, 3, 1))
B.DOS <- mod$sims.concat["B.DOS", nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
B.DOS2 <- mod$sims.concat["B.DOS2", nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
B.temp.min <- mod$sims.concat["B.temp.min", nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
B.temp.prec7 <- mod$sims.concat["B.temp.prec7", nsims.samp] %>% array(., dim = c(nsims/10, nx, J))

for(v in 1:length(vars.to.plot.PSR)) {
  z <- seq(quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
           quantile(X[,,vars.to.plot.PSR[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
           length.out = 20)
  x <- z * X.sd[vars.to.plot.PSR[v]] + X.mn[vars.to.plot.PSR[v]]
  z.arr <- z %>%
    array(., dim = c(nx, J, nsims/10)) %>%
    aperm(c(3, 1, 2))
  B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
  B1 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v]), nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
  if(vars.quad.PSR[v]) {
    B2 <- mod$sims.concat[str_c("B.", vars.to.plot.PSR[v], "2"), nsims.samp] %>% array(., dim = c(nsims/10, nx, J))
  } else {
    B2 <- rep(0, nsims/10) %>% array(., dim = c(nsims/10, nx, J))
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
  assign(str_c("dat.plot.", vars.to.plot.PSR[v]), dat.plot)
}

rm(mod, B0, B1, B2, B.DOS, B.DOS2, B.temp.min, B.temp.prec7, z.arr, z.DOS,
   z.DOS2, z.temp.min, z.temp.prec7, DSR, PSR, st, end, dat.plot, data.spp, v)
save.image("ShrubSpp_GRSP_cache.RData")

#__________________________________________#
