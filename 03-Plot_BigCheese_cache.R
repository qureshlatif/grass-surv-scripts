library(R.utils)
library(tidyverse)
library(cowplot)
library(QSLpersonal)
library(abind)
theme_set(theme_cowplot())

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"

###################
# Baird's Sparrow #
###################

spp <- "BAIS"
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
nsims <- dim(mod$sims.concat)[2]
vars.to.plot.DSR <- c("DOS")
vars.quad.DSR <- c(F)
vars.to.plot.PSR <- c("hierbas", "pasto_ht_cv", "Shrub_All_5m_CV", "Max_Shrub_Height_50m_CV",
                      "Max_Shrub_Height_500m_CV", "peso", "prey", "LOSH")
vars.quad.PSR <- c(T, F, F, F,
                   F, F, F, F)

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
  assign(str_c("dat.plot.", vars.to.plot.DSR[v]), dat.plot)
}

## Seasonally fixed covariates ##
st <- 1:(J-89)
end <- 90:J
nsims.samp <- sample(nsims, nsims/10, replace = F)
z.DOS <- apply(X[,,"DOS"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
z.DOS2 <- z.DOS^2
z.temp.min <- apply(X[,,"temp.min"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
z.temp.prec7 <- apply(X[,,"temp.prec7"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
B.DOS <- mod$sims.concat["B.DOS", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.DOS2 <- mod$sims.concat["B.DOS2", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.temp.min <- mod$sims.concat["B.temp.min", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.temp.prec7 <- mod$sims.concat["B.temp.prec7", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))

  # Non-interactive effects #
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

  # Interaction #
z1 <- c(0, rep(seq(quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
         quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
         length.out = 20), 2))
z2 <- X[,,"Shrub_All_5m"][!is.na(ymat)]
z2.0 <- min(z2)
z2.not0 <- z2[-which(z2 == min(z2))]
z2 <- c(z2.0,
        rep(quantile(z2.not0, prob = 0.01, type = 8, na.rm = T), 20),
        rep(quantile(z2.not0, prob = 0.99, type = 8, na.rm = T), 20))
x1 <- z1 * X.sd["Max_Shrub_Height_5m"] + X.mn["Max_Shrub_Height_5m"]
x2 <- z2 * X.sd["Shrub_All_5m"] + X.mn["Shrub_All_5m"]
z1.arr <- z1 %>%
  array(., dim = c(length(x1), J, nsims/10)) %>%
  aperm(c(3, 1, 2))
z2.arr <- z2 %>%
  array(., dim = c(length(x1), J, nsims/10)) %>%
  aperm(c(3, 1, 2))
B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B1 <- mod$sims.concat["B.Max_Shrub_Height_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B12 <- mod$sims.concat["B.Max_Shrub_Height_5m2", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B2 <- mod$sims.concat["B.Shrub_All_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B22 <- mod$sims.concat["B.Shrub_All_5m2", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B3 <- mod$sims.concat["B.Max_Shrub_Height_5mXShrub_All_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))

DSR <- expit(B0 + B1*z1.arr + B12*(z1.arr^2) + B2*z2.arr + B22*(z2.arr^2) + B3*z1.arr*z2.arr +
               abind(B.DOS[,1,], B.DOS, B.DOS, along = 2)*abind(z.DOS, z.DOS[,1,], z.DOS, along = 2) +
               abind(B.DOS2[,1,], B.DOS2, B.DOS2, along = 2)*abind(z.DOS2, z.DOS2[,1,], z.DOS2, along = 2) +
               abind(B.temp.min[,1,], B.temp.min, B.temp.min, along = 2)*abind(z.temp.min[,1,], z.temp.min, z.temp.min, along = 2) +
               abind(B.temp.prec7[,1,], B.temp.prec7, B.temp.prec7, along = 2)*abind(z.temp.prec7[,1,], z.temp.prec7, z.temp.prec7, along = 2))
PSR <- apply(DSR, c(1, 2), function(x) {
  v <- numeric(length = length(st))
  for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
  return(mean(v))
})
dat.plot <- data.frame(x1, x2, z1, z2, PSR.md = apply(PSR, 2, median),
                       PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
                       PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
assign("dat.plot.Max_Shrub_Height_5mXShrub_All_5m", dat.plot)

rm(mod, B0, B1, B12, B2, B22, B3, B.DOS, B.DOS2, B.temp.min, B.temp.prec7, x, x1, x2, z, z1, z2, z.arr, z1.arr, z2.arr, z.DOS,
   z.DOS2, z.temp.min, z.temp.prec7, DSR, PSR, st, end, dat.plot, data.spp, v)
save.image("Big_cheese_BAIS_cache.RData")

#######################
# Grasshopper Sparrow #
#######################
if(any(str_detect(ls(), "dat.plot"))) rm(list = ls()[which(str_detect(ls(), "dat.plot"))])

spp <- "GRSP" # BAIS or GRSP
mod.nam <- "BigCheese"
mod <- loadObject(str_c("mod_mcmcR_", mod.nam, "_", spp))
nsims <- dim(mod$sims.concat)[2]
vars.to.plot.PSR <- c("otra", "hierbas_cv", "pasto_ht_cv", "Shrub_All_5m_CV",
                      "Shrub_All_50m_CV", "Max_Shrub_Height_50m_CV", "prey")
vars.quad.PSR <- c(T, F, F, F,
                   F, F, F)

source(str_c(scripts.loc, "Data_processing_", mod.nam,".R"))
dimnames(X)[[3]] <- X.nams

# Time-varying covariates (none) #
nsims <- dim(mod$sims.concat)[2]
# for(v in 1:length(vars.to.plot.DSR)) {
#   z <- seq(quantile(X[,,vars.to.plot.DSR[v]][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
#            quantile(X[,,vars.to.plot.DSR[v]][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
#            length.out = 20)
#   x <- z * X.sd[vars.to.plot.DSR[v]] + X.mn[vars.to.plot.DSR[v]]
#   z.arr <- z %>%
#     array(., dim = c(length(x), nsims)) %>%
#     aperm(c(2, 1))
#   B0 <- mod$sims.concat["B.Intercept", ] %>% array(., dim = c(nsims, length(x)))
#   B1 <- mod$sims.concat[str_c("B.", vars.to.plot.DSR[v]), ] %>% array(., dim = c(nsims, length(x)))
#   if(vars.quad.DSR[v]) {
#     B2 <- mod$sims.concat[str_c("B.", vars.to.plot.DSR[v], "2"), ] %>% array(., dim = c(nsims, length(x)))
#   } else {
#     B2 <- rep(0, nsims) %>% array(., dim = c(nsims, length(x)))
#   }
#   DSR <- expit(B0 + B1*z.arr + B2*(z.arr^2))
#   dat.plot <- data.frame(x, z, DSR.md = apply(DSR, 2, median),
#                          DSR.lo = apply(DSR, 2, function(x) quantile(x, prob = 0.05)),
#                          DSR.hi = apply(DSR, 2, function(x) quantile(x, prob = 0.95)))
#   assign(str_c("dat.plot.", vars.to.plot.DSR[v]), dat.plot)
# }

## Seasonally fixed covariates ##
st <- 1:(J-89)
end <- 90:J
nsims.samp <- sample(nsims, nsims/10, replace = F)
z.DOS <- apply(X[,,"DOS"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
z.DOS2 <- z.DOS^2
z.temp.min <- apply(X[,,"temp.min"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
z.temp.prec7 <- apply(X[,,"temp.prec7"], 2, mean) %>% array(., dim = c(J, nsims/10, 20)) %>%
  aperm(c(2, 3, 1))
B.DOS <- mod$sims.concat["B.DOS", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.DOS2 <- mod$sims.concat["B.DOS2", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.temp.min <- mod$sims.concat["B.temp.min", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))
B.temp.prec7 <- mod$sims.concat["B.temp.prec7", nsims.samp] %>% array(., dim = c(nsims/10, 20, J))

# Non-interactive effects #
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

# Interaction #
z1 <- c(0, rep(seq(quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.01, type = 8, na.rm = T),
                   quantile(X[,,"Max_Shrub_Height_5m"][!is.na(ymat)], prob = 0.99, type = 8, na.rm = T),
                   length.out = 20), 2))
z2 <- X[,,"Shrub_All_5m"][!is.na(ymat)]
z2.0 <- min(z2)
z2.not0 <- z2[-which(z2 == min(z2))]
z2 <- c(z2.0,
        rep(quantile(z2.not0, prob = 0.01, type = 8, na.rm = T), 20),
        rep(quantile(z2.not0, prob = 0.99, type = 8, na.rm = T), 20))
x1 <- z1 * X.sd["Max_Shrub_Height_5m"] + X.mn["Max_Shrub_Height_5m"]
x2 <- z2 * X.sd["Shrub_All_5m"] + X.mn["Shrub_All_5m"]
z1.arr <- z1 %>%
  array(., dim = c(length(x1), J, nsims/10)) %>%
  aperm(c(3, 1, 2))
z2.arr <- z2 %>%
  array(., dim = c(length(x1), J, nsims/10)) %>%
  aperm(c(3, 1, 2))
B0 <- mod$sims.concat["B.Intercept", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B1 <- mod$sims.concat["B.Max_Shrub_Height_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B12 <- mod$sims.concat["B.Max_Shrub_Height_5m2", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B2 <- mod$sims.concat["B.Shrub_All_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B22 <- mod$sims.concat["B.Shrub_All_5m2", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))
B3 <- mod$sims.concat["B.Max_Shrub_Height_5mXShrub_All_5m", nsims.samp] %>% array(., dim = c(nsims/10, length(x1), J))

DSR <- expit(B0 + B1*z1.arr + B12*(z1.arr^2) + B2*z2.arr + B22*(z2.arr^2) + B3*z1.arr*z2.arr +
               abind(B.DOS[,1,], B.DOS, B.DOS, along = 2)*abind(z.DOS, z.DOS[,1,], z.DOS, along = 2) +
               abind(B.DOS2[,1,], B.DOS2, B.DOS2, along = 2)*abind(z.DOS2, z.DOS2[,1,], z.DOS2, along = 2) +
               abind(B.temp.min[,1,], B.temp.min, B.temp.min, along = 2)*abind(z.temp.min[,1,], z.temp.min, z.temp.min, along = 2) +
               abind(B.temp.prec7[,1,], B.temp.prec7, B.temp.prec7, along = 2)*abind(z.temp.prec7[,1,], z.temp.prec7, z.temp.prec7, along = 2))
PSR <- apply(DSR, c(1, 2), function(x) {
  v <- numeric(length = length(st))
  for(j in 1:length(st)) v[j] <- prod(x[st[j]:end[j]])
  return(mean(v))
})
dat.plot <- data.frame(x1, x2, z1, z2, PSR.md = apply(PSR, 2, median),
                       PSR.lo = apply(PSR, 2, function(x) quantile(x, prob = 0.05)),
                       PSR.hi = apply(PSR, 2, function(x) quantile(x, prob = 0.95)))
assign("dat.plot.Max_Shrub_Height_5mXShrub_All_5m", dat.plot)

rm(mod, B0, B1, B12, B2, B22, B3, B.DOS, B.DOS2, B.temp.min, B.temp.prec7, x, x1, x2, z, z1, z2, z.arr, z1.arr, z2.arr, z.DOS,
   z.DOS2, z.temp.min, z.temp.prec7, DSR, PSR, st, end, dat.plot, data.spp)
save.image("Big_cheese_GRSP_cache.RData")

#__________________________________________#
