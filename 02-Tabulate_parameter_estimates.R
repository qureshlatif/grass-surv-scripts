library(tidyverse)
library(R.utils)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

## Summary function ##
sum.fn <- function(x) {
  md <- median(x)
  lo <- quantile(x, prob = 0.05, type = 8)
  hi <- quantile(x, prob = 0.95, type = 8)
  x.sum <- ifelse(lo > 0 | hi < 0,
                  str_c(round(md, digits = 2),
                        " (",
                        round(lo, digits = 2),
                        ",",
                        round(hi, digits = 2),
                        ")*"),
                  str_c(round(md, digits = 2),
                        " (",
                        round(lo, digits = 2),
                        ",",
                        round(hi, digits = 2),
                        ")"))
  return(x.sum)
}                          

## Regularized covariate models ##
load("Data_compiled_MissingCovsImputed.RData")
mod.nam <- "ShrubSpp"
mod.BAIS <- loadObject(str_c("mod_mcmcR_", mod.nam, "_BAIS"))
mod.GRSP <- loadObject(str_c("mod_mcmcR_", mod.nam, "_GRSP"))

rows <- unique(c(dimnames(mod.BAIS$sims.concat)[[1]], dimnames(mod.GRSP$sims.concat)[[1]]))
cols <- c("BAIS", "GRSP")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

for(sp in c("BAIS", "GRSP")) {
  mod <- eval(as.name(str_c("mod.", sp)))
  x.sum <- apply(mod$sims.concat, 1, sum.fn)
  out[names(x.sum), sp] <- x.sum
}

write.csv(out, str_c("Mod_estimates_", mod.nam, ".csv"), row.names = T)

## JAGS models with supported covariates and random effect ##
load("Data_compiled_MissingCovsImputed.RData")
spp <- "BAIS"
mod.BAIS <- loadObject(str_c("mod_CJSRL_SiteXSeason_", spp))
source(str_c("grass-surv-scripts/Data_processing_JAGS_", spp, ".r"))
X.nams.BAIS <- X.nams

spp <- "GRSP"
mod.GRSP <- loadObject(str_c("mod_CJSRL_SiteXSeason_", spp))
source(str_c("grass-surv-scripts/Data_processing_JAGS_", spp, ".r"))
X.nams.GRSP <- X.nams
rm(X.nams)

rows <- c("B0.mean", "B0.sd", str_c("B.", unique(c(X.nams.BAIS, X.nams.GRSP))), "psi", "p")
cols <- c("BAIS", "GRSP")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

for(sp in c("BAIS", "GRSP")) {
  mod <- eval(as.name(str_c("mod.", sp)))
  out["B0.mean", sp] <- sum.fn(mod$sims.list$B0.mean)
  out["B0.sd", sp] <- sum.fn(mod$sims.list$B0.sd)
  X.nams <- eval(as.name(str_c("X.nams.", sp)))
  out[str_c("B.", X.nams), sp] <- apply(mod$sims.list$B, 2, sum.fn)
  out["psi", sp] <- sum.fn(mod$sims.list$psi)
  out["p", sp] <- sum.fn(mod$sims.list$p)
}

write.csv(out, "Mod_estimates_SiteXSeason.csv", row.names = T)
