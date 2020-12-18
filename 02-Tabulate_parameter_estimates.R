library(tidyverse)
library(R.utils)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

## Summary function ##
sum.fn <- function(x, ndig = 2) {
  md <- median(x)
  lo <- quantile(x, prob = 0.05, type = 8)
  hi <- quantile(x, prob = 0.95, type = 8)
  x.sum <- ifelse(lo > 0 | hi < 0,
                  str_c(round(md, digits = ndig),
                        " (",
                        round(lo, digits = ndig),
                        ",",
                        round(hi, digits = ndig),
                        ")*"),
                  str_c(round(md, digits = ndig),
                        " (",
                        round(lo, digits = ndig),
                        ",",
                        round(hi, digits = ndig),
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
mod.BAIS <- loadObject(str_c("mod_CJSRL_SiteXSeason_Transmitter_", spp))
source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))
X.nams.BAIS <- X.nams

spp <- "GRSP"
mod.GRSP <- loadObject(str_c("mod_CJSRL_SiteXSeason_Transmitter_", spp))
source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))
X.nams.GRSP <- X.nams

rows <- c("DSR", "PSR", "B0.mean", "B0.sd", "P.trans", "B.trans", str_c("B.", unique(c(X.nams.BAIS, X.nams.GRSP))), "psi", "p")
cols <- c("BAIS", "GRSP")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

for(sp in c("BAIS", "GRSP")) {
  mod <- eval(as.name(str_c("mod.", sp)))
  DSR <- QSLpersonal::expit(mod$sims.list$B0.mean)
  out["DSR", sp] <- sum.fn(DSR, ndig = 3)
  out["PSR", sp] <- sum.fn(DSR^90)
  out["B0.mean", sp] <- sum.fn(mod$sims.list$B0.mean)
  out["B0.sd", sp] <- sum.fn(mod$sims.list$B0.sd)
  out["P.trans", sp] <- sum.fn(mod$sims.list$P.trans)
  out["B.trans", sp] <- sum.fn(mod$sims.list$B.trans)
  X.nams <- eval(as.name(str_c("X.nams.", sp)))
  out[str_c("B.", X.nams), sp] <- apply(mod$sims.list$B, 2, sum.fn)
  out["psi", sp] <- sum.fn(mod$sims.list$psi)
  out["p", sp] <- sum.fn(mod$sims.list$p)
}

write.csv(out, "Mod_estimates_SiteXSeason_Transmitter.csv", row.names = T)

# Transmitter effect #
rows <- c("DSR.pre", "DSR.post", "PSR.pre", "PSR.post")
cols <- c("BAIS", "GRSP")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

for(sp in c("BAIS", "GRSP")) {
  mod <- eval(as.name(str_c("mod.", sp)))
  DSR <- QSLpersonal::expit(mod$sims.list$B0.mean + mod$sims.list$B.trans)
  out["DSR.pre", sp] <- sum.fn(DSR, ndig = 3)
  out["PSR.pre", sp] <- sum.fn(DSR^90)
  DSR <- QSLpersonal::expit(mod$sims.list$B0.mean)
  out["DSR.post", sp] <- sum.fn(DSR, ndig = 3)
  out["PSR.post", sp] <- sum.fn(DSR^90)
}

write.csv(out, "Surv_estimates_Transmitter.csv", row.names = T)

## Daily and 90-day survival estimates ##
load("Data_compiled_MissingCovsImputed.RData")
mods <- c("BigCheese_BAIS","BigCheese_GRSP")
rows <- c("Daily", "7-day", "30-day", "60-day", "90-day")
out <- matrix("", nrow = length(rows), ncol = length(mods),
              dimnames = list(rows, mods))

for(m in mods) {
  mod <- loadObject(str_c("mod_mcmcR_", m))
  nsim <- dim(mod$sims.concat)[2]
  nsim.samp <- sample(nsim, 10000, replace = F)
  npar <- dim(mod$sims.concat)[1] - 2
  spp <- str_sub(m, -4, -1)
  source("grass-surv-scripts/Data_processing_BigCheese.R")
  ndays <- dim(X)[2]
  nind <- dim(X)[1]
  DSR <- PSR07 <- PSR30 <- PSR60 <- PSR90 <- matrix(NA, nrow = length(nsim.samp), ncol = nind)
  for(i in 1:ncol(DSR)) {
    dsr <- matrix(NA, nrow = length(nsim.samp), ncol = ndays)
    for(t in 1:dim(X)[2]) {
      x <- X[i,t,] %>% array(dim = c(npar, length(nsim.samp)))
      dsr[, t] <- QSLpersonal::expit(apply(mod$sims.concat[1:npar, nsim.samp] * x, 2, sum))
    }
    DSR[,i] <- apply(dsr, 1, mean)
    PSR07[,i] <- apply(dsr, 1, function(x) {
      st <- 1:(ndays-6)
      end <- 7:ndays
      xind <- sample(1:length(st), 1)
      psr <- prod(x[st[xind]:end[xind]])
      return(psr)
    })
    PSR30[,i] <- apply(dsr, 1, function(x) {
      st <- 1:(ndays-29)
      end <- 30:ndays
      xind <- sample(1:length(st), 1)
      psr <- prod(x[st[xind]:end[xind]])
      return(psr)
    })
    PSR60[,i] <- apply(dsr, 1, function(x) {
      st <- 1:(ndays-59)
      end <- 60:ndays
      xind <- sample(1:length(st), 1)
      psr <- prod(x[st[xind]:end[xind]])
      return(psr)
    })
    PSR90[,i] <- apply(dsr, 1, function(x) {
      st <- 1:(ndays-89)
      end <- 90:ndays
      xind <- sample(1:length(st), 1)
      psr <- prod(x[st[xind]:end[xind]])
      return(psr)
    })
  }
  out["Daily", m] <- sum.fn(apply(DSR, 1, mean), ndig = 3)
  out["7-day", m] <- sum.fn(apply(PSR07, 1, mean))
  out["30-day", m] <- sum.fn(apply(PSR30, 1, mean))
  out["60-day", m] <- sum.fn(apply(PSR60, 1, mean))
  out["90-day", m] <- sum.fn(apply(PSR90, 1, mean))
}

write.csv(out, "Mean_survival_estimates.csv", row.names = T)
