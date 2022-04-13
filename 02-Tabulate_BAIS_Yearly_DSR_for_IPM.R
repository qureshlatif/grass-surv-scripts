library(tidyverse)
library(R.utils)

setwd("C:/Users/Quresh.Latif/files/projects/grasslands/WintSurv")

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

load("Data_compiled_MissingCovsImputed.RData")

spp <- "BAIS"
mod <- loadObject(str_c("mod_CJSRL_SiteXSeason_noCUZA1516_only_", spp))
chop.init <- 8
chop.CUZA1516 <- TRUE # Set to true if excluding CUZA 2015-2016 (Only works if chop.init is not NULL)

source(str_c("grass-surv-scripts/Data_processing_JAGS.r"))

rows <- sort(unique(SiteSeasons$Season))
cols <- c("logit.DSR.mean", "logit.DSR.SD")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

logit.DSR <- mod$sims.list$B0
out[,"logit.DSR.mean"] <- apply(logit.DSR, 3, mean)
out[,"logit.DSR.SD"] <- apply(logit.DSR, 3, sd)

write.csv(out, "Mod_estimates_SiteXSeason_no_covs_BAIS_4_IPM.csv", row.names = T)
