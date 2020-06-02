library(tidyverse)
library(R.utils)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

load("Data_compiled_MissingCovsImputed.RData")
mod.nam <- "BigCheese"
mod.BAIS <- loadObject(str_c("mod_mcmcR_", mod.nam, "_BAIS"))
mod.GRSP <- loadObject(str_c("mod_mcmcR_", mod.nam, "_GRSP"))

rows <- dimnames(mod.BAIS$sims.concat)[[1]]
cols <- c("BAIS", "GRSP")
out <- matrix(NA, nrow = length(rows), ncol = length(cols),
              dimnames = list(rows, cols))

sum.fn <- function(x) {
  md <- median(x)
  lo <- quantile(x, prob = 0.025, type = 8)
  hi <- quantile(x, prob = 0.975, type = 8)
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

for(sp in c("BAIS", "GRSP")) {
  mod <- eval(as.name(str_c("mod.", sp)))
  x.sum <- apply(mod$sims.concat, 1, sum.fn)
  out[names(x.sum), sp] <- x.sum
}

write.csv(out, str_c("Mod_estimates_", mod.nam, ".csv"), row.names = T)
