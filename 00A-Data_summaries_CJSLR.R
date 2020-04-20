library(dplyr)
library(stringr)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

cols <- c("No_indivs", "nDays", "nDets", "prpDaysDetWhenAlive", "nMort", "nMortDay1",
          "MortConfDayMed", "MortConfDay95thQntl", "nMortConfDayGT95th", "MortConfDayMax",
          "nSurv", "nMalf", "nUnk")
out <- matrix(NA, nrow = 2, ncol = length(cols),
              dimnames = list(c("BAIS", "GRSP"), cols))

for(spp in dimnames(out)[[1]]) {
  data.spp <- eval(as.name(str_c("data.", spp)))
  out[spp, "No_indivs"] <- nrow(data.spp$ymat)
  out[spp, "nDays"] <- sum(!is.na(data.spp$ymat)) - nrow(data.spp$ymat)
  out[spp, "nDets"] <- sum(data.spp$ymat %in% c(1, 2), na.rm = T) - nrow(data.spp$ymat)
  out[spp, "prpDaysDetWhenAlive"] <- sum(data.spp$ymat == 1, na.rm = T) /
    sum(data.spp$Covs$lastAlive - data.spp$Covs$firstDay)
  out[spp, "nMort"] <- sum(data.spp$ymat == 2, na.rm = T)
  ydied <- data.spp$ymat[which(apply(data.spp$ymat, 1, function(x) any(x == 2, na.rm = T))),]
  out[spp, "nMortDay1"] <- sum(apply(ydied, 1, function(x) {
    ind.died <- which(x == 2)
    ind.pre <- max(which(x %in% c(0,1)))
    return(x[ind.pre] == 1)
  }))
  DayConfDied <- apply(ydied, 1, function(x) {
    ind.died <- which(x == 2)
    ind.lastalive <- max(which(x == 1))
    return(ind.died - ind.lastalive)
    })
  out[spp, "MortConfDayMed"] <- median(DayConfDied)
  out[spp, "MortConfDay95thQntl"] <- quantile(DayConfDied, prob = 0.95, type = 8)
  out[spp, "nMortConfDayGT95th"] <- sum(DayConfDied > out[spp, "MortConfDay95thQntl"])
  out[spp, "MortConfDayMax"] <- max(DayConfDied)
  out[spp, "nSurv"] <- sum(data.spp$Covs$resultado.final == "S")
  out[spp, "nMalf"] <- sum(data.spp$Covs$resultado.final %in% c("RC", "P"))
  out[spp, "nUnk"] <- sum(data.spp$Covs$resultado.final %in% c("Q", "U"))
}
write.csv(out, "Summary.csv", row.names = T)
