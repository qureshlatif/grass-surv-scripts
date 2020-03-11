library(stringr)
library(dplyr)
library(QSLpersonal)

#setwd("/home/RMBO.LOCAL/quresh.latif/CPW_beetle")
setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
load("Data_compiled.RData")

## Baird's Sparrow ##
data.BAIS$Covs %>% select(hierbas:pastos_cv) %>% cor(use = "complete")
# dump desnudo - r = -0.94 with pastos, r = -0.61 with pasto_ht
# dump hierba_ht - r = 0.66 with hierbas, r = 0.69 with pastos, r = -0.64 with desnudo
# dump arbusto_ht - r = 0.76 with arbusto, r = -0.67 with arbusto_cv
# dump pastos_cv - r = -0.80 with pastos
VIF(data.BAIS$Covs %>% select(hierbas:pastos_cv))
#    hierbas    hierba_ht    arbusto    arbusto_ht     pastos      pasto_ht    salsola    salsola_ht    desnudo     arbusto_cv  pastos_cv 
#    2.345361   2.992237     2.391387   3.356929       18.264891   2.041560    1.286665   1.536323      17.335484   1.994888    3.105700
VIF(data.BAIS$Covs %>% select(hierbas, arbusto, pastos:salsola_ht, arbusto_cv))
#hierbas    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
#1.332275   1.336457    1.905944   1.764161    1.151801   1.233153   1.593890
VIF(data.BAIS$Covs %>% select(hierbas:arbusto, pastos:salsola_ht, arbusto_cv)) # Could keep hierba_ht For consistency with GRSP in prelim models.
#hierbas    hierba_ht    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
#1.837260   2.903369     1.354274    2.451714   1.811199    1.156155   1.239830   1.617221 
data.BAIS$Covs %>% select(peso, grasa) %>% cor(use = "complete")
# peso X grasa r = 0.73, just using peso

perc.samp <- (data.BAIS$Covs %>% select(hierbas_n:desnudo_n) %>% data.matrix) / apply(data.BAIS$ymat, 1, function(x) sum(!is.na(x)))
cols <- c("mean", "SD", "min", "max", "prp_gt30", "cor")
rows <- str_remove(dimnames(perc.samp)[[2]], "_n")
out <- matrix(NA, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))
for(i in 1:length(rows)) {
  ind <- which(!is.na(perc.samp[, i]) & perc.samp[, i] != 0)
  vals <- perc.samp[ind, i]
  out[i, ] <- c(mean(vals), sd(vals), min(vals), max(vals), sum(vals > 0.3) / length(vals), cor(vals, data.BAIS$Covs[ind, rows[i]]))
}
rm(vals, i)
write.csv(out, "Percent_days_veg_measured_BAIS.csv")
  
## Grasshopper Sparrow ##
data.GRSP$Covs %>% select(hierbas:pastos_cv) %>% cor(use = "complete")
# dump desnudo - r = -0.93 with pastos, r = -0.64 with hierba_ht, r = -0.61 with pasto_ht
# dump arbusto_ht - r = 0.76 with arbusto, r = -0.67 with arbusto_cv
# dump pastos_cv - r = -0.67 with pastos
VIF(data.GRSP$Covs %>% select(hierbas:pastos_cv))
#hierbas    hierba_ht    arbusto    arbusto_ht     pastos      pasto_ht    salsola    salsola_ht    desnudo     arbusto_cv  pastos_cv 
#1.439720   1.864989     2.643957   3.827554       10.173471   1.851443    1.233285   1.262147      11.619710   1.957617    1.954822 
VIF(data.GRSP$Covs %>% select(hierbas:arbusto, pastos:salsola_ht, arbusto_cv))
#hierbas    hierba_ht    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
#1.296187   1.807718     1.387014    1.948040   1.732345    1.055154   1.217699   1.285592 
data.GRSP$Covs %>% select(peso, grasa) %>% cor(use = "complete")
# peso X grasa r = 0.61, just using peso

perc.samp <- (data.GRSP$Covs %>% select(hierbas_n:desnudo_n) %>% data.matrix) / apply(data.GRSP$ymat, 1, function(x) sum(!is.na(x)))
cols <- c("mean", "SD", "min", "max", "prp_gt30", "cor")
rows <- str_remove(dimnames(perc.samp)[[2]], "_n")
out <- matrix(NA, nrow = length(rows), ncol = length(cols), dimnames = list(rows, cols))
for(i in 1:length(rows)) {
  ind <- which(!is.na(perc.samp[, i]) & perc.samp[, i] != 0)
  vals <- perc.samp[ind, i]
  out[i, ] <- c(mean(vals), sd(vals), min(vals), max(vals), sum(vals > 0.3) / length(vals), cor(vals, data.GRSP$Covs[ind, rows[i]]))
}
rm(vals, i)
write.csv(out, "Percent_days_veg_measured_GRSP.csv")
