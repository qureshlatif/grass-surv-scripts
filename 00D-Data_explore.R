library(tidyverse)
library(QSLpersonal)
library(corrplot)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
#load("Data_compiled_MissingCovsImputed.RData")
load("Data_compiled.RData")

#########################################################################
# Explore and tabulate correlations among field veg and drone variables #
#########################################################################

## Baird's Sparrow ##
data.BAIS$Covs %>%
  select(hierbas:otra,
         desnudo, hierbas_cv, hierba_ht_cv,
         pastos_cv, pasto_ht_cv, otra_cv, desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  write.csv("Correlations_veg_BAIS.csv")
  #write.csv("Correlations_veg_BAIS_fillData.csv")
  
pdf("Correlations_veg_BAIS.pdf")
#pdf("Correlations_veg_BAIS_fillData.pdf")
data.BAIS$Covs %>%
  select(hierbas:otra,
         desnudo, hierbas_cv, hierba_ht_cv,
         pastos_cv, pasto_ht_cv, otra_cv, desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()

## Grasshopper Sparrow ##
data.GRSP$Covs %>%
  select(hierbas:otra,
         desnudo, hierbas_cv, hierba_ht_cv,
         pastos_cv, pasto_ht_cv, otra_cv, desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  write.csv("Correlations_veg_GRSP.csv")
  #write.csv("Correlations_veg_GRSP_fillData.csv")
  
pdf("Correlations_veg_GRSP.pdf")
#pdf("Correlations_veg_GRSP_fillData.pdf")
data.GRSP$Covs %>%
  select(hierbas:otra,
         desnudo, hierbas_cv, hierba_ht_cv,
         pastos_cv, pasto_ht_cv, otra_cv, desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>% cor(use = "pairwise.complete.obs") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()

#######################################################################
# Tabulate counts of missing values for field veg and drone variables #
#######################################################################

load("Data_compiled.RData")

## Baird's Sparrow ##
data.BAIS$Covs %>%
  select(hierbas:otra, desnudo:desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  write.csv("Missing_count_veg_BAIS.csv")

## Grasshopper Sparrow ##
data.GRSP$Covs %>%
  select(hierbas:otra, desnudo:desnudo_cv,
         Mesquite_5m:Yucca_500m,
         Distance_to_Fence,
         Shrub_All_5m:Shrub_All_500m,
         Mean_Shrub_Height_5m:Distance_to_Fence,
         Shrub_All_5m_CV:Shrub_All_500m_CV,
         Mean_Shrub_Height_5m_CV:Mean_Shrub_Height_500m_CV) %>%
  summarise_all(function(x) sum(is.na(x))) %>%
  write.csv("Missing_count_veg_GRSP.csv")

# # dump desnudo - r = -0.94 with pastos, r = -0.61 with pasto_ht
# # dump hierba_ht - r = 0.66 with hierbas, r = 0.69 with pastos, r = -0.64 with desnudo
# # dump arbusto_ht - r = 0.76 with arbusto, r = -0.67 with arbusto_cv
# # dump pastos_cv - r = -0.80 with pastos
# VIF(data.BAIS$Covs %>% select(hierbas:pastos_cv))
# #    hierbas    hierba_ht    arbusto    arbusto_ht     pastos      pasto_ht    salsola    salsola_ht    desnudo     arbusto_cv  pastos_cv 
# #    2.345361   2.992237     2.391387   3.356929       18.264891   2.041560    1.286665   1.536323      17.335484   1.994888    3.105700
# VIF(data.BAIS$Covs %>% select(hierbas, arbusto, pastos:salsola_ht, arbusto_cv))
# #hierbas    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
# #1.332275   1.336457    1.905944   1.764161    1.151801   1.233153   1.593890
# VIF(data.BAIS$Covs %>% select(hierbas:arbusto, pastos:salsola_ht, arbusto_cv)) # Could keep hierba_ht For consistency with GRSP in prelim models.
# #hierbas    hierba_ht    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
# #1.837260   2.903369     1.354274    2.451714   1.811199    1.156155   1.239830   1.617221 
# data.BAIS$Covs %>% select(peso, grasa) %>% cor(use = "complete")
# # peso X grasa r = 0.73, just using peso

perc.samp <- (data.BAIS$Covs %>% select(hierbas_n:otra_n, desnudo_n) %>% data.matrix) / apply(data.BAIS$ymat, 1, function(x) sum(!is.na(x)))
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
  
# ## Grasshopper Sparrow ##
# data.GRSP$Covs %>% select(hierbas:pastos_cv) %>% cor(use = "complete")
# # dump desnudo - r = -0.93 with pastos, r = -0.64 with hierba_ht, r = -0.61 with pasto_ht
# # dump arbusto_ht - r = 0.76 with arbusto, r = -0.67 with arbusto_cv
# # dump pastos_cv - r = -0.67 with pastos
# VIF(data.GRSP$Covs %>% select(hierbas:pastos_cv))
# #hierbas    hierba_ht    arbusto    arbusto_ht     pastos      pasto_ht    salsola    salsola_ht    desnudo     arbusto_cv  pastos_cv 
# #1.439720   1.864989     2.643957   3.827554       10.173471   1.851443    1.233285   1.262147      11.619710   1.957617    1.954822 
# VIF(data.GRSP$Covs %>% select(hierbas:arbusto, pastos:salsola_ht, arbusto_cv))
# #hierbas    hierba_ht    arbusto     pastos     pasto_ht    salsola    salsola_ht arbusto_cv 
# #1.296187   1.807718     1.387014    1.948040   1.732345    1.055154   1.217699   1.285592 
# data.GRSP$Covs %>% select(peso, grasa) %>% cor(use = "complete")
# # peso X grasa r = 0.61, just using peso

perc.samp <- (data.GRSP$Covs %>% select(hierbas_n:otra_n, desnudo_n) %>% data.matrix) / apply(data.GRSP$ymat, 1, function(x) sum(!is.na(x)))
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

###############################################################################
# Explore and tabulate correlations among all variables selected for analysis #
###############################################################################

load("Data_compiled_MissingCovsImputed.RData")
scripts.loc <- "grass-surv-scripts/"

spp <- "BAIS" # BAIS or GRSP
mod.nam <- "ShrubSpp"
source(str_c(scripts.loc, "Data_processing_", mod.nam, ".R"))

X.mat <- matrix(X, prod(dim(X)[1:2]), dim(X)[3])
dimnames(X.mat)[[2]] <- X.nams
X.mat[,-1] %>% cor(use = "complete") %>%
  write.csv(str_c("Correlations_pre-analysis_", mod.nam, "_", spp, ".csv"))
#write.csv("Correlations_veg_BAIS_fillData.csv")

pdf(str_c("Correlations_pre-analysis_", mod.nam, "_", spp, ".pdf"))
#pdf("Correlations_veg_BAIS_fillData.pdf")
X.mat[,-1] %>% cor(use = "complete") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()

# correlated pairs for BAIS (*BAIS only):
# hierbas, hierbas2 - 0.77
# arbusto, arbusto2 - 0.68
# pastos, pasto_ht - 0.65 (drop pastos and keep NDVI & pasto_ht)
# pastos, NDVI - 0.72 (see above)*
# salsola, salsola2 - 0.84
# Shrub_All_5m, Shrub_All_5m2 - 0.84
# Mean_Shrub_Height_5m, Mean_Shrub_Height_5m2 - 0.78
# raptor, prey - 0.75 (keep prey, drop raptor)

# correlated pairs for GRSP (*GRSP only):
# hierbas, hierbas2 - 0.66
# arbusto, arbusto2 - 0.64
# pastos, pasto_ht - 0.6 (drop pastos and keep NDVI & pasto_ht?)
# salsola, salsola2 - 0.72
# Shrub_All_5m, Shrub_All_5m2 - 0.72
# Mean_Shrub_Height_5m, Mean_Shrub_Height_5m2 - 0.82
# hierbas2, arbusto2 - 0.88*
# Shrub_All_500m_CV, Mean_Shrub_Height_500m_CV - 0.75*
