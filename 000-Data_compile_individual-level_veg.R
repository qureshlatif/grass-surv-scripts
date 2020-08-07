library(tidyverse)
library(geosphere)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

maxD.grid.join <- 100 # maximum distance for allowing a grid veg record to be joined to a bird location.

dat.locations <- read.csv("wintersurvival_accessDBs/ExportedTables/Locaciones_Todos_all_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  filter(!waypoint == "") %>%
  filter(Latitude != -999999 & Longitude != 999999) %>%
  filter(especie %in% c("BAIS", "GRSP"))
dat.veg <- read.csv("wintersurvival_accessDBs/ExportedTables/Vegetacion_forImport.csv", stringsAsFactors = F) %>%
  tbl_df() %>%
  distinct(Site, Season, waypoint, tipo, Latitude, Longitude, .keep_all = T) %>% # Dumping duplicates. Will need to investigate these at some point. Most of these are 
  rename(hierba_ht = estatura_promedia_hierbas,
         arbusto_ht = estatura_promedia_arbusto,
         pasto_ht = estatura_promedia_pasto,
         salsola_ht = estatura_promedia_salsola) %>%
  select(Site, Season, waypoint, Latitude, Longitude, tipo, hierbas, hierba_ht, arbusto, arbusto_ht,
         pastos, pasto_ht, salsola, salsola_ht, desnudo, otra, otra1) %>% # Too many missing values for robel - leaving it out unless & until instructed otherwise.
  mutate(hierbas = replace(hierbas, which(hierbas == -999), NA), # Set missing values to NA to prep for summarization.
         hierba_ht = replace(hierba_ht, which(hierba_ht == -999), NA),
         arbusto = replace(arbusto, which(arbusto == -999), NA),
         arbusto_ht = replace(arbusto_ht, which(arbusto_ht == -999), NA),
         pastos = replace(pastos, which(pastos == -999), NA),
         pasto_ht = replace(pasto_ht, which(pasto_ht == -999), NA),
         salsola = replace(salsola, which(salsola == -999), NA),
         salsola_ht = replace(salsola_ht, which(salsola_ht == -999), NA),
         desnudo = replace(desnudo, which(desnudo == -999), NA),
         otra = replace(otra, which(otra == -999), NA),
         otra1 = replace(otra1, which(otra1 %in% c("9999", "NULL")), NA)) %>%
  mutate(otra1 = replace(otra1, which(otra1 == "MA"), "VS") %>%
           as.factor)
dat.veg.bird <- dat.veg %>%
  filter(tipo == "bird")
dat.veg.grid <- dat.veg %>%
  filter(tipo == "grid") %>%
  filter(Latitude != -999999 & Longitude != 999999)

# 1. Join veg at bird locations to location records where present.
dat.locations <- dat.locations %>%
  left_join(dat.veg.bird %>%
              select(Site, Season, waypoint, hierbas, hierba_ht, arbusto, arbusto_ht,
                     pastos, pasto_ht, salsola, salsola_ht, otra, otra1, desnudo),
            by = c("Site", "Season", "waypoint"))

# # 2. Join grid veg to location records where present and within 'maxD.grid.join'.
# Site_Season_combos <- dat.locations %>% select(Site, Season) %>% distinct
# for(i in 1:nrow(Site_Season_combos)) {
#   locs <- dat.locations %>% filter(Site == Site_Season_combos$Site[i] & Season == Site_Season_combos$Season[i])
#   veg <- dat.veg.grid %>% filter(Site == Site_Season_combos$Site[i] & Season == Site_Season_combos$Season[i])
#   names(veg)[which(names(veg) %in% c("hierbas", "hierba_ht", "arbusto", "arbusto_ht",
#                                      "pastos", "pasto_ht", "salsola", "salsola_ht",
#                                      "otra", "otra1", "desnudo"))] <-
#     str_c(names(veg)[which(names(veg) %in% c("hierbas", "hierba_ht", "arbusto", "arbusto_ht",
#                                        "pastos", "pasto_ht", "salsola", "salsola_ht",
#                                        "otra", "otra1", "desnudo"))], "_grid")
#   if(nrow(veg) > 0) {
#     D <- distm(locs[, c("Longitude", "Latitude")], veg[, c("Longitude", "Latitude")])
#     minD <- apply(D, 1, min)
#     minD.ind <- apply(D, 1, function(x) which(x == min(x)))
#     veg.keep <- veg %>% slice(minD.ind) %>%
#       mutate_at(vars(names(veg)[7:length(names(veg))]), (function(x) replace(x, which(minD > maxD.grid.join), NA))) %>%
#       mutate(Distance = minD)
#     locs <- locs %>% bind_cols(
#       veg.keep %>% select(hierbas_grid:Distance)
#     )
#   }
#   if(i == 1) dat.locs <- locs
#   if(i > 1) dat.locs <- dat.locs %>%
#     bind_rows(locs)
# }
# dat.locations <- dat.locs
# rm(dat.locs, locs, veg, D, minD, minD.ind, veg.keep, i, Site_Season_combos)

# 3. Summarize (mean, SD) bird- and grid-based versions of covariates.
dat.indveg <- dat.locations %>%
  group_by(Site, Season, anillo) %>%
  mutate(otra1 = fct_infreq(otra1)) %>%
  summarise(hierbas = mean(hierbas, na.rm = T),
            pastos = mean(pastos, na.rm = T),
            salsola = mean(salsola, na.rm = T),
            otra = mean(otra, na.rm = T),
            otra1 = first(otra1),
            desnudo = mean(desnudo, na.rm = T)) %>%
  ungroup()

dat.indveg <- dat.indveg %>% left_join(
  dat.locations %>%
  group_by(Site, Season, anillo) %>%
  summarise(hierba_ht = ifelse(any(hierbas > 0, na.rm = T),
                               sum(hierba_ht * hierbas, na.rm = T) / sum(hierbas, na.rm = T), NA),
            pasto_ht = ifelse(any(pastos > 0, na.rm = T), sum(pasto_ht * pastos, na.rm = T) / sum(pastos, na.rm = T),
                              NA)),
  by = c("Site", "Season", "anillo"))

dat.indveg <- dat.indveg %>%
  left_join(dat.locations %>%
              group_by(Site, Season, anillo) %>%
              summarise(hierbas_sd = sd(hierbas, na.rm = T),
                        pastos_sd = sd(pastos, na.rm = T),
                        salsola_sd = sd(salsola, na.rm = T),
                        otra_sd = sd(otra, na.rm = T),
                        desnudo_sd = sd(desnudo, na.rm = T)),
            by = c("Site", "Season", "anillo"))

dat.indveg <- dat.indveg %>%
  left_join(dat.locations %>%
              group_by(Site, Season, anillo) %>%
              summarise(hierba_ht_sd = ifelse(any(hierbas > 0, na.rm = T),
                                              sqrt(sum(hierbas * (hierba_ht - mean(hierba_ht)) ^ 2, na.rm = T) /
                                                     ((n() - 1) * sum(hierbas, na.rm = T)) /
                                                     n()), NA),
                        pasto_ht_sd = ifelse(any(pastos > 0, na.rm = T),
                                             sqrt(sum(pastos * (pasto_ht - mean(pasto_ht)) ^ 2, na.rm = T) /
                                                    ((n() - 1) * sum(pastos, na.rm = T)) /
                                                    n()), NA)),
            by = c("Site", "Season", "anillo"))

dat.indveg <- dat.indveg %>%
  left_join(dat.locations %>%
              group_by(Site, Season, anillo) %>%
              summarise(hierbas_n = sum(!is.na(hierbas)),
                        hierba_ht_n = sum(hierbas > 0, na.rm = T),
                        pastos_n = sum(!is.na(pastos)),
                        pasto_ht_n = sum(pastos > 0, na.rm = T),
                        salsola_n = sum(!is.na(salsola)),
                        otra_n = sum(!is.na(otra)),
                        otra1_n = length(unique(otra1)),
                        desnudo_n = sum(!is.na(desnudo))),
            by = c("Site", "Season", "anillo")) %>%
  ungroup %>%
  mutate(hierbas_sd = ifelse(hierbas > 0, hierbas_sd / hierbas, NA),
         hierba_ht_sd = ifelse(hierba_ht > 0, hierba_ht_sd / hierba_ht, NA),
         pastos_sd = ifelse(pastos > 0, pastos_sd / pastos, NA),
         pasto_ht_sd = ifelse(pasto_ht > 0, pasto_ht_sd / pasto_ht, NA),
         salsola_sd = ifelse(salsola, salsola_sd / salsola, NA),
         otra_sd = ifelse(otra > 0, otra_sd / otra, NA),
         desnudo_sd = ifelse(desnudo > 0, desnudo_sd / desnudo, NA)) %>%
  rename(hierbas_cv = hierbas_sd,
         hierba_ht_cv = hierba_ht_sd,
         pastos_cv = pastos_sd,
         pasto_ht_cv = pasto_ht_sd,
         salsola_cv = salsola_sd,
         otra_cv = otra_sd,
         desnudo_cv = desnudo_sd)

# # 4. Calculate correlations between bird- and grid-based versions of covariates in years when both were present.
# vars <- c("hierbas", "hierba_ht", "arbusto", "arbusto_cv", "arbusto_ht", "pastos",
#           "pastos_cv", "pasto_ht", "pasto_ht_cv", "salsola", "salsola_ht", "otra", "desnudo")
# cols <- c("cor", "cor10")
# out_cor <- matrix(NA, nrow = length(vars), ncol = length(cols),
#                   dimnames = list(vars, cols))
# 
# for(v in vars) {
#   out_cor[v, "cor"] <- cor(dat.indveg[, v], dat.indveg[, str_c(v, "_grid")], use = "complete")
#   ifelse(str_detect(v, "_cv"),
#          ind10 <- which(dat.indveg[, str_c(str_remove(v, "_cv"), "_grid_n")] >= 10 & dat.indveg[, str_c(str_remove(v, "_cv"), "_n")] >= 10),
#          ind10 <- which(dat.indveg[, str_c(v, "_grid_n")] >= 10 & dat.indveg[, str_c(v, "_n")] >= 10))
#   out_cor[v, "cor10"] <- cor(dat.indveg[ind10, v], dat.indveg[ind10, str_c(v, "_grid")], use = "complete")
# }
# write.csv(out_cor, "Correlations_BirdXGrid_veg.csv")
# 
# # 5. Come up with correction factor for imputing bird-level values where only grid-level values are available.
# mod <- lm(pastos ~ pastos_grid, data = dat.indveg)
# n <- sum(!is.na(dat.indveg$pastos) & !is.na(dat.indveg$pastos_grid))
# dat.indveg <- dat.indveg %>%
#   mutate(pastos_pred = predict(mod, dat.indveg, se.fit = T)$fit,
#          pastos_predsd = predict(mod, dat.indveg, se.fit = T)$se.fit * sqrt(n))
# 
# mod <- lm(desnudo ~ desnudo_grid, data = dat.indveg)
# n <- sum(!is.na(dat.indveg$desnudo) & !is.na(dat.indveg$desnudo_grid))
# dat.indveg <- dat.indveg %>%
#   mutate(desnudo_pred = predict(mod, dat.indveg, se.fit = T)$fit,
#          desnudo_predsd = predict(mod, dat.indveg, se.fit = T)$se.fit * sqrt(n))

# 6. Prune unneeded columns and save
dat.indveg <- dat.indveg %>%
  select(Site:hierbas, hierba_ht, pastos, pasto_ht, salsola:desnudo, hierbas_cv,
         hierba_ht_cv, pastos_cv, pasto_ht_cv, salsola_cv:desnudo_cv,
         hierbas_n:desnudo_n)#, pastos_pred:desnudo_predsd)
trim <- (dat.indveg %>%
  select(hierbas:otra, desnudo:desnudo_cv) %>% # pastos_pred:desnudo_predsd
  data.matrix %>%
  apply(1, function(x) sum(!is.na(x)))) > 0
trim.ind <- which(trim)
dat.indveg <- dat.indveg %>% slice(trim.ind)

write.csv(dat.indveg, "Veg_individual.csv", row.names = F)
