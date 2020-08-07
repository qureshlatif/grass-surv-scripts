library(tidyverse)
library(randomForest)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")
#load("Data_compiled_knownFate.RData")
load("Data_compiled.RData")

vars <- names(data.BAIS$Covs)
vars <- vars[c(which(vars == "hierbas"):which(vars == "otra"),
               which(vars == "desnudo"):which(vars == "desnudo_cv"),
               which(vars == "Mesquite_5m"):which(vars == "Mean_Shrub_Height_500m_CV"))]
vars.drone <- vars[which(vars == "Mesquite_5m"):which(vars == "Mean_Shrub_Height_500m_CV")]
vars.field <- vars[-which(vars %in% vars.drone)]
vars.ht <- vars[which(str_detect(vars, "_ht") | str_detect(vars, "Height"))]
vars.ht.covs <- character(length=length(vars.ht))
for(v in 1:length(vars.ht)) {
  if(!str_detect(vars.ht[v], "Height")) {
    vars.ht.covs[v] <- vars[which(str_detect(vars, str_sub(vars.ht[v], 1, 5)) & !str_detect(vars, "ht") & !str_detect(vars, "cv"))]
  }
  if(str_detect(vars.ht[v], "Height")) {
    scale <- str_c("_",str_split(vars.ht[v], "_")[[1]][4])
    vars.ht.covs[v] <- vars[which(str_detect(vars, "Shrub_All") & str_detect(vars, scale) & !str_detect(vars, "CV"))]
  }
}
var.cov.cvs <- vars[which((str_detect(vars, "cv") | str_detect(vars, "CV")) & !str_detect(vars, "_ht") & !str_detect(vars, "Height"))]
var.covs <- character(length=length(var.cov.cvs))
for(v in 1:length(var.cov.cvs)) {
  if(!str_detect(var.cov.cvs[v], "_CV")) {
    var.covs[v] <- vars[which(str_detect(vars, str_sub(var.cov.cvs[v], 1, 4)) & !str_detect(vars, "ht") & !str_detect(vars, "cv"))]
  }
  if(str_detect(var.cov.cvs[v], "_CV")) {
    scale <- str_split(var.cov.cvs[v], "_")[[1]]
    scale <- scale[(length(scale) - 1)]
    scale <- str_c("_", scale)
    var.covs[v] <- vars[which(str_detect(vars, str_sub(var.cov.cvs[v], 1, 5)) &
                                str_detect(vars, scale) &
                                !str_detect(vars, "CV") &
                                !str_detect(vars, "Height"))]
  }
}

#vars.field.shrub <- vars.field[c(3, 4, 13, 14)]
#vars.field.ground <- vars.field[-which(vars.field %in% vars.field.shrub)]


# data.BAIS$Covs %>%
#   select(one_of(vars)) %>%
#   summarise_all(function(x) sum(is.na(x))) %>%
#   bind_rows(data.GRSP$Covs %>%
#               select(one_of(vars)) %>%
#               summarise_all(function(x) sum(is.na(x)))) %>%
#   data.matrix %>% t() %>%
#   write.csv("Count_missing_covs_BAIS.csv", row.names = T)

for(spp in species[1:2]) {
  data.spp <- str_c("data.", spp) %>% as.name %>% eval
  
  # Impute heights where covers are zero #
  for(v in 1:length(vars.ht)) {
    ind.missing <- which(is.na(data.spp$Covs[, vars.ht[v]]) & !is.na(data.spp$Covs[, vars.ht.covs[v]]))
    ind.known <- which(!is.na(data.spp$Covs[, vars.ht[v]]))
    if(length(ind.missing) > 0) data.spp$Covs[ind.missing, vars.ht[v]] <- mean(data.spp$Covs[ind.known, vars.ht[v]])
  }

  # Impute CVs where covers are zero #
  for(v in 1:length(var.cov.cvs)) {
    ind.missing <- which(is.na(data.spp$Covs[, var.cov.cvs[v]]) & !is.na(data.spp$Covs[, var.covs[v]]))
    ind.known <- which(!is.na(data.spp$Covs[, var.cov.cvs[v]]))
    if(length(ind.missing) > 0) data.spp$Covs[ind.missing, var.cov.cvs[v]] <- mean(data.spp$Covs[ind.known, var.cov.cvs[v]])
  }
  
  # Impute missing drone variables first by filling with means #
  miss <- which((data.spp$Covs %>%
                      select(one_of(vars.drone)) %>%
                      data.matrix() %>%
                      apply(1, function(x) sum(!is.na(x)))) == 0)
  # which(apply(data.spp$ymat[missall,], 1, function(x) any(x == 2, na.rm = T))) # Index data missing all covariates as needed for inspection.
  miss.fill <- data.spp$Covs %>% select(one_of(vars.drone)) %>% summarize_all(function(x) mean(x, na.rm = T))
  data.spp$Covs[miss, vars.drone] <- miss.fill
  
  # # Impute grass cover and bare ground with grid-based values where available #
  # data.spp$Covs <- data.spp$Covs %>%
  #   mutate(pastos = ifelse(is.na(pastos) & !is.na(pastos_pred),
  #                          pastos_pred, pastos),
  #          desnudo = ifelse(is.na(desnudo) & !is.na(desnudo_pred),
  #                          pastos_pred, desnudo))
  
  # Impute missing field-measured shrub variables based on drone data # (***dropped field shrub metrics, so this is no longer relevant***)
  # for(v in vars.field.shrub) {
  #   ind.missing <- which(is.na(data.spp$Covs[, v]))
  #   ind.known <- which(!is.na(data.spp$Covs[, v]))
  #   rf <- randomForest(as.formula(str_c(v, "~", str_c(vars.drone, collapse = "+"))), data = (data.spp$Covs %>% slice(ind.known)))
  #   data.spp$Covs[ind.missing, v] <- predict(rf, newdata = (data.spp$Covs %>% slice(ind.missing)))
  #   if(any(data.spp$Covs[, v] < 0)) data.spp$Covs[which(data.spp$Covs[, v] < 0), v] <- 0
  # }
  
  # Impute missing values for remaining field-measured variables #
  v.complete <- vars.drone
  for(v in vars.field) {
    ind.missing <- which(is.na(data.spp$Covs[, v]))
    ind.known <- which(!is.na(data.spp$Covs[, v]))
    rf <- randomForest(as.formula(str_c(v, "~", str_c(v.complete, collapse = "+"))), data = (data.spp$Covs %>% slice(ind.known)))
    data.spp$Covs[ind.missing, v] <- predict(rf, newdata = (data.spp$Covs %>% slice(ind.missing)))
    if(any(data.spp$Covs[, v] < 0)) data.spp$Covs[which(data.spp$Covs[, v] < 0), v] <- 0
    v.complete <- c(v.complete, v)
  }
  
  assign(str_c("data.", spp), data.spp)
}

rm(data.spp, miss.fill, rf, ind.known, ind.missing, miss, spp, v, v.complete,
   vars, vars.drone, vars.field, var.cov.cvs, var.covs, vars.ht, vars.ht.covs, scale) # vars.field.ground, vars.field.shrub, 
save.image("Data_compiled_MissingCovsImputed.RData")
#save.image("Data_compiled_KnownFateMissingImputed.RData")
