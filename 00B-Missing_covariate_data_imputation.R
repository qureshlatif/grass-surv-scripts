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
vars.field.shrub <- vars.field[c(3, 4, 13, 14)]
vars.field.ground <- vars.field[-which(vars.field %in% vars.field.shrub)]

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
  
  # Impute missing field-measured shrub variables based on drone data #
  for(v in vars.field.shrub) {
    ind.missing <- which(is.na(data.spp$Covs[, v]))
    ind.known <- which(!is.na(data.spp$Covs[, v]))
    rf <- randomForest(as.formula(str_c(v, "~", str_c(vars.drone, collapse = "+"))), data = (data.spp$Covs %>% slice(ind.known)))
    data.spp$Covs[ind.missing, v] <- predict(rf, newdata = (data.spp$Covs %>% slice(ind.missing)))
    if(any(data.spp$Covs[, v] < 0)) data.spp$Covs[which(data.spp$Covs[, v] < 0), v] <- 0
  }
  
  # Impute missing values for remaining field-measured variables #
  v.complete <- c(vars.drone, vars.field.shrub)
  for(v in vars.field.ground) {
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
   vars, vars.drone, vars.field, vars.field.ground, vars.field.shrub)
save.image("Data_compiled_MissingCovsImputed.RData")
#save.image("Data_compiled_KnownFateMissingImputed.RData")
