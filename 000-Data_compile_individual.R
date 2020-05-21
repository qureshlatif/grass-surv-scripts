## This script compiles sex, age, and mass values for each individual ##
library(tidyverse)
library(MASS)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

dat.banding <- read.csv("wintersurvival_accessDBs/ExportedTables/Anillamiento_forImport.csv", header = T, stringsAsFactors = F) %>%
  tbl_df() %>%
  mutate(peso = ifelse(peso == -999, NA, peso),
         pico_culmen = ifelse(pico_culmen == -999, NA, pico_culmen),
         ala = ifelse(ala == -999, NA, ala),
         cola = ifelse(cola == -999, NA, cola),
         tarso = ifelse(tarso == -999, NA, tarso))

dat.sex <- read.csv("wintersex_1219.csv", header = T, stringsAsFactors = F) %>%
  mutate(sex = sex %>% str_to_upper()) %>%
  mutate(sex = ifelse(sex %in% c("", "U"), NA, sex)) %>%
  distinct(anillo, .keep_all = T)
#sum(!dat.sex$anillo %in% dat.banding$anillo) # zero - yay!
#sum(dat.banding$especie %in% c("BAIS", "GRSP") & !dat.banding$anillo %in% dat.sex$anillo) /
#  sum(dat.banding$especie %in% c("BAIS", "GRSP"))
#sum(is.na(dat.sex$sex)) / nrow(dat.sex)

dat.banding <- dat.banding %>%
  left_join(
    dat.sex %>%
      rename(sex_gen = sex) %>%
      dplyr::select(anillo, sex_gen),
    by = c("anillo")
  ) %>%
  mutate(female = (sex_gen == "H")*1)
dat.banding$female_how <- ""
dat.banding$female_how[which(!is.na(dat.banding$female))] <- "genetics"

### Impute missing sexes with DFA ###
# Use: peso, tarso (where available), pico_culmen, ala, cola

## BAIS ##
dat <- dat.banding %>%
  filter(especie == "BAIS") %>%
  dplyr::select(sex_gen, peso, pico_culmen, ala, cola) %>%
  filter_all(function(x) !is.na(x)) %>%
  mutate(sex_gen = as.factor(sex_gen))

mod <- lda(sex_gen ~ peso + pico_culmen + ala + cola, data = dat)
Predictions <- predict(mod, dat)
table(Predictions$class, dat$sex_gen)
ldahist(Predictions$x[,1], g = dat$sex_gen)
sum(diag(table(Predictions$class, dat$sex_gen)))/sum(table(Predictions$class, dat$sex_gen)) * 100 # Percent classification accuracy

ind.pred <- which(is.na(dat.banding$female) &
                    dat.banding$especie == "BAIS" &
                    !is.na(dat.banding$peso) &
                    !is.na(dat.banding$pico_culmen) &
                    !is.na(dat.banding$ala) &
                    !is.na(dat.banding$cola))
dat.banding$female[ind.pred] <-
  predict(mod, dat.banding %>% slice(ind.pred))$posterior[,"H"] # 378 observations imputed based on morphometrics
dat.banding$female_how[ind.pred] <- "morphometrics"

ind.pred <- which(is.na(dat.banding$female) & dat.banding$especie == "BAIS")
dat.banding$female[ind.pred] <- mean(dat.banding$female[which(dat.banding$especie == "BAIS")], na.rm = T)
dat.banding$female_how[ind.pred] <- "mean"

## GRSP ##
dat <- dat.banding %>%
  filter(especie == "GRSP") %>%
  dplyr::select(sex_gen, peso, pico_culmen, ala, cola) %>%
  filter_all(function(x) !is.na(x)) %>%
  mutate(sex_gen = as.factor(sex_gen))

mod <- lda(sex_gen ~ peso + pico_culmen + ala + cola, data = dat)
Predictions <- predict(mod, dat)
table(Predictions$class, dat$sex_gen)
ldahist(Predictions$x[,1], g = dat$sex_gen)
sum(diag(table(Predictions$class, dat$sex_gen)))/sum(table(Predictions$class, dat$sex_gen)) * 100 # Percent classification accuracy

ind.pred <- which(is.na(dat.banding$female) &
                    dat.banding$especie == "GRSP" &
                    !is.na(dat.banding$peso) &
                    !is.na(dat.banding$pico_culmen) &
                    !is.na(dat.banding$ala) &
                    !is.na(dat.banding$cola))
dat.banding$female[ind.pred] <-
  predict(mod, dat.banding %>% slice(ind.pred))$posterior[,"H"] # 378 observations imputed based on morphometrics
dat.banding$female_how[ind.pred] <- "morphometrics"

ind.pred <- which(is.na(dat.banding$female) & dat.banding$especie == "GRSP")
dat.banding$female[ind.pred] <- mean(dat.banding$female[which(dat.banding$especie == "GRSP")], na.rm = T)
dat.banding$female_how[ind.pred] <- "mean"

### Impute missing ages ###
dat.banding <- dat.banding %>%
  mutate(edad_V = ifelse(edad_V %in% c("U", "-9"), NA, edad_V)) %>%
  mutate(adult = (edad_V == "A")*1)

dat.banding$adult[which(is.na(dat.banding$adult) & dat.banding$especie == "BAIS")] <-
  mean(dat.banding$adult[which(dat.banding$especie == "BAIS")], na.rm = T)
dat.banding$adult[which(is.na(dat.banding$adult) & dat.banding$especie == "GRSP")] <-
  mean(dat.banding$adult[which(dat.banding$especie == "GRSP")], na.rm = T)

write.csv(dat.banding, "Banding_data_plus.csv", row.names = F)

# ## Try by site ##
# dat <- dat.banding %>%
#   filter(especie == "BAIS" & Site == "Janos") %>%
#   dplyr::select(sex_gen, peso, pico_culmen, ala, cola) %>%
#   filter_all(function(x) !is.na(x)) %>%
#   mutate(sex_gen = as.factor(sex_gen))
# 
# mod <- lda(sex_gen ~ peso + pico_culmen + ala + cola, data = dat)
# Predictions <- predict(mod, dat)
# table(Predictions$class, dat$sex_gen)
# ldahist(Predictions$x[,1], g = dat$sex_gen)
# sum(diag(table(Predictions$class, dat$sex_gen)))/sum(table(Predictions$class, dat$sex_gen)) * 100 # Percent classification accuracy
# nrow(dat)
