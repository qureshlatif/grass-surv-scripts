library(tidyverse)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv/weather/")

# Simulated weather data #
dat.sim <- read.csv("dailyweather_meteoblue_all.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = date %>% str_sub(1, 10) %>% ymd()) %>%
  select(site, date, temp.min, temp.mean)

# Weather station data #
dat.field <- read.csv("Janos.dailyweather.1219.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = dmy(date),
         site = "janos") %>%
  select(site, date, temp.min, temp.mean)
dat.add <- read.csv("Janos_dailyweather_unifrut.csv", header = T, stringsAsFactors = F) %>%
    mutate(date = dmy(date),
           site = "janos") %>%
    filter(!date %in% dat.field$date) %>%
    select(site, date, temp.min, temp.mean)
dat.field <- dat.field %>% bind_rows(
  dat.add
)
dat.add <- read.csv("Marfa_dailyweather_airport.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = mdy(date),
         site = "marfa") %>%
  select(site, date, temp.min, temp.mean) %>%
  mutate(temp.min = ifelse(temp.min == -9999, NA, temp.min),
         temp.mean = ifelse(temp.mean == -9999, NA, temp.mean)) %>%
  bind_rows(
    read.csv("Marfa_dailyweather_mimms3007.csv", header = T, stringsAsFactors = F) %>%
      mutate(date = mdy(date),
             site = "marfa") %>%
      select(site, date, temp.min, temp.mean)
    ) %>%
  dplyr::group_by(site, date) %>%
  summarise(temp.min = mean(temp.min, na.rm = T),
            temp.mean = mean(temp.mean, na.rm = T)) %>%
  mutate(temp.min = ifelse(temp.min %in% c(Inf, -Inf), NA, temp.min),
         temp.mean = ifelse(temp.mean %in% c(Inf, -Inf), NA, temp.mean)) # %>%
  #mutate(wind.mean = ifelse(is.na(wind.mean) & # Not sure about this yet. Need to check with Erin.
  #                            !is.na(temp.min) &
  #                            !is.na(temp.mean) &
  #                            !is.na(precip), 0, wind.mean))
dat.field <- dat.field %>%
  bind_rows(dat.add)
dat.add <- read.csv("vaco_dailyweather.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = dmy(date),
         site = "vaco") %>%
  select(site, date, temp.min, temp.mean)# %>%
#  full_join(
#    read.csv("weather/vaco_temp.csv", header = T, stringsAsFactors = F) %>%
#      mutate(date = str_sub(date, 1, -10) %>% ymd()) %>%
#      rename(temp_mean_suppl = temp.mean,
#             temp_min_suppl = tmep.min) %>%
#      select(date, temp_mean_suppl, temp_min_suppl), by = "date"
#  )
dat.field <- dat.field %>%
  bind_rows(dat.add)
dat.add <- read.csv("cuza_all_revolucion.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = mdy(date),
         site = "cuza") %>%
  select(site, date, temp.min, temp.mean)
dat.field <- dat.field %>%
  bind_rows(dat.add) %>%
  distinct()
rm(dat.add)

## Compare simulated vs field-measured weather data ##
library(cowplot)
theme_set(theme_cowplot())

dat.compare <- dat.sim %>%
  left_join(
    dat.field %>%
      rename(temp.min.f = temp.min,
             temp.mean.f = temp.mean),
    by = c("site", "date")
  ) %>%
  filter(!(is.na(temp.min.f) &
           is.na(temp.mean.f)))
rows <- c("temp.min", "temp.mean")
cols <- c("Intercept", "Beta.temp.min", "Beta.temp.mean", "n", "R-squared")
tabCoeff <- matrix("", nrow = length(rows), ncol = length(cols),
                   dimnames = list(rows, cols))
# Minimum temperature #
mod <- lm(temp.min.f ~ temp.min + temp.mean, data = dat.compare)
tabCoeff["temp.min", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                    round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["temp.min", "Beta.temp.min"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                    round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["temp.min", "Beta.temp.mean"] <- str_c(round(coef(summary(mod))[3, 1], digits = 3), "(",
                                               round(coef(summary(mod))[3, 2], digits = 3), ")")
tabCoeff["temp.min", "n"] <- sum(!is.na(dat.compare$temp.min.f))
tabCoeff["temp.min", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$temp.min, dat.compare$temp.min.f, na.rm = T)
maxx <- maxy <- max(dat.compare$temp.min, dat.compare$temp.min.f, na.rm = T)
p.tempmin <- ggplot(aes(x = temp.min.f, y = temp.min), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Minimum temperature")

# Mean temperature #
mod <- lm(temp.mean.f ~ temp.min + temp.mean, data = dat.compare)
tabCoeff["temp.mean", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                           round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["temp.mean", "Beta.temp.min"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                               round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["temp.mean", "Beta.temp.mean"] <- str_c(round(coef(summary(mod))[3, 1], digits = 3), "(",
                                                round(coef(summary(mod))[3, 2], digits = 3), ")")
tabCoeff["temp.mean", "n"] <- sum(!is.na(dat.compare$temp.mean.f))
tabCoeff["temp.mean", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$temp.mean, dat.compare$temp.mean.f, na.rm = T)
maxx <- maxy <- max(dat.compare$temp.mean, dat.compare$temp.mean.f, na.rm = T)
p.tempmean <- ggplot(aes(x = temp.mean.f, y = temp.mean), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Mean temperature")

p <- ggdraw() +
  draw_plot(p.tempmin, x = 0, y = 0, width = 0.5, height = 1) +
  draw_plot(p.tempmean, x = 0.5, y = 0, width = 0.5, height = 1)
p <- ggdraw() + 
  draw_plot(p, x = 0.05, y = 0.05, width = 0.95, height = 0.95) +
  draw_plot_label(c("Simulated", "Field measured"),
                  size = c(18, 18),
                  x = c(0.02, 0.38), y = c(0.35, 0.07),
                  angle = c(90, 0))

#save_plot("Compare_temp_sim_vs_field.tiff", p, ncol = 2.5, nrow = 1.3, dpi = 200)
#write.csv(tabCoeff, "Compare_temp_sim_vs_field.csv", row.names = T)

## Impute missing field temp data based on simulated data ##
dat.all <- dat.field %>%
  full_join(dat.sim %>%
              rename(temp.min.s = temp.min,
                     temp.mean.s = temp.mean),
            by = c("site", "date"))

mod <- lm(temp.min ~ temp.mean.s + temp.min.s, data = dat.all)
ind.missing <- which(is.na(dat.all$temp.min))
dat.all$temp.min[ind.missing] <- predict(mod, dat.all %>% slice(ind.missing))

mod <- lm(temp.mean ~ temp.mean.s + temp.min.s, data = dat.all)
ind.missing <- which(is.na(dat.all$temp.mean))
dat.all$temp.mean[ind.missing] <- predict(mod, dat.all %>% slice(ind.missing))

rm(mod)

dat.all <- dat.all %>%
  select(site, date, temp.min, temp.mean) %>%
  arrange(site, date)

## Compute 1-7 day lag averages ##
dat.all <- dat.all %>%
  mutate(temp.min.lag1 = NA,
         temp.min.lag2 = NA,
         temp.min.lag3 = NA,
         temp.min.lag4 = NA,
         temp.min.lag5 = NA,
         temp.min.lag6 = NA,
         temp.min.lag7 = NA,
         temp.min.prec7 = NA,
         temp.mean.lag1 = NA,
         temp.mean.lag2 = NA,
         temp.mean.lag3 = NA,
         temp.mean.lag4 = NA,
         temp.mean.lag5 = NA,
         temp.mean.lag6 = NA,
         temp.mean.lag7 = NA,
         temp.mean.prec7 = NA)
for(i in 1:nrow(dat.all)) {
  for(j in 1:7) {
    if(i > j) {
      lag <- i - j
      if(length(unique(dat.all$site[lag:i])) == 1) {
        dat.all[i, str_c("temp.min.lag",j)] <- mean(dat.all$temp.min[lag:i])
        dat.all[i, str_c("temp.mean.lag",j)] <- mean(dat.all$temp.mean[lag:i])
      }}}
  if((i-7)>0) if(length(unique(dat.all$site[(i-7):i])) == 1) {
    dat.all[i, "temp.min.prec7"] <- mean(dat.all$temp.min[(i-7):(i-1)])
    dat.all[i, "temp.mean.prec7"] <- mean(dat.all$temp.mean[(i-7):(i-1)])
  }}
dat.all <- dat.all %>%
  select(site, date, temp.min, temp.min.lag1:temp.min.prec7,
         temp.mean, temp.mean.lag1:temp.mean.prec7) %>%
  mutate(min.mean.diff = temp.mean - temp.min)

# Convert all day of years to day of season #
library(lubridate)
minDOS <- 317 # Sets first day of season - currently corresponds with 11/12/2012
dat.all$DOY <- yday(dat.all$date)
dat.all$DOS <- dat.all$DOY
ind.beforeD31 <- which(dat.all$DOY > 200)
ind.afterD31 <- which(dat.all$DOY < 200)
dat.all$DOS[ind.beforeD31] <- dat.all$DOS[ind.beforeD31] - (minDOS - 1)
D31 <- (dat.all$date %>% str_sub(1, 4) %>% str_c("12-31") %>% ymd %>% yday)  - (minDOS - 1)
dat.all$DOS[ind.afterD31] <- dat.all$DOS[ind.afterD31] + D31[ind.afterD31]

dat.all <- dat.all %>% filter(DOS > 0 & DOS < 125) # Remove all days outside winter field season.

## Correlations among temp vars ##
library(corrplot)
dat.all %>% select(DOS, temp.min:min.mean.diff) %>%
  cor(use = "complete") %>%
  write.csv("Temperature_variable_correlations.csv", row.names = T)
pdf("Temp_var_correlations.pdf")
dat.all %>%
  select(DOS, temp.min:min.mean.diff) %>% cor(use = "complete") %>%
  corrplot(method = "ellipse", addCoefasPercent = T, diag = F, tl.cex = 0.5)
dev.off()
plot(dat.all$DOS, dat.all$temp.min)
plot(dat.all$DOS, dat.all$temp.mean)

## After reviewing correlations, decided to keep 3 covariates:
# 1. min temp on day of observation.
# 2. mean temp on over preceding 7 days.
# 3. difference between min and mean on day of observation.

dat.all <- dat.all %>%
  select(site, date, DOY, DOS, temp.min, temp.mean.prec7, min.mean.diff) %>%
  rename(temp.prec7 = temp.mean.prec7,
         temp.diff = min.mean.diff)

write.csv(dat.all, "Data_temperature_all.csv", row.names = F)
