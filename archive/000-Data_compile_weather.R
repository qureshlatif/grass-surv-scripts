library(tidyverse)
library(lubridate)

setwd("C:/Users/Quresh.Latif/files/projects/grassWintSurv")

# Simulated weather data #
dat.sim <- read.csv("weather/dailyweather_meteoblue_all.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = date %>% str_sub(1, 10) %>% ymd()) %>%
  select(site, date, temp.min, temp.mean)

# Weather station data #
dat.field <- read.csv("weather/Janos.dailyweather.1219.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = dmy(date),
         site = "janos") %>%
  select(site, date, temp.min, temp.mean, precip, wind.mean)
dat.add <- read.csv("weather/Janos_dailyweather_unifrut.csv", header = T, stringsAsFactors = F) %>%
    mutate(date = dmy(date),
           site = "janos") %>%
    filter(!date %in% dat.field$date) %>%
    select(site, date, temp.min, temp.mean, precip, wind.mean)
dat.field <- dat.field %>% bind_rows(
  dat.add
)
dat.add <- read.csv("weather/Marfa_dailyweather_airport.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = mdy(date),
         site = "marfa") %>%
  select(site, date, temp.min, temp.mean, precip, wind.mean) %>%
  mutate(temp.min = ifelse(temp.min == -9999, NA, temp.min),
         temp.mean = ifelse(temp.mean == -9999, NA, temp.mean),
         precip = ifelse(precip == -9999, NA, precip),
         wind.mean = ifelse(wind.mean == -9999, NA, wind.mean)) %>%
  bind_rows(
    read.csv("weather/Marfa_dailyweather_mimms3007.csv", header = T, stringsAsFactors = F) %>%
      mutate(date = mdy(date),
             site = "marfa") %>%
      select(site, date, temp.min, temp.mean, precip) %>%
      mutate(wind.mean = NA)
    ) %>%
  dplyr::group_by(site, date) %>%
  summarise(temp.min = mean(temp.min, na.rm = T),
            temp.mean = mean(temp.mean, na.rm = T),
            precip = mean(precip, na.rm = T),
            wind.mean = mean(wind.mean, na.rm = T)) %>%
  mutate(temp.min = ifelse(temp.min %in% c(Inf, -Inf), NA, temp.min),
         temp.mean = ifelse(temp.mean %in% c(Inf, -Inf), NA, temp.mean),
         precip = ifelse(precip %in% c(Inf, -Inf), NA, precip),
         wind.mean = ifelse(wind.mean %in% c(Inf, -Inf), NA, wind.mean))# %>%
  #mutate(wind.mean = ifelse(is.na(wind.mean) & # Not sure about this yet. Need to check with Erin.
  #                            !is.na(temp.min) &
  #                            !is.na(temp.mean) &
  #                            !is.na(precip), 0, wind.mean))
dat.field <- dat.field %>%
  bind_rows(dat.add)
dat.add <- read.csv("weather/vaco_dailyweather.csv", header = T, stringsAsFactors = F) %>%
  mutate(date = dmy(date),
         site = "vaco") %>%
  select(site, date, temp.min, temp.mean, precip, wind.mean) %>%
  full_join(
    read.csv("weather/vaco_precip.csv", header = T, stringsAsFactors = F) %>%
      mutate(date = dmy(date)) %>%
      rename(precip_suppl = precip), by = "date"
  )
dat.field <- dat.field %>%
  bind_rows(dat.add)
rm(dat.add)

## Compare simulated vs field-measured weather data ##
library(cowplot)
theme_set(theme_cowplot())

dat.compare <- dat.sim %>%
  left_join(
    dat.field %>%
      rename(temp.min.f = temp.min,
             temp.mean.f = temp.mean,
             precip.f = precip,
             wind.mean.f = wind.mean),
    by = c("site", "date")
  ) %>%
  filter(!(is.na(temp.min.f) &
           is.na(temp.mean.f) &
           is.na(precip.f) &
           is.na(wind.mean.f)))

rows <- c("temp.min", "temp.mean", "precip", "wind.mean")
cols <- c("Intercept", "Coefficient", "n", "R-squared")
tabCoeff <- matrix("", nrow = length(rows), ncol = length(cols),
                   dimnames = list(rows, cols))

# Minimum temperature #
mod <- lm(temp.min.f ~ temp.min, data = dat.compare)
tabCoeff["temp.min", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                    round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["temp.min", "Coefficient"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                    round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["temp.min", "n"] <- sum(!is.na(dat.compare$temp.min.f))
tabCoeff["temp.min", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$temp.min, dat.compare$temp.min.f, na.rm = T)
maxx <- maxy <- max(dat.compare$temp.min, dat.compare$temp.min.f, na.rm = T)
p.tempmin <- ggplot(aes(x = temp.min.f, y = temp.min), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Minimum temperature")
mod.tempmin <- mod

# Mean temperature #
mod <- lm(temp.mean.f ~ temp.mean, data = dat.compare)
tabCoeff["temp.mean", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                           round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["temp.mean", "Coefficient"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                             round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["temp.mean", "n"] <- sum(!is.na(dat.compare$temp.mean.f))
tabCoeff["temp.mean", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$temp.mean, dat.compare$temp.mean.f, na.rm = T)
maxx <- maxy <- max(dat.compare$temp.mean, dat.compare$temp.mean.f, na.rm = T)
p.tempmean <- ggplot(aes(x = temp.mean.f, y = temp.mean), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Mean temperature")
mod.tempmean <- mod

# Precipitation #
mod <- lm(precip.f ~ precip, data = dat.compare)
tabCoeff["precip", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                            round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["precip", "Coefficient"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                              round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["precip", "n"] <- sum(!is.na(dat.compare$precip.f))
tabCoeff["precip", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$precip, dat.compare$precip.f, na.rm = T)
maxx <- maxy <- max(dat.compare$precip, dat.compare$precip.f, na.rm = T)
p.precip <- ggplot(aes(x = precip.f, y = precip), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Precipitation")
mod.precip <- mod

# Mean wind speed #
mod <- lm(wind.mean.f ~ wind.mean, data = dat.compare)
tabCoeff["wind.mean", "Intercept"] <- str_c(round(coef(summary(mod))[1, 1], digits = 3), "(",
                                            round(coef(summary(mod))[1, 2], digits = 3), ")")
tabCoeff["wind.mean", "Coefficient"] <- str_c(round(coef(summary(mod))[2, 1], digits = 3), "(",
                                              round(coef(summary(mod))[2, 2], digits = 3), ")")
tabCoeff["wind.mean", "n"] <- sum(!is.na(dat.compare$wind.mean.f))
tabCoeff["wind.mean", "R-squared"] <- round(summary(mod)$r.squared, digits = 3)
minx <- miny <- min(dat.compare$wind.mean, dat.compare$wind.mean.f, na.rm = T)
maxx <- maxy <- max(dat.compare$wind.mean, dat.compare$wind.mean.f, na.rm = T)
p.windmean <- ggplot(aes(x = wind.mean.f, y = wind.mean), data = dat.compare) +
  geom_point(alpha = 0.3) +
  xlab(NULL) + ylab(NULL) +
  xlim(c(minx, maxx)) + ylim(c(miny, maxy)) +
  ggtitle("Mean wind speed")
mod.windmean <- mod

p <- ggdraw() +
  draw_plot(p.tempmin, x = 0, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(p.tempmean, x = 0.5, y = 0.5, width = 0.5, height = 0.5) +
  draw_plot(p.precip, x = 0, y = 0, width = 0.5, height = 0.5) +
  draw_plot(p.windmean, x = 0.5, y = 0, width = 0.5, height = 0.5)
p <- ggdraw() + 
  draw_plot(p, x = 0.05, y = 0.05, width = 0.95, height = 0.95) +
  draw_plot_label(c("Simulated", "Field measured"),
                  size = c(20, 20),
                  x = c(0, 0.4), y = c(0.4, 0.05),
                  angle = c(90, 0))

save_plot("Compare_weather_sim_vs_field.tiff", p, ncol = 2.5, nrow = 2.5, dpi = 200)
write.csv(tabCoeff, "Compare_weather_sim_vs_field.csv", row.names = T)
