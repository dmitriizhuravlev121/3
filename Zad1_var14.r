library(tidyverse)
library(rnoaa)
library(dplyr)
station_data = ghcnd_stations()
chelyabinsk = data.frame(id = "CHELYABINSK", latitude = 55.154,  longitude = 61.4291)
chelyabinsk_around = meteo_nearby_stations(lat_lon_df = chelyabinsk, station_data = station_data,
                                   limit = 30, var = c("PRCP", "TAVG"),
                                   year_min = 2007, year_max = 2012)
chelyabinsk_around_id = chelyabinsk_around[["CHELYABINSK"]][["id"]]
all_chelyabinsk_around_data = meteo_tidy_ghcnd(stationid = chelyabinsk_around_id, var=c("TAVG"), date_min = "2007-01-01", date_max = "2012-12-31")
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% mutate(tavg = tavg / 10)
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% filter(tavg > 5)
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% mutate(year = year(date), month = month(date))
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% group_by(id, year, month) %>% summarise(sum_month = mean(tavg, na.rm = TRUE))
var_table <- data.frame(
  m = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12),
  afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00),
  bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00),
  di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
)
q = 1600
l = 2.2
e = 25
k = 300
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% 
  mutate(yj = ((var_table[["afi"]][month] + var_table[["bfi"]][month] * 1.0 * sum_month) * var_table[["di"]][month] * k)/(q * l * (100 - e)))
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% group_by(id, year) %>% summarise(sum_year = mean(yj, na.rm = TRUE))
all_chelyabinsk_around_data <- all_chelyabinsk_around_data %>% group_by(id) %>% summarise(sum_station = mean(sum_year, na.rm = TRUE))
Y <- sum(all_chelyabinsk_around_data$sum_station)
print(paste(Y, "тонн")) 
