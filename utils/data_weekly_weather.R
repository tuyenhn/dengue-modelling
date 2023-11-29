library(tidyverse)
library(tsibble)

library(sf)
library(stars)

# # HCMC shapefile
# hcmc_shp <- read_rds("gadm/gadm41_VNM_1_pk.rds") %>%
#   terra::unwrap() %>%
#   st_as_sf() %>%
#   filter(GID_1 == "VNM.25_1")
#
# hcmc_shp_w_buff <- st_buffer(hcmc_shp, units::set_units(10, "km"))
#
# # weather variables from ERA5
# raw_weather <- read_ncdf("L2_V3_T2m_RH_TP_SH_1999_2022.nc") %>%
#   st_set_crs(4326)

# hcmc_temp_df <- raw_weather %>%
#   select(t2m) %>%
#   aggregate("1 week", FUN = mean) %>%
#   aggregate(hcmc_shp_w_buff, FUN = mean) %>%
#   as_tibble() %>%
#   select(time, t2m) %>%
#   rename(date = time) %>%
#   mutate(
#     date = as.Date(date) %>% yearweek(),
#     t2m = t2m - 273.15,
#   ) %>%
#   as_tsibble()
#
# hcmc_precip_df <- raw_weather %>%
#   select(tp) %>%
#   aggregate("1 week", FUN = sum) %>%
#   aggregate(hcmc_shp_w_buff, FUN = sum) %>%
#   as_tibble() %>%
#   select(time, tp) %>%
#   rename(date = time, precip = tp) %>%
#   mutate(
#     date = as.Date(date) %>% yearweek(),
#     precip = precip * 1000,
#   ) %>%
#   as_tsibble()
#
# hcmc_rh_df <- raw_weather %>%
#   select(rh) %>%
#   aggregate("1 week", FUN = mean) %>%
#   aggregate(hcmc_shp_w_buff, FUN = mean) %>%
#   as_tibble() %>%
#   select(time, rh) %>%
#   rename(date = time, rh = rh) %>%
#   mutate(date = as.Date(date) %>% yearweek()) %>%
#   as_tsibble()

hcmc_temp_df <- read_rds("./data_weekly_rds-s/hcmc_temp_df.rds")
hcmc_precip_df <- read_rds("./data_weekly_rds-s/hcmc_precip_df.rds")
hcmc_rh_df <- read_rds("./data_weekly_rds-s/hcmc_rh_df.rds")

# hcmc_weather_df <- hcmc_temp_df %>%
#   left_join(hcmc_precip_df) %>%
#   left_join(hcmc_rh_df)
