library(tidyverse)
library(tsibble)
library(magrittr)

library(sf)
library(stars)

# # HCMC shapefile
hcmc_shp <- read_rds("gadm/gadm41_VNM_1_pk.rds") %>%
  terra::unwrap() %>%
  st_as_sf() %>%
  filter(GID_1 == "VNM.25_1")

hcmc_shp_w_buff <- st_buffer(hcmc_shp, units::set_units(10, "km"))

# # weather variables from ERA5
raw_weather <- read_ncdf("../TP_HB_SPI_corrected_L2.nc") %>%
  st_set_crs(4326)
# # SPI data
# #  - Monthly SPI shows how wet/dry was the weather compared to the usual in the whole month
# #  - Daily SPI shows how wet/dry was the weather compared to the usual using for the last 30 days
# spi_daily <- read_ncdf("weather_data/SPI_daily_L2.nc")
# spi_monthly <- read_ncdf("weather_data/SPI_monthly_L2.nc")

# raw_weather %>% write_rds("data_weekly_rds-s/raw_weather.rds")
# spi_daily %>% write_rds("data_weekly_rds-s/spi_daily.rds")
# spi_monthly %>% write_rds("data_weekly_rds-s/spi_monthly.rds")

raw_weather <- read_rds("data_weekly_rds-s/raw_weather.rds")
spi_daily <- read_rds("data_weekly_rds-s/spi_daily.rds")
spi_monthly <- read_rds("data_weekly_rds-s/spi_monthly.rds")

# # fns
# source("covar_fns.R")

# hcmc_temp_df <- gen_mean_covar_df("t2m", subtract, 273.15)
# hcmc_precip_df <- gen_covar_df("tp", sum, multiply_by, 1000, new_covar_name = "precip")
# hcmc_rh_df <- gen_mean_covar_df("rh")
# hcmc_sh_df <- gen_mean_covar_df("sh")
# hcmc_ws_df <- gen_mean_covar_df("ws")
# hcmc_hb_df <- gen_covar_df("hb", sum)
# hcmc_ssdr_df <- gen_covar_df("ssdr", sum)
# hcmc_mx_mn2t24_df <- lapply(
#   c("mx2t24", "mn2t24"),
#   \(covar) gen_mean_covar_df(covar, subtract, 273.15)
# ) %>% reduce(left_join, by = "date")
# hcmc_mx_mnrh24_df <- lapply(c("mxrh24", "mnrh24"), gen_mean_covar_df) %>%
#   reduce(left_join, by = "date")
# hcmc_mx_mnsh24_df <- lapply(c("mxsh24", "mnsh24"), gen_mean_covar_df) %>%
#   reduce(left_join, by = "date")
# hcmc_spi_df <- gen_mean_covar_df("tp", new_covar_name = "spi_daily", dataset = spi_daily)


# hcmc_temp_df %>% write_rds("data_weekly_rds-s/hcmc_temp_df.rds")
# hcmc_precip_df %>% write_rds("data_weekly_rds-s/hcmc_precip_df.rds")
# hcmc_rh_df %>% write_rds("data_weekly_rds-s/hcmc_rh_df.rds")
# hcmc_sh_df %>% write_rds("data_weekly_rds-s/hcmc_sh_df.rds")
# hcmc_ws_df %>% write_rds("data_weekly_rds-s/hcmc_ws_df.rds")
# hcmc_hb_df %>% write_rds("data_weekly_rds-s/hcmc_hb_df.rds")
# hcmc_ssdr_df %>% write_rds("data_weekly_rds-s/hcmc_ssdr_df.rds")
# hcmc_mx_mn2t24_df %>% write_rds("data_weekly_rds-s/hcmc_mx_mn2t24_df.rds")
# hcmc_mx_mnrh24_df %>% write_rds("data_weekly_rds-s/hcmc_mx_mnrh24_df.rds")
# hcmc_mx_mnsh24_df %>% write_rds("data_weekly_rds-s/hcmc_mx_mnsh24_df.rds")
# hcmc_spi_df %>% write_rds("data_weekly_rds-s/hcmc_spi_df.rds")


hcmc_temp_df <- read_rds("data_weekly_rds-s/hcmc_temp_df.rds")
hcmc_precip_df <- read_rds("data_weekly_rds-s/hcmc_precip_df.rds")
hcmc_rh_df <- read_rds("data_weekly_rds-s/hcmc_rh_df.rds")
hcmc_sh_df <- read_rds("data_weekly_rds-s/hcmc_sh_df.rds")
hcmc_ws_df <- read_rds("data_weekly_rds-s/hcmc_ws_df.rds")
hcmc_hb_df <- read_rds("data_weekly_rds-s/hcmc_hb_df.rds")
hcmc_ssdr_df <- read_rds("data_weekly_rds-s/hcmc_ssdr_df.rds")
hcmc_mx_mn2t24_df <- read_rds("data_weekly_rds-s/hcmc_mx_mn2t24_df.rds")
hcmc_mx_mnrh24_df <- read_rds("data_weekly_rds-s/hcmc_mx_mnrh24_df.rds")
hcmc_mx_mnsh24_df <- read_rds("data_weekly_rds-s/hcmc_mx_mnsh24_df.rds")
hcmc_spi_df <- read_rds("data_weekly_rds-s/hcmc_spi_df.rds")

hcmc_weather_df <- reduce(
  ls() %>%
    Filter(f = \(name) startsWith(name, "hcmc") && endsWith(name, "df"), x = .) %>%
    lapply(FUN = get),
  \(acc, nxt) left_join(acc, nxt, by = "date")
)
