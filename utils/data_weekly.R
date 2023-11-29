library(tidyverse)
library(tsibble)

# library(sf)
# library(stars)

## incidence data
# incidence_raw <- read_csv("incidence_ts_in.csv", show_col_types = FALSE) %>%
#   mutate(date_admitted = as.Date(date_admitted)) %>%
#   as_tsibble(index = date_admitted) %>%
#   fill_gaps(n = 0)
#
# incidence_weekly_df <- incidence_raw %>%
#   index_by(agg = ~ yearweek(.)) %>%
#   summarise(n = sum(n)) %>%
#   rename(date_admitted = agg)

## sets
# train_weekly_df <- incidence_weekly_df %>%
#   filter_index("2000 W01" ~ "2017 W52")
#
# val_weekly_df <- incidence_weekly_df %>%
#   filter_index("2018 W01" ~ "2018 W52")
#
# test_weekly_df <- incidence_weekly_df %>%
#   filter_index("2019 W01" ~ "2019 W52")

# incidence_weekly_df %>% write_rds("data_weekly_rds-s/incidence_weekly_df.rds")
# train_weekly_df %>% write_rds("data_weekly_rds-s/train_weekly_df.rds")
# val_weekly_df %>% write_rds("data_weekly_rds-s/val_weekly_df.rds")
# test_weekly_df %>% write_rds("data_weekly_rds-s/test_weekly_df.rds")

incidence_weekly_df <- read_rds("data_weekly_rds-s/incidence_weekly_df.rds")
train_weekly_df <- read_rds("data_weekly_rds-s/train_weekly_df.rds")
val_weekly_df <- read_rds("data_weekly_rds-s/val_weekly_df.rds")
test_weekly_df <- read_rds("data_weekly_rds-s/test_weekly_df.rds")

source("data_weekly_weather.R")

# ## join weather and incidence data
# incidence_weekly_weather_df <- incidence_weekly_df %>%
#   append_row(n = -51, keep_all = TRUE) %>%
#   left_join(hcmc_weather_df, by = c("date_admitted" = "date"))
#
# train_weekly_weather_df <- incidence_weekly_weather_df %>%
#   filter_index("2000 W01" ~ "2017 W52")
#
# val_weekly_weather_df <- incidence_weekly_weather_df %>%
#   filter_index("2018 W01" ~ "2018 W52")
#
# test_weekly_weather_df <- incidence_weekly_weather_df %>%
#   filter_index("2019 W01" ~ "2019 W52")

# incidence_weekly_weather_df %>% write_rds("data_weekly_rds-s/incidence_weekly_weather_df.rds")
# train_weekly_weather_df %>% write_rds("data_weekly_rds-s/train_weekly_weather_df.rds")
# val_weekly_weather_df %>% write_rds("data_weekly_rds-s/val_weekly_weather_df.rds")
# test_weekly_weather_df %>% write_rds("data_weekly_rds-s/test_weekly_weather_df.rds")

incidence_weekly_weather_df <- read_rds("data_weekly_rds-s/incidence_weekly_weather_df.rds")
train_weekly_weather_df <- read_rds("data_weekly_rds-s/train_weekly_weather_df.rds")
val_weekly_weather_df <- read_rds("data_weekly_rds-s/val_weekly_weather_df.rds")
test_weekly_weather_df <- read_rds("data_weekly_rds-s/test_weekly_weather_df.rds")
