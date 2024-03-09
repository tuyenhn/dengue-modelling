.pkgs <- c("tidyverse", "magrittr", "sf", "stars", "tsibble")
xfun::pkg_attach(.pkgs, message = FALSE)

## incidence data
# incidence_raw <- read_csv("../incidence_ts_truncated.csv", show_col_types = FALSE) %>%
#   mutate(date_admitted = yearweek(date_admitted)) %>%
#   as_tsibble(index = date_admitted) %>%
#   fill_gaps(n = 0)
#
# incidence_weekly_df <- incidence_raw %>%
#   index_by(agg = ~ yearweek(.)) %>%
#   summarise(n = sum(n)) %>%
#   rename(date_admitted = agg)
#
# incidence_weekly_df %>% write_rds("data_weekly_rds-s/incidence_weekly_df.rds")
incidence_weekly_df <- read_rds("data_weekly_rds-s/incidence_weekly_df.rds")

# source("data_weekly_weather.R")

## join weather and incidence data
# incidence_weekly_weather_df <- incidence_weekly_df %>%
#   append_row(n = -51, keep_all = TRUE) %>%
#   left_join(hcmc_weather_df, by = c("date_admitted" = "date"))
#
# incidence_weekly_weather_df %>% write_rds("data_weekly_rds-s/incidence_weekly_weather_df.rds")
incidence_weekly_weather_df <- read_rds("data_weekly_rds-s/incidence_weekly_weather_df.rds")

print("all data loaded")
