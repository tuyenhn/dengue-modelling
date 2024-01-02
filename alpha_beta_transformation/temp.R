library(modelr)
library(distributional)

set.seed(123)
source("../utils/data_weekly.R", chdir = T)

# plot(incidence_weekly_df$date_admitted %>% as.Date(), incidence_weekly_df$n, type = "l")

calculate_lags <- function(df, var, lags) {
  map_lag <- lags %>% map(~ partial(lag, n = .x))
  return(df %>% mutate(across(.cols = all_of({{ var }}), .fns = map_lag, .names = "{.col}_lag{lags}")))
}

# covar_names <- incidence_weekly_weather_df %>%
#   select(-c(date_admitted, n)) %>%
#   colnames()
# incidence_new_df <- lapply(covar_names, \(covar) calculate_lags(incidence_weekly_weather_df, covar, 1:20)) %>%
#   bind_cols()

incidence_new_df <- calculate_lags(incidence_weekly_df, "n", 1:20)

############### NO FEATURE ENGINEERING AFTER THIS LINE ###############

incidence_new_df %<>%
  filter_index("2004 W01" ~ "2019 W52") %>%
  as_tibble() %>%
  mutate(week = isoweek(date_admitted), month = month(date_admitted)) %>%
  select(-date_admitted) %>%
  rowid_to_column()

train_frac <- 0.75

train_new_df <- incidence_new_df %>% sample_frac(train_frac)
test_new_df <- anti_join(incidence_new_df, train_new_df, by = "rowid")

formula <- quote(as.formula(paste0("n ~ ", sprintf("n_lag%d", l))))
# formula <- quote(as.formula(paste0("n ~ ", sprintf("n_lag%d", 1:l) %>% paste(collapse = " + "))))

autolag_indi_df <- lapply(1:20, \(l){
  cv <- crossv_kfold(train_new_df)

  fits <- lapply(
    cv$train, \(data) glm(eval(formula), family = "poisson", data = data)
  )

  perfs <- map2(fits, cv$test, \(x, y){
    preds <- predict(x, newdata = y, type = "response")
    c(
      rmse = yardstick::rmse_vec(as.data.frame(y)$n, preds),
      mae = yardstick::mae_vec(as.data.frame(y)$n, preds)
    )
  }) %>%
    as.data.frame() %>%
    t()

  tibble_row(
    lag = l,
    avg_rmse = mean(perfs[, "rmse"]),
    max_rmse = max(perfs[, "rmse"]),
    min_rmse = min(perfs[, "rmse"]),
    avg_mae = mean(perfs[, "mae"]),
    max_mae = max(perfs[, "mae"]),
    min_mae = min(perfs[, "mae"])
  )
}) %>% list_c()
autolag_indi_df

autolag_indi_df %>% ggplot() +
  geom_col(aes(x = lag, y = avg_rmse)) +
  geom_errorbar(aes(x = lag, ymin = min_rmse, ymax = max_rmse), color = "red", alpha = 0.8) +
  ggtitle("Poisson regression by week, month and autolagged weeks") +
  scale_y_continuous("RMSE") +
  scale_x_continuous(breaks = 1:20, minor_breaks = NULL)

fit <- glm(n ~ n_lag1, family = "poisson", data = train_new_df)

dists <- apply(pred$bootstrap, 2, \(b) dist_sample(list(b)))

# score_df <- tibble(
#   rowid = test_new_df$rowid,
#   y = test_new_df$n,
#   dist = dists
# ) %>%
#   mutate(dist = dist[[1]]) %>%
#   group_by(rowid) %>%
#   summarise(
#     rmse = sqrt(mean((y - mean(dist))^2)),
#     crps = scoringRules::crps_sample(y, matrix(parameters(dist)$x[[1]], nrow = n()))
#   )

# mean(score_df$crps)
# mean(score_df$rmse)
