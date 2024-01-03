dat <- incidence_weekly_weather_df %>%
  filter_index("2004 W01" ~ "2019 W52") %>%
  select(n, starts_with("t2m")) %>%
  as_tibble() %>%
  mutate(date = unclass(date_admitted))

# vcm_fit <- gam(n ~ s(date, by = t2m), family = poisson(), data = dat)
tvreg_fit <- tvLM(n ~ t2m, data = dat)
lm_fit <- lm(n ~ t2m, data = dat)

summary(tvreg_fit)
coef(tvreg_fit)

{
  plot(dat$date, tvreg_fit$coefficients[, 2], type = "l")
  abline(h = lm_fit$coefficients[2])
}

acf(tvreg_fit$coefficients[, 2])
pacf(tvreg_fit$coefficients[, 2])

dat <- incidence_weekly_weather_df %>%
  calculate_lags("t2m", 1:5) %>%
  filter_index("2004 W01" ~ "2019 W52") %>%
  select(n, starts_with("t2m")) %>%
  as_tibble() %>%
  mutate(date = unclass(date_admitted))

tvreg_fit <- tvLM(
  formula = as.formula(paste0("n ~ t2m + ", paste("t2m_lag", 1:5, sep = "", collapse = " + "))),
  data = dat
)
lm_fit <- lm(
  formula = as.formula(paste0("n ~ t2m + ", paste("t2m_lag", 1:5, sep = "", collapse = " + "))),
  data = dat
)

tvreg_coefs_df <- tvreg_fit$coefficients %>%
  as_tibble() %>%
  rename(intercept = 1) %>%
  mutate(mean_t2m_coef = rowMeans(select(., -intercept))) %>%
  rowid_to_column()

tvreg_coefs_df %>%
  select(-intercept) %>%
  pivot_longer(-c(rowid, mean_t2m_coef)) %>%
  ggplot() +
  geom_line(aes(x = rowid, y = value, color = name), alpha = 0.3) +
  geom_line(aes(x = rowid, y = mean_t2m_coef), color = "black")

{
  plot(tvreg_coefs_df$rowid, tvreg_coefs_df$mean_t2m_coef, type = "l")
  abline(h = mean(lm_fit$coefficients[-1]))
}

hist(tvreg_coefs_df$mean_t2m_coef)
acf(tvreg_coefs_df$mean_t2m_coef)
pacf(tvreg_coefs_df$mean_t2m_coef)

shapiro.test(tvreg_coefs_df$mean_t2m_coef)
