fsel_covar_glm <- recur_miso_glm(
  train_weekly_weather_df, test_weekly_weather_df, seq(1, 49, 3),
  glm_formula = as.formula(paste0("n ", stepwise_forward$final)),
  # cl = cl
)