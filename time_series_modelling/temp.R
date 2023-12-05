recur_miso_glm(
  train_weekly_weather_df, test_weekly_weather_df,
  seq(1, 49, 3), n ~ t2m
)
