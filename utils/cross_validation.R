cv <- crossv_kfold(train_weekly_weather_df)

models <- map(cv$train, ~ glm(n ~ ., data = .))
mses <- map2_dbl(models, cv$test, mse)
