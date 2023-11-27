chunks_forecast_perf <- function(frcst_df, model_name, metrics = c("RMSE", "CRPS"), crps_fn = NULL, ...) {
  # browser()

  elips <- enquos(...)
  if ("CRPS" %in% metrics) {
    if (is.null(crps_fn)) {
      stop("`crps_fn` needs to be provided if CRPS is in `metrics`")
    } else {
      CRPS <- quo(mean(crps_fn(n, !!!elips)))
    }
  }

  what <- c(
    RMSE = quo(sqrt(mean((n - .mean)^2))),
    # CRPS = quo(crp_score(dist, n))
    CRPS = CRPS
  )

  res_df <- frcst_df %>%
    group_by(startweek) %>%
    summarise(
      data.frame(t(
        sapply(metrics, \(metric) eval(what[[metric]]))
      ))
    )

  mean_df <- res_df %>%
    summarise(across(all_of(metrics), mean)) %>%
    mutate(model = model_name, .before = everything())


  for (m in metrics) {
    print(sprintf("Test mean %s: %f", m, mean_df[1, m]))
  }

  model_perfs <<- model_perfs %>% bind_rows(mean_df)

  res_df
}

forecast_perf <- function(forecast, actual, model_name) {
  rmse <- sqrt(mean((actual - forecast)^2))

  print(sprintf("Test RMSE: %f", rmse))

  model_perfs <<- model_perfs %>%
    bind_rows(tibble(model = model_name, RMSE = rmse))
}
