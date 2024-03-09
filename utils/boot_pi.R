# from: https://www.r-bloggers.com/2015/12/prediction-intervals-for-poisson-regression/
boot_pi <- function(model, newdata, boot_n = 1000, p = 0.95, type = "response") {
  # browser()
  model_dat <- model$data
  if (inherits(model_dat, "resample")) {
    train_data <- model_dat$data[model_dat$idx, ]
  } else {
    train_data <- model_dat
  }
  lower_q <- (1 - p) / 2
  upper_q <- 1 - lower_q

  set.seed(123)
  seeds <- round(runif(boot_n, 1, 1000), 0)

  boot_y <- furrr::future_map(1:boot_n, \(i){
    set.seed(seeds[i])
    booted <- train_data[sample(seq(nrow(train_data)), size = nrow(train_data), replace = TRUE), ]
    predict(update(model, formula = model$formula, data = booted), type = type, newdata = newdata)
  }, .options = furrr_options(seed = TRUE), .progress = TRUE) %>%
    bind_rows()

  # browser()

  set.seed(123) # reset seed to default

  boot_ci <- t(apply(X = boot_y, MARGIN = 2, FUN = quantile, probs = c(lower_q, upper_q), na.rm = TRUE))

  # browser()

  return(
    list(
      preds = predict(model, newdata = newdata, type = type),
      lowers = boot_ci[, 1],
      uppers = boot_ci[, 2],
      bootstrap = boot_y
    )
  )
}
