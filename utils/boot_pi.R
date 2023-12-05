# from: https://www.r-bloggers.com/2015/12/prediction-intervals-for-poisson-regression/
boot_pi <- function(model, newdata, boot_n = 1000, p = 0.95) {
  train_data <- model$data
  lower_q <- (1 - p) / 2
  upper_q <- 1 - lower_q

  seeds <- round(runif(boot_n, 1, 1000), 0)

  boot_y <- lapply(1:boot_n, \(i){
    set.seed(seeds[i])
    booted <- train_data[sample(seq(nrow(train_data)), size = nrow(train_data), replace = TRUE), ]
    refitted <- update(model, formula = model$formula, family = model$family, data = booted)
    bpred <- predict(refitted, type = "response", newdata = newdata)
    rpois(length(bpred), lambda = bpred)
  }) %>% do.call(what = rbind)

  set.seed(123)

  boot_ci <- t(apply(boot_y, 2, quantile, c(lower_q, upper_q)))

  return(
    tibble(
      index = newdata[[1]],
      pred = predict(model, newdata = newdata, type = "response"),
      lower = boot_ci[, 1],
      upper = boot_ci[, 2]
    )
  )
}
