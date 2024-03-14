calculate_lags <- function(df, var, lags) {
  map_lag <- lags %>% map(~ partial(lag, n = .x))
  return(df %>% mutate(across(.cols = all_of({{ var }}), .fns = map_lag, .names = "{.col}_lag{lags}")))
}

rep_kfold_cv_glm <- function(formula, dataset, rep = 5, k = 5) {
  seeds <- round(runif(rep, 1, 1000), 0)
  cv <- lapply(1:rep, \(r){
    set.seed(seeds[r])
    crossv_kfold(dataset, k = k) %>% mutate(.rep = r)
  }) %>% list_c()

  fits <- lapply(cv$train, \(data) glm(formula, family = "poisson", data = data))

  perfs <- furrr::future_map2(fits, cv$test, \(x, y){
    preds <- boot_pi(x, newdata = y)
    poi_modes <- floor(preds$preds)
    boot_dists <- apply(preds$bootstrap, 2, \(b) dist_sample(list(b)))
    truths <- as.data.frame(y)$n

    cred_ints <- map2(truths, preds$bootstrap, \(gt, b) {
      ecdfx <- ecdf(b)
      2 * abs(0.5 - ecdfx(gt))
    })

    # mean_prob <- map2(boot_dists, truths, stats::density) %>%
    #   unlist() %>%
    #   mean()
    # crps <- map2(truths, boot_dists, \(y, sample){
    #   crps_sample(y, parameters(sample)$x[[1]])
    # })

    c(
      rmse = yardstick::rmse_vec(truths, poi_modes),
      mae = yardstick::mae_vec(truths, poi_modes),
      mean_cis = cred_ints |> unlist() |> mean()
      # mean_prob = mean_prob
      # crps = mean(unlist(crps))
    )
  }, .options = furrr_options(seed = TRUE, packages = "modelr"), .progress = TRUE) %>%
    as.data.frame() %>%
    t()

  perfs
}

rep_kfold_cv_covar <- function(dataset, covar_name, rep = 5, k = 5) {
  formula <- as.formula(paste0("n ~ n_lag1 + ", covar_name))

  cv_perfs <- rep_kfold_cv_glm(formula = formula, dataset = dataset, rep = rep, k = k)

  res_df <- tibble(model = sprintf("Poisson 5-repetition 5-fold CV w/ %s", covar_name))

  for (metric in colnames(cv_perfs)) {
    res_df %<>% mutate(!!metric := list(cv_perfs[, metric]))
  }

  set.seed(123) # reset seed

  res_df
}
