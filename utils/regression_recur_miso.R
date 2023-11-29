recur_miso <- function(
    train_set, test_set,
    start_indices,
    model = list(), xreg_names = NULL,
    horizon = 4,
    link = c("identity", "log"), distr = c("poisson", "nbinom"),
    cl = NULL) {
  if (!missing(cl)) {
    clusterEvalQ(cl, suppressPackageStartupMessages(library(magrittr)))
    clusterEvalQ(cl, suppressPackageStartupMessages(library(dplyr)))
    clusterEvalQ(cl, library(tscount))
    clusterExport(cl, c("train_set", "test_set"), environment())
  }
  distr <- match.arg(distr)
  link <- match.arg(link)

  pbapply::pblapply(
    start_indices,
    \(start_idx) {
      train_df <- train_set %>%
        bind_rows(test_set %>% slice_head(n = start_idx)) %>%
        tail(nrow(train_set))

      xreg <- NULL
      newxreg <- NULL
      if (!is.null(xreg_names)) {
        xreg <- train_df %>%
          as_tibble() %>%
          select(all_of(xreg_names)) %>%
          as.matrix()
        newxreg <- test_set %>%
          tail(52 - start_idx) %>%
          head(horizon) %>%
          as_tibble() %>%
          select(all_of(xreg_names)) %>%
          as.matrix()
      }

      fit <- tsglm(train_df$n, model = model, link = link, distr = distr, xreg = xreg)
      # `newobs = NULL` to force to recursively use 1-step-ahead predictions
      pred <- predict(fit, n.ahead = horizon, newobs = NULL, newxreg = newxreg)

      forecast_df <- tibble(
        startweek = start_idx,
        actualweek = start_idx + (1:horizon - 1),
        preds = pred$pred,
        lowers = (pred$interval[, "lower"]),
        uppers = (pred$interval[, "upper"]),
      )

      # browser()

      if (pred$method == "bootstrap") {
        dists <- apply(pred$futureobs, 1, \(obs) dist_sample(list(obs)))
        forecast_df %<>%
          mutate(dist = dists, dist = dist[[1]])
      }

      forecast_df
    },
    cl = cl
  ) %>% list_c()
}
