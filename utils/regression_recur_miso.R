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

        browser()

        newxreg <- test_set %>%
          tail(52 - start_idx + 1) %>%
          head(horizon) %>%
          as_tibble() %>%
          select(all_of(xreg_names)) %>%
          as.matrix()
      }

      fit <- tsglm(train_df$n, model = model, link = link, distr = distr, xreg = xreg)

      # `newobs = NULL` to force to recursively use 1-step-ahead predictions
      pred <- predict(
        fit,
        n.ahead = horizon, newobs = NULL, newxreg = newxreg,
        method = "bootstrap", estim = "ignore"
      )

      dists <- apply(pred$futureobs, 1, \(obs) dist_sample(list(obs)))

      tibble(
        startweek = start_idx,
        actualweek = start_idx + (1:horizon - 1),
        preds = pred$pred,
        lowers = (pred$interval[, "lower"]),
        uppers = (pred$interval[, "upper"]),
      ) %>%
        mutate(dist = dists, dist = dist[[1]])
    },
    cl = cl
  ) %>% list_c()
}

source("./boot_pi.R", chdir = TRUE)

recur_miso_glm <- function(
    train_set, test_set,
    start_indices,
    glm_formula,
    horizon = 4,
    glm_family = c("poisson"),
    cl = NULL) {
  glm_family <- match.arg(glm_family)

  # browser()

  pbapply::pblapply(
    start_indices,
    \(start_idx) {
      train_df <- train_set %>%
        bind_rows(test_set %>% slice_head(n = start_idx)) %>%
        tail(nrow(train_set))

      test_df <- test_set %>%
        slice_tail(n = 52 - start_idx + 1) %>%
        head(horizon)

      model <- glm(glm_formula, family = glm_family, data = train_df)

      pred <- boot_pi(model, test_df)

      forecast_df <- tibble(
        startweek = start_idx,
        actualweek = start_idx + (1:horizon - 1),
        preds = pred$pred,
        lower = pred$lower,
        upper = pred$upper,
      )

      forecast_df
    },
    cl = cl
  ) %>% list_c()
}
