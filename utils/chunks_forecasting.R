forecast_chunks <- function(
    train_set, test_set,
    start_indices,
    model_fn, formula, ...,
    horizon = 4, cl = NULL) {
  if (!missing(cl)) {
    clusterEvalQ(cl, suppressPackageStartupMessages(library(dplyr)))
    clusterEvalQ(cl, library(fabletools))
    clusterExport(cl, c("train_set", "test_set"), environment())
  }
  model_fn <- match.fun(model_fn)
  
  # browser()
  
  pbapply::pblapply(start_indices, \(start_idx){
    train_df <- train_set %>%
      bind_rows(test_set %>% slice_head(n = start_idx)) %>%
      tail(nrow(train_set))

    fit <- train_df %>% model(model = model_fn(!!enquo(formula), ...))

    new_data <- test_set %>%
      slice_tail(n = 52 - start_idx) %>%
      head(horizon)

    fit %>%
      forecast(new_data = new_data) %>%
      as_tibble() %>%
      mutate(
        .model = format(fit$model),
        startweek = start_idx,
        uppers = hilo(n)$upper,
        lowers = hilo(n)$lower
      )
  }, cl = cl) %>%
    list_c()
}
