metrics_plot <- function(frcst_res_df, dates) {
  frcst_res_df %>%
    slice(rep(1:n(), each = 4)) %>%
    mutate(date = dates, .before = everything()) %>%
    pivot_longer(-c(date, startweek), names_to = "Metric") %>%
    ggplot() +
    geom_line(aes(x = date, y = value, color = Metric)) +
    scale_x_yearweek(
      "Week",
      breaks = test_weekly_df$date_admitted[seq(0, 52, 3) + 1],
      labels = as.character(test_weekly_df$date_admitted[seq(0, 52, 3) + 1]) %>% str_extract("W\\d+"),
      minor_breaks = NULL
    ) +
    scale_y_continuous("Metric value")
}

forecast_layer <- function(frcst_df, funnel = TRUE) {
  # TODO: frcst_df schema
  frcst_df %<>%
    group_by(startweek)

  if (funnel) {
    frcst_df %<>%
      group_map(\(.x, .y){
        target_date <- min(.x$date_admitted) - 1
        target_row <- incidence_weekly_df %>%
          filter(date_admitted == target_date) %>%
          mutate(.mean = n, uppers = n, lowers = n, startweek = .y$startweek)

        .x %>%
          mutate(startweek = .y$startweek) %>%
          add_row(target_row, .before = 0)
      }) %>%
      list_c() %>%
      group_by(startweek)
  }

  lapply(
    frcst_df %>% group_split(),
    \(df)
    c(layer(
      data = df,
      geom = "ribbon", stat = "identity", position = "identity",
      mapping = aes(x = date_admitted, ymin = lowers, ymax = uppers),
      params = list(alpha = 0.5, fill = "pink")
    ), layer(
      data = df,
      geom = "line", stat = "identity", position = "identity",
      mapping = aes(x = date_admitted, y = .mean), params = list(color = "red")
    ))
  )
}
