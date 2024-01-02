# generate any covar from name
gen_covar_df <- function(covar_name, agg_fn, trans_fn = identity, ..., new_covar_name = NULL, dataset = raw_weather) {
  # browser()
  dataset %>%
    select(all_of(covar_name)) %>%
    aggregate("1 week", FUN = agg_fn) %>%
    aggregate(hcmc_shp_w_buff, FUN = agg_fn) %>%
    as_tibble() %>%
    select(all_of(c("time", covar_name))) %>%
    mutate(
      date = as.Date(time) %>% yearweek(),
      !!ifelse(is.null(new_covar_name), covar_name, new_covar_name) := trans_fn(!!rlang::sym(covar_name), ...)
    ) %>%
    select(all_of(c("date", ifelse(is.null(new_covar_name), covar_name, new_covar_name)))) %>%
    as_tsibble(index = date)
}

gen_mean_covar_df <- function(covar_name, trans_fn = identity, ..., new_covar_name = NULL, dataset = raw_weather) {
  gen_covar_df(
    covar_name = covar_name,
    agg_fn = mean,
    trans_fn = trans_fn, ...,
    new_covar_name = new_covar_name,
    dataset = dataset
  )
}

plot_covar <- function(covar_df, scale_y_label, title, extra_layers = list(), ...) {
  covar_name <- names(covar_df)[[2]]

  covar_df %>%
    ggplot(aes(x = date, y = .data[[covar_name]], ...)) +
    geom_line() +
    geom_smooth() +
    scale_x_yearweek(
      "Date",
      breaks = yearweek(paste0(seq(1999, 2023, 1), " W01")), minor_breaks = NULL,
      labels = as.character(seq(1999, 2023, 1))
    ) +
    scale_y_continuous(scale_y_label) +
    ggtitle(title) +
    extra_layers
}
