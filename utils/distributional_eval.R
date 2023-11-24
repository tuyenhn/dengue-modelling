# Quantile score - from `fabletools`
q_score <- function(dist, y, probs) {
  percentiles <- map(probs, quantile, x = dist)

  2 * mean(
    map2_dbl(
      percentiles, probs,
      function(percentile, prob) {
        L <- ifelse(y < percentile, (1 - prob), prob) * abs(percentile - y)
        mean(L, na.rm = TRUE)
      }
    ),
    na.rm = TRUE
  )
}

# Winkler score
w_score <- function(dist, y, level) {
  alpha <- 1 - (level / 100)
  (q_score(dist, y, alpha / 2) + q_score(dist, y, 1 - alpha / 2)) / alpha
}

# Continuous Ranked Probability Score (CRPS)
crp_score <- function(dist, y) {
  dist_type <- family(dist)
  if (!all(dist_type == "normal") && !all(dist_type == "transformed")) {
    stop("Only works for normal distribution at the moment")
  }

  mu <- mean(dist)
  sigma <- sqrt(distributional::variance(dist))
  z_score <- (y - mu) / sigma
  
  crps <- sigma * ((1 / sqrt(pi)) - 2 * dnorm(z_score) - (z_score * (2 * pnorm(z_score) - 1)))

  mean(crps)
}
