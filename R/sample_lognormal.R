#' Sample observations using log-normal error corrected for bias
#'
#' Sample a standard normal in log-space and apply the error to observations.
#'
#' @details
#' Newly sampled values are calculated
#' \eqn{obs*exp(stats::rnorm(1, 0, sd)-sd^2/2)}.
#' The second term adjusts the random samples so that their expected value is
#' `obs`, i.e., the log-normal bias correction.
#'
#' @param obs A vector of observed values you wish to sample with
#' log-normal error.
#' @param sd A vector of standard deviations to use in
#' [stats::rnorm()].
#' @author Cole Monnahan
#'
sample_lognormal <- function(obs, sd) {
  obs * exp(stats::rnorm(n = 1, mean = 0, sd = sd) - sd^2 / 2)
}
