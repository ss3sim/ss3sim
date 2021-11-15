#' Sample with a Dirichlet-Multinomial distribution
#'
#' @param data A data frame with one row.
#' @param n The desired sample size.
#' @param par The cpar value to define overdispersion.
#'
#' @return A data frame with one row because right now the input
#' data should only be a single row of data.
sample_dm <- function(data, n, par) {
  lambda <- n / par^2 - 1
  if (lambda < 0 | is.na(lambda)) {
    return(rep(NA, length(data)))
  }
  # replace Nsamp with effective sample size
  xxx <- gtools::rdirichlet(1, unlist(prop.table(data) * lambda))
  return(xxx)
}
