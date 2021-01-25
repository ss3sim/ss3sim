#' Sample with a multinomial distribution
#'
#' @param data A data frame with one row.
#' @param n The desired sample size.
#' 
#' @return A data frame with one row because right now the input
#' data should only be a single row of data.
sample_mn <- function(data, n) {
  xxx <- stats::rmultinom(1, size = n, prob = prop.table(data))
  return(t(xxx))
}
