#' Return a set of recruitment deviations
#'
#' This function returns a set of pseudo-random recruitment deviations based
#' on an iteration number. Given the same iteration number the function will
#' return the same recruitment deviations. The deviations are standard normal.
#' I.e., they have a mean of 0 and a standard deviation of 1.
#' @param iteration The iteration number. This is used as an ID to set the
#'   random number seed.
#' @param n The length of the vector returned.
#' @param seed An integer value to pass to \code{\link[base]{set.seed}}.
#' @return A vector of standard normal recruitment deviations.
#' @examples
#' get_recdevs(1, 10)
#' get_recdevs(1, 10)
#' get_recdevs(2, 10)
#' @export

get_recdevs <- function(iteration, n, seed = 21) {
  set.seed(seed)
  x <- sample(1:1e6)[iteration]
  set.seed(x)
  rnorm(n, 0, 1)
}
