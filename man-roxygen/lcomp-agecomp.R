#' @param Nsamp *A numeric list of the same length as \code{fleets}.
#' Either single values or vectors of the same length as the number of
#' years can be passed through. Single values are repeated for all
#' years. If no fleet collected samples, keep the value to
#' \code{Nsamp=NULL}.
#' @param cpar *A numeric value or vector the same length as
#' \code{fleets} controlling the variance of the Dirichlet
#' distribution used for sampling. A value of \code{1} indicates the
#' same standard deviation as a multinomial of the given \code{Nsamp},
#' \code{2} indicates twice, etc. Values greater than one indicate
#' overdispersion, and less underdispersion.


