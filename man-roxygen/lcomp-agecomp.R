#' @param ESS An effective sample size to write to file (but not use in
#' sampling). The default of NULL means to use the true (internally
#' calculated) ESS, which is \code{Nsamp} for the multinomial case or given
#' by the formula under \code{cpar} for the Dirichlet case. Must match the
#' structure of the \code{years} arguments.
#' @param cpar *A numeric value or vector the same length as
#' \code{fleets} controlling the variance of the Dirichlet
#' distribution used for sampling. A value of \code{1} indicates the
#' same standard deviation as a multinomial of the given \code{Nsamp},
#' \code{2} indicates twice, etc. Values greater than one indicate
#' overdispersion, and less underdispersion.
