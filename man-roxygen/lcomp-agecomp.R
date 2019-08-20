#' @param ESS The final effective sample size (ESS) associated with the
#' simulated data. The ESS is not used to generate the simulated data
#' but can be used as an input sample size in subsequent models that estimate
#' population parameters or status.
#' The default, \code{NULL}, leads to the true (internally calculated)
#' #' ESS being used, which is \code{Nsamp} for the multinomial case or given
#' by the formula under \code{cpar} for the Dirichlet case.
#' At least one value must be provided for each fleet or a vector of
#' year-specific values can be used for any given fleet.
#' The argument accepts a list with entries,
#' either a single integer or a vector of integers, for each fleet.
#' @param cpar A numeric value or vector the same length as
#' \code{fleets} controlling the variance of the Dirichlet
#' distribution used for sampling. A value of \code{1} leads to the
#' same standard deviation as a multinomial of the given \code{Nsamp},
#' \code{2} indicates twice, etc. Values greater than one indicate
#' overdispersion, and less underdispersion. \code{NULL} or \code{NA}
#' for a given fleet will lead to no dispersion.
