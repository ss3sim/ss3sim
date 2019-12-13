#' @param fleets *A vector of integers specifying which fleets to include.
#' The order of the fleets pertains to the input order of other arguments.
#' An entry of \code{fleets=NULL} leads to zero samples for any fleet. 
#' @param years *A list the same length as \code{fleets} giving the years as
#' numeric vectors. If no fleet collected samples, keep the value to
#' \code{years=NULL}.
