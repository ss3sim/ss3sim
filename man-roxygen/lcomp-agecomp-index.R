#' @param outfile A character string of the new file name to be created.
#' Must end in \code{.dat} or equal \code{wtatage.ss}.
#' @param fleets *A numeric vector giving the fleets to be used. This order also
#' pertains to other arguments. A missing value excludes that fleet from
#' \code{outfile} (i.e. it turns it off so no samples are written). If none of
#' the fleet collected samples, keep the value to \code{fleets=NULL}.
#' @param years *A list the same length as \code{fleets} giving the years as
#' numeric vectors. If no fleet collected samples, keep the value to
#' \code{years=NULL}.
#' @param write_file A logical switch for whether to write \code{outfile} to
#' disk. Can be turned off to speed up testing or exploration of the
#' function. The new data are returned invisibly, as in the examples
#' below.
