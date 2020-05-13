#' Example time-series data from the Introduction vignette
#'
#' An R object read in using \code{read.csv("ss3sim_ts.csv")} after
#' processing the results from the Introduction vignette
#' using \code{get_results_all()}. The data set is available
#' so users do not have to wait for the scenarios to run.
#'
#' @name ts_dat
#' @docType data
#' @keywords data
#' @examples
#' data("ts_dat", package = "ss3sim")
#'
#' \dontshow{
#' testthat::expect_true("year" %in% colnames(ts_dat),
#'   label = "the column 'year' of the time-series data set")
#' }
#'
NULL
