#' Example scalar data from the Introduction vignette
#'
#' An R object read in using `utils::read.csv("ss3sim_scalar.csv")` after
#' processing the results from the Introduction vignette
#' using [get_results_all()]. The data set is available
#' so users do not have to wait for the scenarios to run.
#'
#' @name scalar_dat
#' @docType data
#' @keywords data
#' @examples
#' data("scalar_dat", package = "ss3sim")
#' \dontshow{
#' testthat::expect_true("max_grad" %in% colnames(scalar_dat),
#'   label = "the column 'max_grad' of the scalar data set"
#' )
#' }
#'
NULL
