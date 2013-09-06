#' Extract the expected data values
#'
#' Read in a \code{data.ss_new} file, move the expected values up in
#' the file, and write it back out to a new data file.
#'
#' @param data_ss_new The location of the \code{.ss_new} file that was
#' generated from a run of SS.
#' @param data_out The location of the \code{.ss_new} file that was
#' generated from a run of SS.                                                  
#' @author Kotaro Ono

extract_expected_data <- function(data_ss_new = "data.ss_new",
  data_out = "ss3.dat") {
  data_file <- readLines(data_ss_new)
  data_file_new <- data_file[(
    grep("#_expected values with no error added", 
      data_file, fixed=TRUE)+1):(grep("#_bootstrap file: 1", 
      data_file, fixed=TRUE)-1)]
    writeLines(data_file_new, con = data_out)
}
