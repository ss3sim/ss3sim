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
  x1 <- grep("#_expected values with no error added", data_file, fixed=TRUE)+1
  x2 <- grep("ENDDATA", data_file)-2
  if(length(x1)!=1 | length(x2)!=1)
      stop("grep failed to find lines for extracting data, not of length 1")
  if(x2<x1)
      stop("Something wrong with OM data.ss_new file, last line comes before first line")
  data_file_new <- data_file[x1:x2]
  if(999 != as.numeric(data_file_new[length(data_file_new)]))
     warning("last line of extracted expected values was not 999, was it read in correctly?")
  writeLines(data_file_new, con = data_out)
}
