#' Read in the data.ss_new file and move it to the em folder

extract_expected_data <- function(data_ss_new = "data.ss_new",
  data_out = "data.dat") {
  data_file <- readLines(data_ss_new)
  data_file_new <- data_file[(
    grep("#_expected values with no error added", 
      data_file, fixed=TRUE)+1):(grep("#_bootstrap file: 1", 
      data_file, fixed=TRUE)-1)]
    writeLines(data_file_new, con = data_out)
}
