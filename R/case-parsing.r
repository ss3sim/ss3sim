#' Take a csv file, read it, and turn the first column into the list
#' names and the second column into the list values.
#' @param file The file name as character
get_args <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE, col.names =
    c("arg", "val"), header = FALSE)
  y <- as.list(x$val)
  names(y) <- x$arg
  y
}

#' Take a vector of cases and return the case number
#'
#' @param scenario A character object with the cases. E.g.
#' \code{"M1-F1-D1-R1"}
#' @param case The case you want to extract. E.g. \code{"M"}
#' @param delimiter The delimiter between the cases. Defaults to a
#' dash.
#' @examples
#' get_caseval("M1-F1-D1-R1", "M")
get_caseval <- function(scenario, case, delimiter = "-") {
  if(!grepl(delimiter, scenario)) 
    stop("Your case string doesn't contain your delimiter.")
  if(!is.character(scenario))
    stop("cases must be of class character")
  if(!is.character(case))
    stop("case must be of class character")
  x <- strsplit(scenario, "-")[[1]]
  as.numeric(substr(x[grep(case, x)], 2, 2))
}

#' Takes a scenario and returns file names of argument input files
#'
#' This function calls a number of internal functions to go from a
#' unique scenario identifier like \code{"M1-F2-D3-R4"}, read
#' corresponding input files (like \code{"M1.txt"}) that have two
#' columns: the first column contains the argument names and the
#' second column contains the argument values. The output is returned
#' in a named list.
#'
#' @param scenario A character object with the cases. E.g.
#' \code{"M1-F1-D1-R1"}
#' @param delimiter The delimiter between the cases. Defaults to a
#' dash.
#' @param ext The file extension of the input files. Defaults to .txt.
#' @param case_vals The cases that make up the scenario ID. In the
#' example above the \code{case_vals} would be \code{c("M", "F", "D",
#' "F")}
#' @param case_files A named list that relates the \code{case_vals} to
#' the files to return. If each \code{case_val} has only one file then
#' this is simple. See the default values for a more complicated case.
#' @return
#' A (nested) named list. The first level of the named list refers to
#' the \code{case_vals}. The second level of the named list refers to
#' the argument names (the first column in the input text files). The
#' contents of the list are the argument values themselves (the second
#' column of the input text files).
#' @examples \dontrun{
#' # Given the default values for this function,
#' # this example assumes you have files named:
#' # M1.txt, F2.txt, index3.txt, lcomp3.txt, agecomp3.txt, R4.txt
#' # Each text file should be comma delimited with quote characters
#' # around vectors like "c(1, 2, 3)" or around sets of words.
#' get_argfiles("M1-F2-D3-R1")
#' }
#' @export
get_caseargs <- function(scenario, delimiter = "-", ext = ".txt",
  case_vals = c("M", "F", "D", "R"), case_files = list(M = "M", F =
    "F", D = c("index", "lcomp", "agecomp"), R = "R")) {
  case_vals <- sapply(case_vals, function(x)
    get_caseval(scenario, x, delimiter))
  paste0(names(case_vals), case_vals)
  args_out <- vector("list", length = length(case_files))
  names(args_out) <- names(case_files)
  for(i in 1:length(case_files)) {
    args_out[[i]] <- paste0(case_files[[i]], case_vals[i], ext)
  }
  argvalues_out <- lapply(unlist(args_out), function(x) get_args(x))
  argvalues_out
}

