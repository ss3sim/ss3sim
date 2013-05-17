#' Take a csv file, read it, and turn the first column into the list
#' names and the second column into the list values.
#' @param file The file name as character
get_args <- function(file) {
  x <- read.csv(file, stringsAsFactors = FALSE, col.names =
    c("arg", "val"), header = FALSE, strip.white = TRUE)
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
# @examples
# get_caseval("M1-F1-D1-R1", "M")
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

#' Take a scenario ID and return argument lists to pass to functions
#'
#' This function calls a number of internal functions to go from a
#' unique scenario identifier like \code{"M1-F2-D3-R4-cod"} and read the
#' corresponding input files (like \code{"M1.txt"}) that have two
#' columns: the first column contains the argument names and the
#' second column contains the argument values. The two columns should
#' be separated by a comma. The output is then returned in a named
#' list. 
#'
#' @details
#' The input plain text files should have arguments in the first
#' column that should be passed on to functions. The names should
#' match exactly. The second column should contain the values to be
#' passed to those arguments. Multiple words should be enclosed in
#' quotes. Vectors (\code{"c(1, 2, 3"}) should also be enclosed in
#' quotes as shown.
#'
#' @param folder The folder to look for input files in.
#' @param scenario A character object that has the cases separated by
#' some delimiter. The combination of cases is referred to as a
#' scenario. E.g. \code{"M1-F1-D1-R1-S1"}. 
#' @param delimiter The delimiter between the cases. Defaults to a
#' dash.
#' @param ext The file extension of the input files. Defaults to
#' \code{".txt"}.
#' @param case_vals The case types that make up the scenario ID. In
#' the example above the \code{case_vals} would be \code{c("M", "F",
#' "D", "F")}
#' @param case_files A named list that relates the \code{case_vals} to
#' the files to return. If each \code{case_val} has only one file then
#' this is simple. See the default values for a more complicated case.
#' @return
#' A (nested) named list. The first level of the named list refers to
#' the \code{case_files}. The second level of the named list refers to
#' the argument names (the first column in the input text files). The
#' contents of the list are the argument values themselves (the second
#' column of the input text files).
#'
#' @examples \dontrun{
#' # Create some demo input files first:
#' wt <- function(x, f) write.table(x, f, row.names = FALSE, col.names =
#'   FALSE, sep = ",")
#' wt(data.frame("a", 1), "M1.txt")
#' wt(data.frame("b", "Some words"), "F2.txt")
#' wt(data.frame("d", 1), "index3.txt")
#' wt(data.frame("d", 1), "agecomp3.txt")
#' wt(data.frame(c("e", "f"), c(1, 99)), "lcomp3.txt")
#' wt(data.frame("c", "c(1, 2, 3)"), "R4.txt")
#'
#' get_caseargs(".", "M1-F2-D3-R4-cod")
#'
#' # Clean up the files created above:
#' file.remove(c("M1.txt", "F2.txt", "index3.txt", "agecomp3.txt",
#' "lcomp3.txt", "R4.txt"))
#' 
#' # The following output is returned:
#' # $M
#' # $M$a
#' # [1] 1
#' # 
#' # $F
#' # $F$b
#' # [1] "Some words"
#' # 
#' # $index
#' # $index$d
#' # [1] 1
#' # 
#' # $lcomp
#' # $lcomp$e
#' # [1] 1
#' # 
#' # $lcomp$f
#' # [1] 99
#' # 
#' # $agecomp
#' # $agecomp$d
#' # [1] 1
#' # 
#' # $R
#' # $R$c
#' # [1] "c(1, 2, 3)"
#' }
#' @export
get_caseargs <- function(folder, scenario, delimiter = "-", ext = ".txt",
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
  args_out2 <- unlist(args_out)
  names(args_out2) <- unlist(case_files)
  argvalues_out <- lapply(args_out2, function(x) get_args(pastef(folder, x)))
  argvalues_out
}

