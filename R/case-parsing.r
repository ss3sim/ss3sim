#' Take a csv file, read it, and turn the first column into the list
#' names and the second column into the list values.
#' @param file The file name as character
get_args <- function(file) {

  x <- read.csv(file, stringsAsFactors = FALSE, col.names =
    c("arg", "val"), header = FALSE, strip.white = TRUE, sep = ";",
    comment.char = "#")
  y <- as.list(x$val)
  names(y) <- x$arg

# if all numeric then eval(parse(text =
# if has [a-zA-Z]( then eval(parse(text =
# if has : then eval(parse(text =
# else use as character
  lapply(y, function(z) {
    if(is_f(as.character(z))) {
      eval(parse(text = z)) # turn into correct class
    } else {
      as.character(z)
    }}
    )
}

#' Check if we should leave as character or convert to another class
#'
#' @param x A character object
is_f <- function(x) {
  if(!is.character(x)) stop("x must be a character")
  fn <- grepl("[a-zA-Z0-9]\\(", x) # is a function
  nu <- !grepl("[a-zA-Z]", x) # is not character (is numeric)
  ## If a function, numeric, NULL or NA treat as a function
  ifelse(fn | nu | x== "NULL" | x== "NA", TRUE, FALSE)
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
  # get the case number that is up to 9 digits long:
  as.numeric(substr(x[grep(case, x)], 2, 9))
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
#' "D", "R")}
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
#'   FALSE, sep = "; ", quote = FALSE)
#' wt(data.frame("a", 1), "M1-cod.txt")
#' wt(data.frame("b", "Some words"), "F2-cod.txt")
#' wt(data.frame("d", 1), "index3-cod.txt")
#' wt(data.frame("d", 1), "agecomp3-cod.txt")
#' wt(data.frame(c("e", "f"), c(1, 99)), "lcomp3-cod.txt")
#' wt(data.frame("c", "c(1, 2, 3)"), "R4-cod.txt")
#'
#' get_caseargs(".", "M1-F2-D3-R4-cod")
#'
#' # Clean up the files created above:
#' file.remove(c("M1-cod.txt", "F2-cod.txt", "index3-cod.txt",
#' "agecomp3-cod.txt", "lcomp3-cod.txt", "R4-cod.txt"))
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
#' # [1] c(1, 2, 3)
#' }
#' @export
get_caseargs <- function(folder, scenario, delimiter = "-", ext = ".txt",
  case_vals = c("M", "F", "D", "R", "S", "G", "E"), case_files = list(M = "M", F =
    "F", D = c("index", "lcomp", "agecomp"), R = "R", S = "S", G = "G", E = "E")) {
  spp <- substr_r(scenario, 3) # take 3 last characters
  case_vals <- sapply(case_vals, function(x)
    get_caseval(scenario, x, delimiter))
  args_out <- vector("list", length = length(case_files))
  names(args_out) <- names(case_files)
  for(i in 1:length(case_files)) {
    args_out[[i]] <- paste0(case_files[[i]], case_vals[i], "-", spp, ext)
  }
  args_out2 <- unlist(args_out)
  names(args_out2) <- unlist(case_files)
  argvalues_out <- lapply(args_out2, function(x) get_args(pastef(folder, x)))

  # TODO this should check for the value of "function_type" too
  # now, check for all "function_type = change_tv" and concatenate these
  # into a list to pass to change_param()
  change_param_args <- sapply(argvalues_out, function(x) {
    if("function_type" %in% names(x)) {
      y <- list(temporary_name = x$dev)
      names(y) <- x$param
      y
      }
    })

  # remove elements that aren't time varying:
  args_null <- sapply(change_param_args, function(x) is.null(x))
  if(!length(which(args_null)) == length(args_null)) { # some are time varying
    change_param_args[which(args_null)] <- NULL
    # and re-arrange to pass to change_params
    change_param_args_short <- lapply(change_param_args, "[[", 1)
    names(change_param_args_short) <- sapply(change_param_args, function(x) names(x))
  } else {
    change_param_args_short <- NULL
  }

# remove time varying elements from argvalues_out:
  argvalues_out <- argvalues_out[which(args_null)]

# and concatenate on the time varying arguments
  c(argvalues_out, list(tv_params = change_param_args_short))

}

#' Substring from right
#'
#' @references
#' http://stackoverflow.com/questions/7963898/extracting-the-last-n-characters-from-a-string-in-r
#' @param x A character object
#' @param n The number of characters from the right to extract
substr_r <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

