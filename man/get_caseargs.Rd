\name{get_caseargs}
\alias{get_caseargs}
\title{Take a scenario ID and return argument lists to pass to functions}
\usage{
  get_caseargs(folder, scenario, delimiter = "-",
    ext = ".txt",
    case_files = list(M = "M", F = "F", D = c("index", "lcomp", "agecomp"), R = "R", E 
= "E"))
}
\arguments{
  \item{folder}{The folder to look for input files in.}

  \item{scenario}{A character object that has the cases
  separated by some delimiter. The combination of cases is
  referred to as a scenario. E.g. \code{"M1-F1-D1-R1-S1"}.}

  \item{delimiter}{The delimiter between the cases.
  Defaults to a dash.}

  \item{ext}{The file extension of the input files.
  Defaults to \code{".txt"}.}

  \item{case_files}{A named list that relates the case IDs
  to the files to return.}
}
\value{
  A (nested) named list. The first level of the named list
  refers to the \code{case_files}. The second level of the
  named list refers to the argument names (the first column
  in the input text files). The contents of the list are
  the argument values themselves (the second column of the
  input text files).
}
\description{
  This function calls a number of internal functions to go
  from a unique scenario identifier like
  \code{"M1-F2-D3-R4-cod"} and read the corresponding input
  files (like \code{"M1.txt"}) that have two columns: the
  first column contains the argument names and the second
  column contains the argument values. The two columns
  should be separated by a comma. The output is then
  returned in a named list.
}
\details{
  The input plain text files should have arguments in the
  first column that should be passed on to functions. The
  names should match exactly. The second column should
  contain the values to be passed to those arguments.
  Multiple words should be enclosed in quotes.

  You can use any simple R syntax to declare argument
  values. For example: \code{c(1, 2, 4)}, or \code{seq(1,
  100)}, or \code{1:100}, or \code{matrix()}, or
  \code{NULL}. Character objects don't need to be quoted,
  but can be if you'd like. However, be careful not to use
  the delimiter (set up as a semicolon) anywhere else in
  the file besides to denote columns. You can add comments
  after any \code{\#} symbol. Internally, the functions
  evaluate in \code{R} any entries that have no character
  values (e.g. \code{1:100}), or have an alpha-numeric
  character followed by a \code{(}. Anything that is
  character only or has character mixed with numeric but
  doesn't have the regular expression \code{"[A-Za-z0-9]("}
  gets turned into a character argument. (\code{NA} and
  \code{NULL} are special cases that are also passed on
  directly.)
}
\examples{
\dontrun{
# Find the example data folders:
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")

# An example using the cases defined by default:
get_caseargs(case_folder, scenario = "D0-E0-F0-M0-R0-cod")

# With a custom time-varying case for selectivity, which we'll call
# the S case. Here, we'll need to define which file the case S should
# read from ("S*-cod.txt"):
get_caseargs(case_folder, scenario = "D0-E0-F0-M0-R0-S0-cod",
  case_files = list(E = "E", D = c("index", "lcomp", "agecomp"), F =
    "F", M = "M", R = "R", S = "S"))
}
}

