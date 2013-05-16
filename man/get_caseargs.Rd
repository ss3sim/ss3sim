\name{get_caseargs}
\alias{get_caseargs}
\title{Takes a scenario and returns file names of argument input files}
\usage{
  get_caseargs(folder, scenario, delimiter = "-",
    ext = ".txt", case_vals = c("M", "F", "D", "R"),
    case_files = list(M = "M", F = "F", D = c("index", "lcomp", "agecomp"), R = "R"))
}
\arguments{
  \item{folder}{The folder to look for input files in.}

  \item{scenario}{A character object with the cases. E.g.
  \code{"M1-F1-D1-R1"}}

  \item{delimiter}{The delimiter between the cases.
  Defaults to a dash.}

  \item{ext}{The file extension of the input files.
  Defaults to .txt.}

  \item{case_vals}{The cases that make up the scenario ID.
  In the example above the \code{case_vals} would be
  \code{c("M", "F", "D", "F")}}

  \item{case_files}{A named list that relates the
  \code{case_vals} to the files to return. If each
  \code{case_val} has only one file then this is simple.
  See the default values for a more complicated case.}
}
\value{
  A (nested) named list. The first level of the named list
  refers to the \code{case_vals}. The second level of the
  named list refers to the argument names (the first column
  in the input text files). The contents of the list are
  the argument values themselves (the second column of the
  input text files).
}
\description{
  This function calls a number of internal functions to go
  from a unique scenario identifier like
  \code{"M1-F2-D3-R4"} and read the corresponding input
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
  Multiple words should be enclosed in quotes. Vectors
  (\code{"c(1, 2, 3}") should also be enclosed in quotes as
  shown.
}
\examples{
\dontrun{
# Given the default values for this function,
# this example assumes you have files named:
# M1.txt, F2.txt, index3.txt, lcomp3.txt, agecomp3.txt, R4.txt
# Each text file should be comma delimited with quote characters
# around vectors like "c(1, 2, 3)" or around sets of words.
# To get data that this example works with, download the package
# source and setwd() to the root "ss3sim" folder.

get_caseargs("inst/extdata/", "M1-F2-D3-R4")

# The following output is returned:
#$M
#$M$a
#[1] 1
#
#$M$d
#[1] 7
#
#$F
#$F$b
#[1] "c(1, 2, 3, 4)"
#
#$F$foo
#[1] "Some words"
#
#$index
#$index$c
#[1] 3
#
#$lcomp
#$lcomp$d
#[1] 8
#
#$agecomp
#$agecomp$z
#[1] 99
#
#$R
#$R$g
#[1] 1
}
}

