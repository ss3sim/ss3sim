\name{create_argfiles}
\alias{create_argfiles}
\title{Create template argument input files}
\usage{
  create_argfiles(functions = c(`lcomp0-spp` = "change_lcomp", `agecomp0-spp` = "change_agecomp", `index0-spp` = "change_index", `M0-spp` = "change_m", `F0-spp` = "change_f"),
    ext = ".txt", delim = "; ",
    ignore = c("file", "dir", "make_plot"), ...)
}
\arguments{
  \item{functions}{A named vector. The names correspond to
  the filenames that will get written. The values
  correspond to the functions to grab the arguments from.}

  \item{ext}{The file extension to create the configuration
  files with. Defaults to \code{".txt"}.}

  \item{delim}{The delimiter. Defaults to \code{"; "}.}

  \item{ignore}{A vector of character object of arguments
  to ignore in the arguments. Found via \code{grep} so can
  be part of an argument name.}

  \item{...}{Anything else to pass to \code{write.table}.}
}
\description{
  Creates template input files based on the argument lists
  for specified functions. Look in your working directory
  for the template files. Change the case ID number
  (defaults to \code{0}) and the species identifier to a
  three letter identifier. For the FISH600 project use one
  of \code{cod}, \code{sar}, or \code{fla} for cod,
  sardine, or flatfish. An example filename would be
  \code{M1-sar.txt} or \code{lcomp2-fla.txt}.
}
\details{
  The first column in the text files denotes the argument
  to be passed to a function. The second argument denotes
  the value to be passed. You can use any simple R syntax.
  For example: \code{c(1, 2, 4)}, or \code{seq(1, 100)} or
  \code{1:100} or \code{matrix()}. Character objects don't
  need to be quoted. However, be careful not to use your
  delimiter (set up as a semicolon) anywhere else in the
  file besides to denote columns.
}
\examples{
\dontrun{
create_argfiles()
# Some example input lines:
#
# year1; 1990
# years; 1990:2000
# years; c(1980, 1990, 1995)
# survey_type; fishery
}
}
\author{
  Sean Anderson
}

