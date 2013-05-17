\name{create_argfiles}
\alias{create_argfiles}
\title{Create template argument input files}
\usage{
  create_argfiles(functions = c(`lcomp0-spp` = "change_lcomp", `agecomp0-spp` = "change_agecomp", `index0-spp` = "change_index", `M0-spp` = "change_m", `F0-spp` = "change_f"),
    ext = ".txt", delim = "; ", ...)
}
\arguments{
  \item{functions}{A named vector. The names correspond to
  the filenames that will get written. The values
  correspond to the functions to grab the arguments from.}

  \item{ext}{The file extension to create the configuration
  files with. Defaults to \code{".txt"}.}

  \item{delim}{The delimiter. Defaults to \code{"; "}.}

  \item{...}{Anything else to pass to \code{write.table}.}
}
\description{
  Creates template input files based on the argument lists
  for specified functions. Look in your working directory
  for the template files. Change the case ID number
  (defaults to \code{0}) and the species identifier to a
  three letter identifier. For the FISH600 project use one
  of \code{cod}, \code{sar}, or \code{fla} for cod,
  sardine, or flatfish.
}
\examples{
\dontrun{
create_argfiles()
}
}
\author{
  Sean Anderson
}

