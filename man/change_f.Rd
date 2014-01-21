\name{change_f}
\alias{change_f}
\title{Alter the fishing mortality (F) values in an \code{ss3.par} file}
\usage{
change_f(years, years_alter, fvals, file_in = "ss3.par",
  file_out = "ss3.par")
}
\arguments{
  \item{years}{Vector of years for which F values are
  specified}

  \item{years_alter}{Vector of years for the which F values
  will be altered}

  \item{fvals}{Vector of F values to be entered into
  \code{ss3.par} file}

  \item{file_in}{Input SS3 par file.}

  \item{file_out}{Output SS3 par file.}
}
\value{
A modified SS3 \code{.par} file.
}
\description{
Takes an SS3 \code{.par} file and changes the F values for
specified years.
}
\examples{
# Create a temporary folder for the output:
temp_path <- file.path(tempdir(), "ss3sim-f-example")
dir.create(temp_path, showWarnings = FALSE)

# Find the example .par file in the package data:
d <- system.file("extdata", package = "ss3sim")
par_file <- paste0(d, "/change_f/ss3.par")

change_f(years = 1:49, years_alter = 2, fvals = 9999, file_in =
par_file, file_out = paste0(temp_path, "/test.par"))
}
\author{
Curry James Cunningham
}

