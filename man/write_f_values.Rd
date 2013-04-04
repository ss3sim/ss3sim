\name{write_f_values}
\alias{write_f_values}
\title{Alter the Fvalues in an ss3.par file}
\usage{
  write_f_values(years, years_alter, fvals,
    file_in = "ss3.par", file_out = file_in)
}
\arguments{
  \item{years}{Vector of years for which Fvalues are
  specified}

  \item{years_alter}{Vector of years for which Fvalue will
  be altered}

  \item{fvals}{Vector of Fvalues to be entered into ss3.par
  file}

  \item{file_in}{Input SS3 par file.}

  \item{file_out}{Output SS3 par file. Defaults to
  overwriting}
}
\description{
  Alter the Fvalues in an ss3.par file
}
\details{
  Changed to allow for specific years in series to be
  altered (this may be unnecessary given our purpose)
}
\author{
  Curry James Cunningham
}

