\name{get_caseval}
\alias{get_caseval}
\title{Take a vector of cases and return the case number}
\usage{
  get_caseval(cases, case, delimiter = "-")
}
\arguments{
  \item{cases}{A character object with the cases. E.g.
  \code{"M1-F1-D1-R1"}}

  \item{case}{The case you want to extract. E.g.
  \code{"M"}}

  \item{delimiter}{The delimiter between the cases.
  Defaults to a dash.}
}
\description{
  Take a vector of cases and return the case number
}
\examples{
get_caseval("M1-F1-D1-R1", "M")
}

