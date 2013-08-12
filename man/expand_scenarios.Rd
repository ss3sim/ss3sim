\name{expand_scenarios}
\alias{expand_scenarios}
\title{Create vectors of scenarios from inputs.}
\usage{
  expand_scenarios(e = 0, d = 0, f = 0, g = 0, m = 0,
    r = 0, s = 0, species = c("cod", "fla", "sar"))
}
\arguments{
  \item{e}{Integer values of estimation cases to be
  expanded}

  \item{d}{Integer values of data cases to be expanded}

  \item{f}{Integer values of fishing mortality cases to be
  expanded}

  \item{g}{Integer values of growth cases to be expanded}

  \item{m}{Integer values of natural mortality cases to be
  expanded}

  \item{r}{Integer values of retrospective cases to be
  expanded}

  \item{s}{Integer values of selectivity cases to be
  expanded}

  \item{species}{Vector of characters in
  \code{c("cod","fla","sar")}}
}
\description{
  Create vectors of scenarios from inputs. For passing to
  \code{\link{run_ss3sim}}, \code{\link{run_fish600}}, or
  \code{\link{get_results_all}}. Default case values are
  the base case (\code{0}).
}
\examples{
expand_scenarios()
expand_scenarios(f=0:2, species="cod")
}
\author{
  Cole Monnahan
}

