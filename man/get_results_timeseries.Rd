\name{get_results_timeseries}
\alias{get_results_timeseries}
\title{Extract time series from a model run.}
\usage{
  get_results_timeseries(report.file)
}
\arguments{
  \item{report.file}{An \code{SS_output} list for a model
  (OM or EM).}
}
\description{
  Extract time series from a model run. Returns a
  data.frame of the results (single row) which can be
  rbinded later.
}
\author{
  Cole Monnahan
}
\seealso{
  Other get.results: \code{\link{get_results_all}},
  \code{\link{get_results_scalar}},
  \code{\link{get_results_scenario}}
}

