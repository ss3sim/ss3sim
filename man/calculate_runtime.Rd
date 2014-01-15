\name{calculate_runtime}
\alias{calculate_runtime}
\title{Calculate run time}
\usage{
calculate_runtime(start_time, end_time)
}
\arguments{
  \item{start_time}{Vector of characters as read in from
  the r4ss report file}

  \item{end_time}{Vector of characters as read in from the
  r4ss report file}
}
\description{
Internal function used by \code{get_results_scenario} to
calculate the runtime (in minutes) from a \code{Report.sso}
file.
}
\author{
Cole Monnahan
}

