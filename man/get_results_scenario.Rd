\name{get_results_scenario}
\alias{get_results_scenario}
\title{Extract SS3 simulation results for one scenario.}
\usage{
  get_results_scenario(scenario, directory = getwd(),
    overwrite.files = FALSE)
}
\arguments{
  \item{scenario}{A folder name in the directory folder
  which contains replicates and potentially bias adjustment
  runs.}

  \item{directory}{A path to folder containing the scenario
  folder.}

  \item{overwrite.files}{A switch to determine if existing
  files should be overwritten, useful for testing purposes
  or if new replicates are run.}
}
\description{
  Take a path to a scenario folder with results and write
  the individual scenario results to two data.frames in
  that folder. This function is called by
  \code{\link{get_results_all}} or can be used individually
  for testing.
}
\author{
  Cole Monnahan
}
\seealso{
  Other get.results: \code{\link{get_results_all}},
  \code{\link{get_results_scalar}},
  \code{\link{get_results_timeseries}}
}

