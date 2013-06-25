\name{get_results_all}
\alias{get_results_all}
\title{Extract SS3 simulation output}
\usage{
  get_results_all(directory = getwd(),
    files.overwrite = FALSE)
}
\arguments{
  \item{directory}{The directory which contains scenario
  folders with results.}

  \item{files.overwrite}{A switch to determine if existing
  files should be overwritten, useful for testing purposes
  or if new replicates are run.}
}
\description{
  This high level function extracts results from SS3 model
  runs. Give it a directory which contains directories for
  different "scenario" runs, within which are replicates
  and potentially bias adjustment runs. It writes two
  data.frames to file: one for single scalar values (e.g.
  MSY) and a second that contains output for each year of
  the same model (timeseries, e.g. biomass(year)). These
  can always be joined later.
}
\author{
  Cole Monnahan
}

