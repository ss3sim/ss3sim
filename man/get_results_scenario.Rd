\name{get_results_scenario}
\alias{get_results_scenario}
\title{Extract SS3 simulation results for one scenario.}
\usage{
  get_results_scenario(scenario, directory = getwd(),
    overwrite.files = FALSE)
}
\arguments{
  \item{scenario}{A single character giving the scenario
  from which to extract results.}

  \item{directory}{The directory which contains the
  scenario folder.}

  \item{overwrite.files}{A boolean (default is FALSE) for
  whether to delete any files previously created with this
  function. This is intended to be used if replicates were
  added since the last time it was called, or any changes
  were made to this function.}
}
\description{
  Function that extracts results from all replicates inside
  a supplied scenario folder. The function writes 3 .csv
  files to the scenario folder: (1) scalar metrics with one
  value per replicate (e.g. $R_0$, $h$), (2) a timeseries
  data ('ts') which contains multiple values per replicate
  (e.g. $SSB_y$ for a range of years $y$), and (3)
  residuals on the log scale from the surveys across all
  replicates (this feature is not fully tested!). The
  function \code{get_results_all} loops through these .csv
  files and combines them together into a single "final"
  dataframe.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
run_ss3sim(iterations = 1:30, scenarios =
           c("D1-E0-F0-G0-R0-S0-M0-cod"),
           case_folder = case_folder, om_model_dir = om, em_model_dir = em,
           bias_adjust = TRUE)
get_results_scenario(c("D1-E0-F0-G0-R0-S0-M0-cod"))
}
}
\author{
  Cole Monnahan
}
\seealso{
  Other get.results: \code{\link{get_results_all}},
  \code{\link{get_results_scalar}},
  \code{\link{get_results_timeseries}}
}

