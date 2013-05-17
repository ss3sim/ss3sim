\name{run_bias_ss3}
\alias{run_bias_ss3}
\title{Determine level of bias adjustment for SS runs}
\usage{
  run_bias_ss3(dir, outdir, nsim, iter)
}
\arguments{
  \item{dir}{Folder for all of the bias adjustment runs
  (e.g. \code{"M1-F1-D1-R1-cod/bias"} which must contain
  numbered folders for the \code{nsim} runs, e.g.
  \code{"M1-F1-D1-R1-cod/bias/1/"},
  \code{"M1-F1-D1-R1-cod/bias/2/"}, ...,
  \code{"M1-F1-D1-R1-cod/bias/5/"} if there are \code{nsim
  = 5} bias adjustment runs)}

  \item{outdir}{Folder containing the run folders for a
  given scenario (e.g. \code{"M1-F1-D1-R1-cod"} that
  contains \code{"M1-F1-D1-R1-cod/1/"}
  \code{"M1-F1-D1-R1-cod/2/"}, etc.)}

  \item{nsim}{number of bias adjustment runs conducted for
  a particular scenario (e.g. \code{5})}

  \item{iter}{number of runs conducted for a scenario after
  bias adjustment is complete (e.g. \code{1:100})}
}
\description{
  Determine level of bias adjustment from multiple SS runs.
  IMPORTANT: The Hessian must be calculated for the SS the
  runs that this function uses.
}
\details{
  This function: \itemize{ \item uses the \code{r4ss}
  package to read in output from 5 SS runs, \item uses Ian
  Taylor's \code{r4ss} function to find values for the 5
  bias adjustment parameters for each run, \item takes the
  average over runs for each bias adjustment parameter
  \item writes out the unaveraged and averaged
  (\code{CorrectBias.dat} and \code{AvgBias.dat},
  respectively) bias adjustment parameters to the
  \code{dir} folder \item takes a \code{control.ss_new}
  file from one of the 5 SS runs, changes the 5 bias
  adjustment parameters, and writes the whole updated
  \code{control.ss_new} file with new bias adjustment
  parameters to an \code{em.ctl} file } The new
  \code{em.ctl} file (that now contains the updated bias
  adjustment parameters) can then be copied to the folders
  for each run of the scenario \code{run_bias_ss3} needs to
  be run for each scenario, but results can be used for all
  runs of that scenario
}
\examples{
\dontrun{
# Main Function call example:
# Note that usually nsim will be 5, not 2,
# nsim = 2 is just faster for an example
run_bias_ss3(dir = "M1-F1-D1-R1-cod/", outdir = "M1-F1-D1-R1-cod/",
nsim = 2,iter = 3)
}
}
\author{
  Carey McGilliard
}

