\name{run_bias_ss3}
\alias{run_bias_ss3}
\title{Determine level of bias adjustment for SS runs}
\usage{
  run_bias_ss3(dir, outdir, nsim, conv_crit = 0.2)
}
\arguments{
  \item{dir}{Folder for all of the bias adjustment runs
  (e.g. \code{"M1-F1-D1-R1-cod/bias"} which must contain
  numbered folders for the \code{nsim} runs, e.g.
  \code{"M1-F1-D1-R1-cod/bias/1/"},
  \code{"M1-F1-D1-R1-cod/bias/2/"}, ...,
  \code{"M1-F1-D1-R1-cod/bias/10/"} if there are \code{nsim
  = 10} bias adjustment runs)}

  \item{outdir}{Folder containing the run folders for a
  given scenario (e.g. \code{"M1-F1-D1-R1-cod"} that
  contains \code{"M1-F1-D1-R1-cod/1/"}
  \code{"M1-F1-D1-R1-cod/2/"}, etc.)}

  \item{nsim}{number of bias adjustment runs conducted for
  a particular scenario (e.g. \code{10})}

  \item{conv_crit}{The maximum percentage of bias
  iterations that can produce a non-invertible Hessian
  before a warning will be produced. If this percentage is
  exceeded then a file \code{WARNINGS.txt} will be
  produced. Currently, the simulations will continue to
  run.}
}
\description{
  Determine level of bias adjustment from multiple SS runs.
  IMPORTANT: The Hessian must be calculated for the SS runs
  that this function uses.
}
\details{
  This function: \itemize{ \item uses the \code{r4ss}
  package to read in output from n SS runs, \item uses Ian
  Taylor's \code{r4ss} function to find values for the n
  bias adjustment parameters for each run, \item takes the
  average over runs for each bias adjustment parameter
  \item writes out the unaveraged and averaged
  (\code{AdjustBias.DAT} and \code{AvgBias.DAT},
  respectively) bias adjustment parameters to the
  \code{dir} folder \item takes a \code{control.ss_new}
  file from one of the n SS runs, changes the n bias
  adjustment parameters, and writes the whole updated
  \code{control.ss_new} file with new bias adjustment
  parameters to an \code{em.ctl} file }
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
run_ss3sim(iterations = 1:1, scenarios = "D1-E0-F0-G0-R0-S0-M0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, bias_nsim = 2)
# (Note that bias_nsim should be bigger, say 10, but it is set to 2
# here so the example runs faster.)
}
}
\author{
  Carey McGilliard
}
\references{
  Methot, R. D. and Taylor, I. G. (2011). Adjusting for
  bias due to variability of estimated recruitments in
  fishery assessment models. Can. J. Fish. Aquat. Sci.,
  68(10):1744–1760.
}
\seealso{
  \code{link{run_ss3sim}}, \code{\link{ss3sim_base}},
  \code{\link{run_ss3model}}, \code{\link{bias_ss3}}
}

