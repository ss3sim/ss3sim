\name{run_ss3sim}
\alias{run_ss3sim}
\title{Master function to run SS3 simulations}
\usage{
  run_ss3sim(iterations, scenarios, case_folder,
    om_model_dir, em_model_dir,
    case_files = list(M = "M", F = "F", D = c("index", "lcomp", "agecomp"), R = "R", E = "E"),
    parallel = FALSE, ...)
}
\arguments{
  \item{iterations}{Which iterations to run. A numeric
  vector. For example \code{1:100}.}

  \item{scenarios}{Which scenarios to run. A vector of
  character objects. For example
  \code{c("D0-E0-F0-R0-M0-cod", "D1-E0-F0-R0-M0-cod")}.
  Also, see \code{\link{expand_scenarios}} for a shortcut
  to specifying the scenarios.  See
  \code{\link{get_caseargs}} and the vignette for details
  on specifying the scenarios.}

  \item{case_folder}{The folder containing the plain-text
  case files.}

  \item{om_model_dir}{The folder containing the SS3
  operating model configuration files.}

  \item{em_model_dir}{The folder containing the SS3
  estimation model configuration files.}

  \item{case_files}{A named list that relates the case IDs
  to the files to return. If you are passing time-varying
  parameters beyond (or instead of) natural mortality (M),
  then you will need to adjust these values to reflect your
  scenarios. This argument is passed to
  \code{\link{get_caseargs}}. See that function for details
  and examples of how to specify this.}

  \item{...}{Anything else to pass to
  \code{\link{ss3sim_base}}. This could include
  \code{bias_adjust} and \code{bias_nsim}. Also, you can
  pass additional options to the \code{SS3} command through
  the argument \code{admb_options}.}

  \item{parallel}{A logical argument that controls whether
  the scenarios are run in parallel. You will need to
  register multiple cores first with a package such as
  \pkg{doParallel} and have the \pkg{foreach} package
  installed. See the example below.}
}
\value{
  The output will appear in whatever your current \R
  working directory is. There will be folders named after
  your scenarios. They will look like this: \itemize{ \item
  \code{D0-E0-F0-M0-R0-cod/bias/1/om} \item
  \code{D0-E0-F0-M0-R0-cod/bias/1/em} \item
  \code{D0-E0-F0-M0-R0-cod/bias/2/om} \item ... \item
  \code{D0-E0-F0-M0-R0-cod/1/om} \item
  \code{D0-E0-F0-M0-R0-cod/1/em} \item
  \code{D0-E0-F0-M0-R0-cod/2/om} \item ... }
}
\description{
  This is the main high-level wrapper function for running
  ss3sim simulations. This function first deals with
  parsing a scenario ID into case input files and then
  passes these arguments on to \code{\link{ss3sim_base}} to
  run a simulation. Alternatively, you might choose to run
  \code{\link{ss3sim_base}} directly and skip the case-file
  setup.
}
\details{
  The operating model folder should contain:
  \code{forecast.ss}, \code{yourmodel.ctl},
  \code{yourmodel.dat}, \code{ss3.par}, and
  \code{starter.ss}. The files should be the versions that
  are returned from an SS run as \code{.ss_new} files. This
  is important because it creates consistent formatting
  which many of the functions in this package depend on.
  Rename the \code{.ss_new} files as listed above (and in
  all lowercase). The estimation model folder should
  contain all the same files listed above except the
  \code{ss3.par} and \code{yourmodel.dat} files, which are
  unnecessary but can be included if desired. See the
  vignette for details on modifying an existing \code{SS3}
  model to run with \pkg{ss3sim}. Alternatively, you might
  consider modifying one of the built in model
  configurations.
}
\examples{
\dontrun{
# Without bias adjustment:
case_folder <- system.file("extdata", "eg-cases", package =
  "ss3sim")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")

run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up

# An example specifying the case files:
run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-R0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  case_files = list(F = "F", D = c("index", "lcomp",
      "agecomp"), R = "R", E = "E"))

# With bias adjustment:
# (Note that bias_nsim should be bigger, say 5 or 10, but it is set
# to 2 here so the example runs faster.)
run_ss3sim(iterations = 1:1, scenarios = "D1-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, bias_nsim = 2)

# Restarting the previous run using the existing bias-adjustment
# output
run_ss3sim(iterations = 2:3, scenarios = "D1-E0-F0-R0-M0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = FALSE, bias_already_run = TRUE)
unlink("D1-E0-F0-R0-M0-cod", recursive = TRUE) # clean up

# A run with deterministic process error for model checking:
recdevs_det <- matrix(0, nrow = 100, ncol = 20)
run_ss3sim(iterations = 1:20, scenarios = "D0-E100-F0-R0-M0-cod",
  case_folder = case_folder, om_model_dir = om, em_model_dir = em,
  bias_adjust = TRUE, bias_nsim = 2, user_recdevs = recdevs_det)
unlink("D0-E100-F0-R0-M0-cod", recursive = TRUE) # clean up

# An example of a run using parallel processing across 2 cores:
require(doParallel)
registerDoParallel(cores = 2)
require(foreach)
getDoParWorkers() # check how many cores are registered
run_ss3sim(iterations = 1, scenarios = c("D0-E0-F0-R0-M0-cod",
    "D1-E0-F0-R0-M0-cod"), case_folder = case_folder,
  om_model_dir = om, em_model_dir = em, parallel = TRUE)
unlink("D0-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
unlink("D1-E0-F0-R0-M0-cod", recursive = TRUE) # clean up
}
}
\author{
  Sean C. Anderson
}
\seealso{
  \code{\link{ss3sim_base}}, \code{\link{run_ss3model}},
  \code{\link{run_bias_ss3}}, \code{\link{get_caseargs}}
}

