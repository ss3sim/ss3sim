\name{ss3sim_base}
\alias{ss3sim_base}
\title{Run both the operating model and assessment model}
\usage{
  ss3sim_base(iterations, scenarios, f_params,
    index_params, lcomp_params, agecomp_params,
    estim_params, tv_params, om_model_dir, em_model_dir,
    retro_params = NULL, user_recdevs = NULL,
    bias_adjust = FALSE, bias_nsim = 5,
    bias_already_run = FALSE, hess_always = FALSE,
    print_logfile = TRUE, sleep = 0, seed = NULL,
    conv_crit = 0.2, ...)
}
\arguments{
  \item{iterations}{Which iterations to run. A numeric
  vector.}

  \item{scenarios}{Which scenarios to run.}

  \item{tv_params}{A named list containing all the
  \code{\link{change_tv}} (time-varying) options.}

  \item{f_params}{A named list containing all the
  \code{\link{change_f}} options.}

  \item{index_params}{A named list containing all the
  \code{\link{change_index}} options.}

  \item{lcomp_params}{A named list containing all the
  \code{\link{change_lcomp}} options.}

  \item{agecomp_params}{A named list containing all the
  \code{\link{change_agecomp}} options.}

  \item{retro_params}{A named list containing all the
  \code{\link{change_retro}} options.}

  \item{estim_params}{A named list containing all the
  \code{\link{change_e}} options.}

  \item{om_model_dir}{The directory with the operating
  model you want to copy and use for the specified
  simulations.}

  \item{em_model_dir}{The directory with the estimation
  model you want to copy and use for the specified
  simulations.}

  \item{user_recdevs}{An optional 100x100 matrix of
  recruitment deviations to replace the recruitment
  deviations built into the package
  (\code{\link{recdevs}}). The columns represent run
  iterations and the rows represent years. Note that these
  will be multiplied by sigma R. So, under normal
  conditions this matrix should contain standard-normal
  values \code{~N(0, 1)}.}

  \item{bias_adjust}{Run bias adjustment first? See
  \code{\link{run_bias_ss3}}.}

  \item{bias_nsim}{If bias adjustment is run, how many
  simulations should the bias be estimated from? It will
  take the mean of the adjustment factors across these
  runs.}

  \item{bias_already_run}{If you've already run the bias
  runs for a scenario (the bias folders and \code{.dat}
  files already exist) then you can set this to \code{TRUE}
  to avoid re-running the bias adjustment routine.}

  \item{hess_always}{If \code{TRUE} then the Hessian will
  always be calculated. If \code{FALSE} then the Hessian
  will only be calculated for bias-adjustment runs thereby
  saving time.}

  \item{print_logfile}{Logical. Print a log file?}

  \item{sleep}{A time interval (in seconds) to pause on
  each iteration. Useful if you want to reduce average CPU
  time -- perhaps because you're working on a shared
  server.}

  \item{seed}{If set to a numeric vector then
  \code{set.seed} will be set to each successive value of
  the vector \code{seed} on each iteration. This can be
  useful to make simulations reproducible. If left set to
  \code{NULL} then the seed will not be set.}

  \item{conv_crit}{The maximum percentage of bias
  iterations that can produce a non-invertible Hessian
  before a warning will be produced. If this percentage is
  exceeded then a file \code{WARNINGS.txt} will be
  produced. Currently, the simulations will continue to
  run.}

  \item{...}{Anything extra to pass to
  \code{\link{run_ss3model}}. For example, you may want to
  pass additional options to \code{SS3} through the
  argument \code{admb_options}. Anything that doesn't match
  a named argument in \code{\link{run_ss3model}} will be
  passed to the \code{\link{system}} call that runs
  \code{SS3}. If you are on a Windows computer then you
  might want to pass \code{show.output.on.console = FALSE}
  to make the simulations runs faster.}
}
\description{
  This function is a wrapper function that can call
  \code{\link{run_ss3model}} for the operating model,
  sample the output (add recruitment deviations, survey the
  data, etc.), and run the estimation model.
  \code{ss3sim_base} is the main internal function for
  ss3sim.
}
\details{
  This function is written to be flexible. You can specify
  the natural mortality, fishing mortality, survey index,
  length comp, and age comp parameters in the function call
  as list objects. For a generic higher-level function, see
  \code{\link{run_ss3sim}}.
}
\examples{
\dontrun{
# Pull in file paths from the package example data:
d <- system.file("extdata", package = "ss3sim")
om_model_dir <- paste0(d, "/models/cod-om")
em_model_dir <- paste0(d, "/models/cod-em")
a <- get_caseargs(folder = paste0(d, "/eg-cases"), scenario =
"M0-F0-D0-R0-E0-cod")

ss3sim_base(iterations = 1, scenarios = "M0-F0-D0-R0-E0-cod",
f_params = a$F, index_params = a$index, lcomp_params = a$lcomp,
agecomp_params = a$agecomp, tv_params = a$tv_params, retro_params =
a$R, estim_params = a$E, om_model_dir = om_model_dir, em_model_dir
= em_model_dir)
}
}
\author{
  Sean C. Anderson
}
\seealso{
  \code{\link{run_ss3sim}}
}

