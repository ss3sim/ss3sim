\name{run_ss3sim}
\alias{run_ss3sim}
\title{Run both the operating model and assessment model}
\usage{
  run_ss3sim(iterations, scenarios, m_params, sel_params,
    growth_params, f_params, index_params, lcomp_params,
    agecomp_params, estim_params, retro_params,
    om_model_dir, em_model_dir, user_recdevs = NULL,
    bias_adjust = FALSE, bias_nsim = 5,
    bias_already_run = FALSE, hess_always = FALSE,
    print_logfile = TRUE, sleep = 0, seed = NULL, ...)
}
\arguments{
  \item{iterations}{Which iterations to run. A numeric
  vector.}

  \item{scenarios}{Which scenarios to run.}

  \item{m_params}{A named list containing all the
  \code{\link{change_m}} options.}

  \item{sel_params}{A named list containing all the
  \code{\link{change_sel}} options.}

  \item{growth_params}{A named list containing all the
  \code{\link{change_growth}} options.}

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
  deviations built into the package. The columns represent
  run iterations and the rows represent years. Note that
  these will be multiplied by sigma R. So, under normal
  conditions this matrix should contain standard-normal
  values ~N(0, 1).}

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

  \item{...}{Anything extra to pass to
  \code{\link{run_ss3model}}. For example, you may want to
  pass additional options to \code{SS3} through the
  argument \code{admb_options}. Just don't pass
  \code{-noest} or \code{-nohess} and enable bias
  correction.}
}
\description{
  This function is a wrapper function that can call
  \code{\link{run_ss3model}} for the operating model,
  manipulate the output (add recruitment deviations, survey
  the data, etc.), and run the estimation model.
}
\details{
  This function is written to be flexible. You can specify
  the natural mortality, fishing mortality, survey index,
  length comp, and age comp parameters in the function call
  as list objects. For a higher-level wrapper function
  specific to the setup of the Fish600 projects, see
  \code{\link{run_fish600}}.
}
\examples{
\dontrun{
# Pull in file paths from the package example data:
d <- system.file("extdata", package = "ss3sim")
f <- paste0(d, "/run_ss3sim_eg/")
om_model_dir <- paste0(f, "cod_om")
em_model_dir <- paste0(f, "cod_em")
a <- get_caseargs(folder = paste0(f, "case-arguments"), scenario =
"M1-F1-D1-R1-S1-G1-E1-cod")

run_ss3sim(iterations = 1, scenarios = "M1-F1-D1-R1-S1-G1-E1-cod",
  m_params = a$M, sel_params = a$S, growth_params = a$G, f_params =
  a$F, index_params = a$index, lcomp_params = a$lcomp, agecomp_params
  = a$agecomp, retro_params = a$R, estim_params = a$E,
  om_model_dir = om_model_dir, em_model_dir = em_model_dir)
}
}
\author{
  Sean C. Anderson
}
\seealso{
  \code{\link{run_fish600}}
}

