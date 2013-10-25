\name{change_tv}
\alias{change_tv}
\title{Methods to include time-varying parameters in a SS3 OM}
\usage{
  change_tv(change_tv_list, ctl_file_in = "control.ss_new",
    ctl_file_out = "om.ctl", dat_file_in = "ss3.dat",
    dat_file_out = "ss3.dat", par_file_in = "ss3.par",
    par_file_out = "ss3.par",
    starter_file_in = "starter.ss",
    starter_file_out = "starter.ss",
    report_file = "Report.sso")
}
\arguments{
  \item{change_tv_list}{A list of named vectors. Names
  correspond to parameters in the operating model that
  currently do not utilize environmental deviations.
  Parameters will change to vary with time according to the
  vectors of deviations. Vectors of deviations, also
  referred to as environmental data, must have a length
  equal to \code{ endyr-startyr+1}, where \code{endyr} and
  \code{startyr} are specified the \code{.dat} file.
  Specify years without deviations as zero. Parameter names
  must be unique and match the full parameter name in the
  \code{.ctl} file. Names for stock recruit parameters must
  contain "devs", "R0", or "steep", and only one stock
  recruit parameter can be time-varying per model. The
  feature will include an *additive* functional linkage
  between environmental data and the parameter where the
  link parameter is fixed at a value of one and the par
  value is specified in the \code{.par} file: \eqn{par'[y]
  = par + link * env[y]}. For catchability (\eqn{q}) the
  *additive* functional linkage is implemented on the log
  scale: \eqn{ln(q'[y]) = ln(q) + link * env[y]}}

  \item{ctl_file_in}{Input SS3 control file}

  \item{ctl_file_out}{Output SS3 control file}

  \item{dat_file_in}{Input SS3 data file}

  \item{dat_file_out}{Output SS3 data file}

  \item{par_file_in}{Input SS3 parameter file}

  \item{par_file_out}{Output SS3 parameter file}

  \item{starter_file_in}{Input SS3 starter file}

  \item{starter_file_out}{Output SS3 starter file}

  \item{report_file}{Input SS3 report file}
}
\description{
  Takes SS3 \code{.ctl}, \code{.par}, and \code{.dat} files
  and implements time-varying parameters using
  environmental variables. Specifically set up to work with
  an operating model \code{.ctl} file.
}
\details{
  To pass arguments to \code{change_tv} through
  \code{\link{run_ss3sim}}: (1) create a case file with an
  arbitrary letter not used elsewhere (anything but D, E,
  F, or R) and (2) include the line \code{function_type;
  change_tv} in your case file. For example, you might want
  to use M for natural mortality, S for selectivity, or G
  for growth.

  Although there are three ways to implement time-varying
  parameters within SS3, \code{ss3sim} and \code{change_tv}
  only use the environmental variable option. Within SS3,
  time-varying parameters work on an annual time-step.
  Thus, for models with multiple seasons, the time-varying
  parameters will remain constant for the entire year.

  The \code{ctl_file_in} argument needs to be a
  \code{.ss_new} file because the documentation in
  \code{.ss_new} files are automated and standardized. This
  function takes advantage of the standard documentation
  the \code{.ss_new} files to determine which lines to
  manipulate and where to add code in the \code{.ctl},
  \code{.par}, and \code{.dat} files, code that is
  necessary to implement time-varying parameters.

  \code{ss3sim} uses annual recruitment deviations and may
  not work with a model that ties recruitment deviations to
  environmental covariates. If you need to compare the
  environment to annual recruitment deviations, the
  preferred option is to transform the environmental
  variable into an age 0 pre-recruit survey. See page 55 of
  the SS3 version 3.24f manual for more information.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
simple <- paste0(d, "/Simple")
wd <- getwd()
dir.create("Simple")
file.copy(simple, ".", recursive = TRUE)
setwd("Simple")

# Run SS3 to create control.ss_new and Report.sso:
system("SS3 starter.ss -noest")

change_tv(change_tv_list = list("NatM_p_1_Fem_GP_1" = c(rep(0, 20),
      rep(.1, 11)), "SR_BH_steep"=rnorm(31, 0, 0.05)), ctl_file_in =
  "control.ss_new", ctl_file_out = "example.ctl", dat_file_in =
  "simple.dat", dat_file_out = "example.dat")

# clean up:
setwd("../")
unlink("Simple")
}
}
\author{
  Kotaro Ono, Carey McGilliard, and Kelli Johnson
}

