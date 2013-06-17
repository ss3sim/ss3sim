\name{change_e}
\alias{change_e}
\title{Alter em.ctl to change which parameters are being estimated in the
estimation model}
\usage{
  change_e(ctl_file_in = "sardEM.ctl",
    ctl_file_out = "new.ctl", natM_type = NULL,
    natM_n_breakpoints = NULL, natM_int = NULL,
    natM_lorenzen = NULL, natM_phase = -1,
    steep_phase = -1, qSurvey_phase = NULL,
    qCPUE_phase = -1, CV_young_phase = NULL,
    CV_old_phase = NULL)
}
\arguments{
  \item{ctl_file_in}{The operating model control file to
  manipulate}

  \item{ctl_file_out}{Name of the resulting operating model
  control file}

  \item{natM_type}{A character value corresponding to
  option 1:4 in stock synthesis (i.e. "n_breakpoints",
  "Lorenzen", "agespecific" ,
  "agespec_withseasinterpolate"), default is NULL which
  will leave the parameter as specified in the .ctl file
  (ie a single fixed parameter)}

  \item{natM_n_breakpoints}{A vector of ages at which you
  want breakpoints, only used if you specify natM_type =
  "n_breakpoints".}

  \item{natM_int}{A vector of natural mortality rates for
  each natM parameter}

  \item{natM_lorenzen}{The reference age for the Lorenzen
  function should natM_type = "Lorenzen".}

  \item{natM_phase}{A vector of phases for each natM
  parameter. Leave at default if natM_type = "agespecific"
  or "agespec_withseasinterpolate".}

  \item{steep_phase}{A single value that specifies the
  phase in which steepness is estimated.}

  \item{qSurvey_phase}{A single value that specifies the
  phase in which catchability for the survey is estimated}

  \item{qCPUE_phase}{A single value that specifies the
  phase in which catchability for CPUE is estimated}

  \item{CV_young_phase}{A single parameter that specifies
  the phases in which to estimate the CV for size at age
  for young fish.}

  \item{CV_old_phase}{A single parameter that specifies the
  phases in which to estimate the CV for size at age for
  old fish.}
}
\description{
  Alter em.ctl to change which parameters are being
  estimated in the estimation model
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
ctl_file <- paste0(d, "/run_ss3sim_eg/cod_em/simple_cod_em.ctl")
change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
         natM_type = "n_breakpoints", natM_n_breakpoints = c(1,4),
         natM_int = c(.2,.3), natM_lorenzen = NULL,
         natM_phase = c(2,2), steep_phase = -1, qSurvey_phase = NULL, qCPUE_phase = -1,
         CV_young_phase = NULL, CV_old_phase = NULL )
# clean up
file.remove("change_e.ctl")
}
}
\author{
  Kelli Johnson
}

