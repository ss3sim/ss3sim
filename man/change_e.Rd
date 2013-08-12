\name{change_e}
\alias{change_e}
\title{Alter em.ctl to change which parameters are being estimated in the
estimation model}
\usage{
  change_e(ctl_file_in = pastef("em.ctl"),
    ctl_file_out = pastef("em.ctl"), natM_type = "1Parm",
    natM_n_breakpoints = NULL, natM_lorenzen = NULL,
    natM_val = c(NA, NA), par_name = NULL, par_int = "NA",
    par_phase = "NA")
}
\arguments{
  \item{ctl_file_in}{The operating model control file to
  manipulate}

  \item{ctl_file_out}{Name of the resulting operating model
  control file}

  \item{natM_type}{A character value corresponding to
  option 0:4 in stock synthesis (i.e. "1Parm",
  "n_breakpoints", "Lorenzen", "agespecific",
  "agespec_withseasinterpolate"), NULL will leave natM as
  specified in em.ctl}

  \item{natM_n_breakpoints}{A vector of ages at which you
  want breakpoints, only used if you specify natM_type =
  "n_breakpoints".}

  \item{natM_lorenzen}{The reference age for the Lorenzen
  function should natM_type = "Lorenzen".}

  \item{natM_val}{Vector of int and phase values for each
  natM parameter.}

  \item{par_name}{Vector of values, separated by commas.
  The values will later be turned into character values and
  used to search for specific lines for each parameter in
  the ctl file}

  \item{par_int}{Vector of intial values, one for each
  parameter in par_name, value can be NA if you do not wish
  to change the int.}

  \item{par_phase}{Vector of phase values, one for each
  parameter in par_name, value can be NA if you do not wish
  to change the phase.}
}
\description{
  Alter em.ctl to change which parameters are being
  estimated in the estimation model
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
ctl_file <- paste0(d, "/ss3sim_base_eg/cod_em/simple_cod_em.ctl")
change_e(ctl_file_in = ctl_file, ctl_file_out = "change_e.ctl",
         natM_type = "n_breakpoints", natM_n_breakpoints = c(1,4),
         natM_lorenzen = NULL, natM_val = c(.2,3,.4,5),
         par_name = c("steep", "SizeSel_1P_1_Fishery"),
         par_int = c(.3, 40), par_phase = c(3,2)  )
# clean up
file.remove("change_e.ctl")
}
}
\author{
  Kelli Johnson
}

