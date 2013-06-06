\name{change_e}
\alias{change_e}
\title{Alter em.ctl to change which parameters are being estimated in the estimation model}
\usage{
  change_e(ctl_file_in = "sardEM.ctl",
    ctl_file_out = "new.ctl", natM_type = NULL,
    natM_vector = NULL, natM_int = NULL,
    natM_lorenzen = NULL, natM_phase = -1,
    steep_phase = -1)
}
\arguments{
  \item{ctl_file_in}{The operating model control file to
  manipulate}

  \item{ctl_file_out}{Name of the resulting operating model
  control file}

  \item{natM_type}{A character value corresponding to
  option 1:4 in stock synthesis (i.e. "n_breakpoints",
  "Lorenzen", "agespecific" ,
  "agespec_withseasinterpolate")}

  \item{natM_vector}{A vector of ages at which you want
  breakpoints, only used if you specify natM_type =
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
         natM_type = "n_breakpoints", natM_vector = c(1,4),
         natM_int = c(.2,.3), natM_lorenzen = NULL,
         natM_phase = c(2,2), steep_phase = -1)
# clean up
file.remove("change_e.ctl")
}
}
\author{
  Kelli Johnson
}

