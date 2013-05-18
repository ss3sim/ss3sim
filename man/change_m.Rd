\name{change_m}
\alias{change_m}
\title{Methods to include time varying natural mortality features}
\usage{
  change_m(how_time_varying = "env",
    ctl_file_in = "control.ss_new",
    ctl_file_out = "Simple.ctl", dat_file = "Simple.dat",
    par_file = "ss3.par", n_blocks = 1, block_pattern, dev)
}
\arguments{
  \item{how_time_varying}{The method for including
  time-varying M in the SS input files the 3 options are:
  \code{"env"} \code{"block"} or \code{"dev"}. \itemize{
  \item "env" is to include an ADDITIVE functional linkage
  between environmental data and M where a link parameter
  is estimated: \code{M'(y) = M + link * env(y)} \item
  \code{"block"} is to allow M to be estimated separately
  in different pre-specified time blocks \item \code{"dev"}
  is to estimate a mean M and to estimate deviations about
  that mean }}

  \item{ctl_file_in}{Input control file name (before M
  estimation method is chosen)}

  \item{ctl_file_out}{Output control file name (resulting
  control file with M changed)}

  \item{dat_file}{\code{.dat} File name}

  \item{par_file}{\code{.par} File name}

  \item{n_blocks:}{The number of time blocks within which M
  is estimated separately; Equal to 1 unless
  \code{how_time_varying} is \code{"block"}, but #' is not
  used in the function unless \code{how_time_varying} is
  \code{"block"}}

  \item{block_pattern}{Block pattern. A vector of years
  marking time blocks with separate Ms in each block -
  doesn't get used in the function if
  \code{how_time_varying} does not equal \code{"block"}}

  \item{dev:}{A vector of environmental data of length =
  length of *.dat endyr-startyr+1}
}
\description{
  This function takes generic OM files for sardine,
  flatfish, and cod and inserts time-varying M.
}
\details{
  Although it appears that 3 options exist for how
  time-varying M is modeled within SS and within this
  function, right now only the how_time_varying = "env"
  (making m vary over time by including an ADDITIVE
  environmental linkage to M) has been tested. The
  \code{ctl_file_in} parameter needs to be a \code{.ss_new}
  file because the documentation in \code{.ss_new} files
  are automated and standardized; this function takes
  advantage of standard documentation used to figure out
  where additional lines need to be added to \code{.ctl}
  files to implement time-varying M.

  CAREFUL: currently the function will modify the
  \code{.dat} file (\code{dat_file}) that is input; make a
  copy of the \code{.dat} file under another name if you
  want to keep an unmodified copy

  NOTE: the user has to define an environmental data series
  such that the additive linkage creates the desired
  time-varying pattern in M.
}
\examples{
\dontrun{
# setwd()... simple SS3 example
change_m(how_time_varying="env",ctl_file_in = "control.ss_new",dat_file =
"simple.dat",par_file = "SS3.par",n_blocks = 1,block_pattern =
c(1990,2001),dev = rnorm(31,mean = 0,sd = 0.6))

# Other examples
# setwd() sardine model
change_m(how_time_varying="env",ctl_file_in = "control.ss_new",dat_file =
"SardOM.dat",par_file = "SS3.par",n_blocks = 1,block_pattern =
c(1990,2001),dev = rnorm(100,mean = 0,sd = 0.6))

setwd() flatfish model
change_m(how_time_varying="env",ctl_file_in = "control.ss_new",dat_file =
"flatfish.dat",par_file = "SS3.par",n_blocks = 2,block_pattern =
c(1990,2001),dev = rnorm(100,mean = 0,sd = 0.6))
}
}
\author{
  Kotaro Ono and Carey McGilliard
}

