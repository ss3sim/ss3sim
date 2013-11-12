\name{change_retro}
\alias{change_retro}
\title{Alter a starter file for a retrospective analysis}
\usage{
  change_retro(startfile_in = "starter.ss",
    startfile_out = "starter.ss", retro_yr = 0)
}
\arguments{
  \item{startfile_in}{Input \code{starter.ss} file}

  \item{startfile_out}{Output \code{starter.ss} file}

  \item{retro_yr}{Which retrospective year to enter into
  the starter file. Should be 0 (no retrospective analysis)
  or a negative value.}
}
\description{
  A retrospective analysis tests the effect of peeling back
  the number of operating model years observable to the
  estimation model. This function alters the SS starter
  file to run a retrospective analysis.
}
\details{
  Note that the starter file is set up to run a single
  retrospective run. Therefore, if you would like to run
  retrospective analyses for, say, 0, 1, 2, 3, 4, and 5
  years, you will need to use this function to adjust the
  starter file 6 separate times.
}
\examples{
# Locate the package data:
starterfile <- system.file("extdata", "models", "cod-om",
 "starter.ss", package = "ss3sim")

# No retrospective analysis:
change_retro(starterfile, "retro-0-starter.ss", retro_yr = 0)

# A retrospective analysis of 5 years:
change_retro(starterfile, "retro-5-starter.ss", retro_yr = -5)

# clean up:
unlink("retro-0-starter.ss")
unlink("retro-5-starter.ss")
}
\author{
  Sean C. Anderson
}

