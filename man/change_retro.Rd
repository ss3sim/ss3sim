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
  Alter a starter file for a retrospective analysis
}
\references{
  This function consists of three lines of code extracted
  from Ian Taylor's \code{r4ss::SS_doRetro} function by
  Sean Anderson.
}

