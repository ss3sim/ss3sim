\name{add_time_varying_features}
\alias{add_time_varying_features}
\title{Methods to include time varying param features}
\usage{
  add_time_varying_features(Nblocks, Blockpattern, Dev,
    ctl.file.in = "control.ss_new",
    ctl.file.out = "YTF.ctl", Dat.file = "YTF.dat",
    par.file = "ss3.par",
    how.time.varying = c("env", "block", "dev"))
}
\arguments{
  \item{Nblocks}{Number of blocks}

  \item{Blockpattern}{Block pattern}

  \item{Deviations}{}

  \item{ctl.file.in}{Input control file name}

  \item{ctl.file.out}{Output control file name}

  \item{Dat.file}{.dat file name}

  \item{par.file}{.par file name}

  \item{how.time.varying}{How time varying parameters are
  added. Options are "env" for environment, "block" for
  block, or "dev" for deviations.}
}
\description{
  Methods to include time varying param features
}

