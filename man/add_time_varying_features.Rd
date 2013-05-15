\name{add_time_varying_features}
\alias{add_time_varying_features}
\title{Methods to include time varying natural mortality features}
\usage{
  add_time_varying_features(n_blocks, block_pattern, dev,
    ctl_file_in = "control.ss_new",
    ctl_file_out = "YTF.ctl", dat_file = "YTF.dat",
    par_file = "ss3.par",
    how_time_varying = c("env", "block", "dev"))
}
\arguments{
  \item{n_blocks}{Number of blocks}

  \item{block_pattern}{Block pattern. A vector of years
  when changes occur.}

  \item{dev}{A vector of deviations}

  \item{ctl_file_in}{Input control file name}

  \item{ctl_file_out}{Output control file name}

  \item{dat_file}{.dat file name}

  \item{par_file}{.par file name}

  \item{how_time_varying}{How time varying parameters are
  added. Options are \code{"env"} for environment,
  \code{"block"} for block, or \code{"dev"} for
  deviations.}
}
\description{
  Methods to include time varying natural mortality
  features
}

