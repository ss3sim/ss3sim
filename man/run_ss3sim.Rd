\name{run_ss3sim}
\alias{run_ss3sim}
\title{Run both the operating model and assessment model}
\usage{
  run_ss3sim(scenarios, iterations,
    index_params = "default", lcomp_params = "default",
    agecomp_params = "default",
    type = c("om and em", "om", "em"))
}
\arguments{
  \item{scenarios}{Which scenarios to run.}

  \item{iterations}{Which iterations to run.}

  \item{index_params}{A named list containing all the
  \code{jitter_index} options.}

  \item{lcomp_params}{A named list containing all the
  \code{change_lcomp} options.}

  \item{agecomp_params}{A named list containing all the
  \code{change_agecomp} options.}

  \item{type}{Are you running the both the operating and
  estimation models or just one or the other?}
}
\description{
  This function is a wrapper function that can call
  \code{\link{run_ss3model}} for the operating model,
  manipulate the output (add recruitment deviations, survey
  the data, etc.), and run the estimation model.
}

