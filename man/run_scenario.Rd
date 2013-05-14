\name{run_scenario}
\alias{run_scenario}
\title{Run both the operating model and assessment model}
\usage{
  run_scenario(scenarios, iterations,
    type = c("om and em", "om", "em"))
}
\arguments{
  \item{scenarios}{Which scenarios to run.}

  \item{iterations}{Which iterations to run.}

  \item{type}{Are you running the both the operating and
  estimation models or just one or the other?}
}
\description{
  This function is a wrapper function that can call
  \code{run_model} for the operating model, manipulate the
  output (add recruitment deviations, survey the data,
  etc.), and run the estimation model.
}
\author{
  Sean Anderson
}

