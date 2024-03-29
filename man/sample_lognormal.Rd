% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_lognormal.R
\name{sample_lognormal}
\alias{sample_lognormal}
\title{Sample observations using log-normal error corrected for bias}
\usage{
sample_lognormal(obs, sd)
}
\arguments{
\item{obs}{A vector of observed values you wish to sample with
log-normal error.}

\item{sd}{A vector of standard deviations to use in
\code{\link[stats:Normal]{stats::rnorm()}}.}
}
\description{
Sample a standard normal in log-space and apply the error to observations.
}
\details{
Newly sampled values are calculated
\eqn{obs*exp(stats::rnorm(1, 0, sd)-sd^2/2)}.
The second term adjusts the random samples so that their expected value is
\code{obs}, i.e., the log-normal bias correction.
}
\author{
Cole Monnahan
}
