% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sample_dm.R
\name{sample_dm}
\alias{sample_dm}
\title{Sample with a Dirichlet-Multinomial distribution}
\usage{
sample_dm(data, n, par)
}
\arguments{
\item{data}{A data frame with one row.}

\item{n}{The desired sample size.}

\item{par}{The cpar value to define overdispersion.}
}
\value{
A data frame with one row because right now the input
data should only be a single row of data.
}
\description{
Sample with a Dirichlet-Multinomial distribution
}
