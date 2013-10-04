\name{change_index}
\alias{change_index}
\title{Sample the biomass with observation error to generate indices of abundance.}
\usage{
  change_index(infile, outfile, fleets, years, sds_obs,
    make_plot = FALSE, write_file = TRUE)
}
\arguments{
  \item{infile}{SS data object as read in from
  \code{SS_readdat} in the r4ss package. Make sure you
  select option \code{section=2}.}

  \item{outfile}{Character string of the name for the new
  file to be created. Must end in \code{.dat}.}

  \item{fleets}{Numeric vector giving the fleets to be
  used. This order also pertains to other arguments. A
  value of \code{NA} or missing value excludes that fleet
  from outfile (i.e. turn it off so no samples are
  written).}

  \item{years}{A Numeric list the same length as
  \code{fleets} giving the years.}

  \item{sds_obs}{A Numeric list of the same length as
  \code{fleets}. Either single values or vectors the same
  length as the number of years can be passed. Single
  values are repeated for all years.}

  \item{make_plot}{A switch for whether to make a crude
  ggplot showing the results. Useful for testing and
  exploring the function.}

  \item{write_file}{A switch for whether to write
  \code{outfile} to disk. Can be turned off to speed up
  testing or exploration of the function (the new indices
  are returned invisibly, as in the examples below)}
}
\description{
  This function creates an index of abundance sampled from
  the expected available biomass for given fleets in given
  years. Let B_y be the biomass from the OM. Then the
  sampled value is calculated as: B_y*exp(rnorm(1, 0,
  \code{sds_obs})-\code{sds_obs}^2/2). The second term
  adjusts the random samples so that their expected value
  is B_y (i.e. the log-normal bias correction).
}
\examples{
\dontrun{
# Find the example data location:
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
outfile <- "test.dat"
ex1 <- change_index(infile, outfile, fleets=c(2,3),
                    years=list(1938:2012, 1938:2012) ,
                    sds_obs=list(1e-6, 1e-6), write_file=F)
ex2 <- change_index(infile, outfile, fleets=c(2,3),
                    years=list(1938:2012, 1938:2012) ,
                    sds_obs=list(.05, .05), write_file=F)
library(ggplot2)
ggplot(ex1, aes(x=year, y=obs, group=index, ymin=0,
                colour=as.factor(index)))+geom_line() + geom_point(data=ex2,
                aes(x=year, y=obs, colour=as.factor(index), group=index))
## Exclude a fleet and have varying sds_obs by year
ex3 <- change_index(infile, outfile, fleets=c(2,NA),
                    years=list(1938:2012, 1950),
                    sds_obs=list(seq(.001, .1, len=75), .1), write_file=F)
ggplot(ex3, aes(x=year, y=obs, group=index, ymin=0,
                colour=as.factor(index)))+geom_point()
}
}
\author{
  Cole Monnahan, Kotaro Ono
}

