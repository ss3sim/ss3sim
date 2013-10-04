\name{change_lcomp}
\alias{change_lcomp}
\title{Sample length compositions from expected values}
\usage{
  change_lcomp(infile, outfile, fleets = c(1, 2), Nsamp,
    years, cpar = 1, lengthbin_vector = NULL,
    write_file = TRUE)
}
\arguments{
  \item{infile}{SS data object as read in from
  \code{SS_readdat} in the r4ss packlength. Make sure you
  select option \code{section=2}.}

  \item{outfile}{Character string of the name for the new
  file to be created. Must end in \code{.dat}.}

  \item{fleets}{Numeric vector giving the fleets to be
  used. This order also pertains to other arguments. A
  value of \code{NA} can be passed to exclude that fleet
  from outfile (i.e. turn it off).}

  \item{Nsamp}{A numeric list of the same length as
  \code{fleets}. Either single values or vectors the same
  length as the number of years can be passed through.
  Single values are repeated for all years.}

  \item{years}{A numeric list of the same length as fleets.
  Each element specifies the years to sample from each
  fleet. Years left out are excluded in \code{outfile},
  allowing the user to reduce (but not increase) the sample
  scheme as given in \code{infile}}

  \item{lengthbin_vector}{A numeric vector giving the new
  length bins to use. \code{agebin_vector} must be within
  the [min;max] of population bin. This feature allows
  dynamic binning by the user, but is not fully tested.
  Users should consult the vignette and carefully check the
  function bins the data as desired before proceeding with
  simulations.}

  \item{cpar}{A numeric value or vector the same length as
  \code{fleets} controlling the variance of the Dirichlet
  distribution used for sampling. A value of \code{1}
  indicates the same standard deviation as a multinomial of
  the given \code{Nsamp}, \code{2} indicates twice, etc.
  Values greater than one indicate overdispersion, and less
  underdispersion.}

  \item{write_file}{A switch for whether to write
  \code{outfile} to disk. Can be turned off to speed up
  testing or exploration of the function (the new lcomp is
  returned invisibly)}
}
\description{
  Take a \code{data.SS_new} file containing expected values
  and sample to create observed length compositions which
  are then written to file for use by the EM.
}
\examples{
\dontrun{
}
}
\author{
  Cole Monnahan and Kotaro Ono; modified from a version by
  Roberto Licandeo and Felipe Hurtado-Ferro
}

