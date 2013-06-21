\name{change_lcomp}
\alias{change_lcomp}
\title{Change length comps}
\usage{
  change_lcomp(infile, outfile,
    distribution = "multinomial", Nsamp = NA, minyear = NA,
    maxyear = NA, years = NA, svyears = NA,
    fish_lcomp = TRUE, sv_lcomp = TRUE, lbin_method = NA,
    binwidth = NA, minimum_size = NA, maximum_size = NA,
    N_lbins = NA, lbin_vector = NA, lencomp = NA, cpar = 2)
}
\arguments{
  \item{infile}{SS data object from SS_readdat() in the
  r4ss package. Make sure you select option "section=2"}

  \item{outfile}{Name of the new file to be created. Path
  may be global or local. Make sure to give extension .dat
  to the file name.}

  \item{distribution}{Distribution to be used to sample the
  length compositions. Options are "multinomial" and
  "dirichlet"}

  \item{Nsamp}{Number of samples drawn from a multinomial
  distribution, or precision for the Dirichlet
  distribution}

  \item{minyear}{starting year for the fleet length comps.
  Overridden by specifying "years"}

  \item{maxyear}{ending year for the fleet length comps.
  Overridden by specifying "years"}

  \item{years}{vector of years for the fleet length comps.}

  \item{svyears}{vector of years for the survey lenght
  comps.}

  \item{lbin_method}{method to generate model length bins.
  See SS manual for details}

  \item{binwidth}{Bin width}

  \item{minimum_size,}{Minimum size}

  \item{maximum_size}{Maximum size}

  \item{N_lbins}{Number of length bins.}

  \item{lbin_vector}{Vector of length bins for the
  observations}

  \item{lencomp}{Matrix of length comps}

  \item{fish_lcomp,}{TRUE or FALSE. This indicates whether
  you want to keep the fishery lcomp data at all. default
  to TRUE}

  \item{sv_lcomp,}{TRUE or FALSE. This indicates whether
  you want to keep the survey lcomp data at all. default to
  TRUE}

  \item{cpar}{parameter scaling the variance of the
  Dirichlet and Multinomial distributions.}
}
\description{
  Take a data.SS_new file, resample the length compositions
  from the expected values, and return a new file with the
  new length comp samples. Samples can have dimensions,
  bins, sample sizes, and distributions which are different
  than those coming from SS.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)

# Generate a DAT file with the same dimensions as the original 'infile'
change_lcomp(infile,outfile="newdat.dat")

# Generate a DAT file with less length bins
change_lcomp(infile,outfile="newdat.dat", lbin_vector=seq(10,27,0.5))

# Generate a DAT file with a smaller sample size
change_lcomp(infile,outfile="newdat.dat", Nsamp=20)

# Generate a DAT file with a shorter time series, method 1
change_lcomp(infile,outfile="newdat.dat", Nsamp =100, minyear=1980, maxyear=2012)

# Generate a DAT file with a shorter time series, method 2
change_lcomp(infile,outfile="newdat.dat", Nsamp=100, years=1980:2012)

# Generate a DAT file using Dirichlet distributed samples
change_lcomp(infile,outfile="newdat.dat", Nsamp=100, distribution="dirichlet")
}
}
\author{
  Felipe Hurtado-Ferro (modified by Kotaro Ono)
}

