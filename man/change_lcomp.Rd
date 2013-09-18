\name{change_lcomp}
\alias{change_lcomp}
\title{Change length comps}
\usage{
  change_lcomp(infile, outfile,
    distribution = "multinomial", Nsamp = 100, years = NA,
    svyears = NA, fish_lcomp = TRUE, sv_lcomp = TRUE,
    cpar = 2, minyear = NA, maxyear = NA, lbin_method = NA,
    lbin_vector = NA, binwidth = NA, minimum_size = NA,
    maximum_size = NA, N_lbins = NA, lencomp = NA)
}
\arguments{
  \item{infile}{SS data object from SS_readdat() in the
  r4ss package. Make sure you select option "section=2"}

  \item{outfile}{Name of the new file to be created. Path
  may be global or local. Make sure to give extension .dat
  to the file name.}

  \item{distribution}{Distribution to be used to sample the
  length compositions. Options are "multinomial" and
  "dirichlet". it could also be written in the form of a
  vector if the fishery and survey composition data are not
  sampled using the same distribution}

  \item{Nsamp}{Number of samples drawn from a multinomial
  distribution, or precision for the Dirichlet
  distribution. This could either be a single value if the
  sample size is the same for all years but could also be a
  vector with its length equal to the number of years with
  fishery and survey samples. For the latter, fishery
  sample size precedes the survey sample size. Nsamp only
  accepts numeric values and requires to be specified for
  both the fishery and survey EVEN IF it is not used in the
  end. Default to 100.}

  \item{years}{vector of years for the fleet length comps.}

  \item{svyears}{vector of years for the survey lenght
  comps.}

  \item{minyear}{starting year for the fleet length comps.
  Overridden by specifying "years". Not operational. Leave
  to the default value of NA.}

  \item{maxyear}{ending year for the fleet length comps.
  Overridden by specifying "years". Not operational. Leave
  to the default value of NA.}

  \item{lbin_method}{method to generate model length bins.
  Not operational yet. Leave to the default value of
  NA.Leave to the default value of NA.}

  \item{lbin_vector}{Vector of length bins for the
  observations. Not operational yet. Leave to the default
  value of NA.}

  \item{binwidth}{Bin width}

  \item{minimum_size,}{Minimum size}

  \item{maximum_size}{Maximum size}

  \item{N_lbins}{Number of length bins.}

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

# Generate a DAT file with a smaller sample size
change_lcomp(infile,outfile="newdat.dat", Nsamp=20)

# Generate a DAT file with a shorter time series
change_lcomp(infile,outfile="newdat.dat", Nsamp=100, years=1980:2012)

# Generate a DAT file using Dirichlet distributed samples
change_lcomp(infile,outfile="newdat.dat", Nsamp=100, distribution="dirichlet")
}
# Generate a DAT file using Dirichlet distributed samples for the fishery and Multinomial for the survey
change_lcomp(infile,outfile="newdat.dat", Nsamp=100, distribution=c("dirichlet", "multinomial"))
}
# Generate a DAT file using Dirichlet distributed samples for the fishery with 50 samples
# and Multinomial for the survey with 100 samples
change_lcomp(infile,outfile="newdat.dat", distribution=c("dirichlet", "multinomial"), years = seq(1994,2012,by=2), svyears=seq(2003,2012), Nsamp=c(rep(50,10), rep(100,10)))
}
}
\author{
  Felipe Hurtado-Ferro (modified by Kotaro Ono)
}

