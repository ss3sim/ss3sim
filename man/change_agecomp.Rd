\name{change_agecomp}
\alias{change_agecomp}
\title{Resample age composition data}
\usage{
  change_agecomp(infile, outfile,
    distribution = "multinomial", Nsamp = 100, years = NA,
    svyears = NA, fish_agecomp = TRUE, sv_agecomp = TRUE,
    N_agebins = NA, agecomp = NA, cpar = 2)
}
\arguments{
  \item{infile}{SS data object from \code{SS_readdat()} in
  the r4ss package. Make sure you select option
  \code{section = 2}}

  \item{outfile}{Name of the new file to be created. Make
  sure to give the extension \code{.dat} to the file name.}

  \item{distribution}{Distribution to be used to sample the
  length compositions. Options are \code{"multinomial"} and
  \code{"dirichlet"}}

  \item{Nsamp}{Number of samples drawn from a multinomial
  distribution, or precision for the Dirichlet
  distribution. This could either be a single value if the
  sample size is the same for all years but could also be a
  vector with its length equal to the number of years with
  fishery and survey samples. For the latter, fishery
  sample size precedes the survey sample size. \code{Nsamp}
  only accepts numeric values and requires to be specified
  for both the fishery and survey EVEN IF it is not used in
  the end. Default to 100.}

  \item{N_agebins}{Number of age bins.}

  \item{years}{Vector of years for the fleet age comps.}

  \item{svyears}{Vector of years for the survey age comps.}

  \item{agecomp}{Matrix of age comps.}

  \item{fish_agecomp,}{\code{TRUE} or \code{FALSE}; default
  to \code{TRUE}. Sets whether to use or not use the
  fishery age composition data.}

  \item{sv_agecomp,}{\code{TRUE} or \code{FALSE}; default
  to \code{TRUE.} Sets whether to use or not use the survey
  age composition data.}

  \item{cpar}{\code{c} parameter scaling the variance of
  the Dirichlet and Multinomial distributions.}
}
\description{
  Take a \code{data.SS_new} file, resample the age
  compositions from the expected values, and return a new
  file with the new age composition samples. Samples can
  have dimensions, bins, sample sizes, and distributions
  which are different than those coming from SS.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/example-om/data.ss_new")
infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)

# Generate a .dat file with the same dimensions as the 'infile'
change_agecomp(infile, outfile = "newdat.dat")

# Generate a .dat file with a smaller sample size
change_agecomp(infile, outfile = "newdat.dat", Nsamp=20)

# Generate a .dat file with a shorter time series
change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
  years = 1980:2012)

# Generate a .dat file using Dirichlet distributed samples
change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
  distribution = "dirichlet")

# Generate a .dat file using Dirichlet distributed samples for the
# fishery and Multinomial for the survey
change_agecomp(infile, outfile = "newdat.dat", Nsamp = 100,
  distribution = c("dirichlet", "multinomial"))

# Generate a .dat file using Dirichlet distributed samples for the
# fishery with 50 samples and Multinomial for the survey with 100
# samples
change_agecomp(infile, outfile = "newdat.dat", distribution =
  c("dirichlet", "multinomial"), years = seq(1994, 2012, by = 2),
  svyears = seq(2003, 2012), Nsamp = c(rep(50, 10), rep(100, 10)))

# cleanup:
unlink("newdat.dat")
}
}
\author{
  Roberto Licandeo, Felipe Hurtado-Ferro
}

