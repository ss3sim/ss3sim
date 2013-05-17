\name{change_index}
\alias{change_index}
\title{Sample the biomass with observation error to simulate a survey}
\usage{
  change_index(dat_file_in, dat_file_out, start_surv,
    end_surv, freq_surv = 2, sd_obs_surv = 0.2,
    start_fish = 1, end_fish = 1, freq_fish = 1,
    sd_obs_fish = 0.4, make_plot = FALSE,
    use_index = c("survey", "fishery", "all"))
}
\arguments{
  \item{dat_file_in}{Name of the data file to read in}

  \item{dat_file_out}{Name of the data file to write to}

  \item{start_surv}{Starting year survey index}

  \item{end_surv}{Ending year survey index}

  \item{freq_surv}{Frequency to return index values survey
  index}

  \item{sd_obs_surv}{Standard deviation of the observation
  error survey index}

  \item{start_fish}{Starting year fishery index}

  \item{end_fish}{Ending year fishery index}

  \item{freq_fish}{Frequency to return index values fishery
  index}

  \item{sd_obs_fish}{Standard deviation of the observation
  error fishery index}

  \item{make_plot}{Logical - make a plot of the biomass and
  index values?}

  \item{use_index}{Specify which abundance index you want
  to use. Defaults to \code{"survey"}.}
}
\description{
  This function creates an index of abundance sampled from
  the expected available biomass for each fleet: survey 1
  and survey 2 (which mimics the fishery). Lognormal errors
  are added around the index. By default only the
  fishery-independent survey is sampled.
}
\examples{
\dontrun{
# Find the "Simple" example data location:
d <- system.file("extdata", package = "ss3sim")
f_in <- paste0(d, "/Simple/simple.dat")
change_index(f_in, "output_test.dat", start_surv = 1980,
end_surv = 2001, start_fish = 1980, end_fish = 2001,
make_plot = TRUE, use_index = "survey")
# Clean up:
file.remove("output_test.dat")
}
}
\author{
  Cole Monnahan, Kotaro Ono, Sean Anderson
}

