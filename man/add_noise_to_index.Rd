\name{add_noise_to_index}
\alias{add_noise_to_index}
\title{Create a jittered version of a survey index based on biomass data}
\usage{
  add_noise_to_index(dat_file_in, dat_file_out, start_surv,
    end_surv, start_fish, end_fish, freq_surv = 2,
    sd_obs_surv = 0.2, freq_fish = 1, sd_obs_fish = 0.4,
    make_plot = FALSE)
}
\arguments{
  \item{dat_file_in}{Name of the data file to read in}

  \item{dat_file_out}{Name of the data file to write to}

  \item{start_surv}{Starting year survey index}

  \item{end_surv}{Ending year survey index}

  \item{start_fish}{Starting year fishery index}

  \item{end_fish}{Ending year fishery index}

  \item{freq_surv}{Frequency to return index values survey
  index}

  \item{sd_obs_surv}{Standard deviation of the observation
  error survey index}

  \item{freq_fish}{Frequency to return index values fishery
  index}

  \item{sd_obs_fish}{Standard deviation of the observation
  error fishery index}

  \item{make_plot}{Logical - make a plot of the biomass and
  index values?}
}
\description{
  This function is used to create an index of abundance
  sampled from the expected available biomass for each
  fleet: survey 1 and survey 2 (which mimics the fishery) +
  add some lognormal errors around it
}
\examples{
\\notrun{
Location of the R package code:
(I can't get system.file() to bring over the package files with the
correct permissions for SS_output() to work. Resorting to this in
the meantime. Download the package from Github and locate the
Simple example folder.)
f_in <- "~/Documents/github/ss3sim/inst/extdata/Simple/simple.dat"
# change as necessary
add_noise_to_index(f_in, "output_test.dat", start_surv = 1980,
end_surv = 2001, start_fish = 1980, end_fish = 2001,
make_plot = TRUE)
}
}
\author{
  Cole Monnahan, Kotaro Ono, Sean Anderson
}

