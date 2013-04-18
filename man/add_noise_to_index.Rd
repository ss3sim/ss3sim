\name{add_noise_to_index}
\alias{add_noise_to_index}
\title{Create a jittered version of a survey index based on biomass data}
\usage{
  add_noise_to_index(operating_path_in, assess_path_in,
    dat_file_in, assess_path_out, dat_file_out, start_surv,
    end_surv, freq_surv = 1, sd_obs_surv = 0.1, start_fish,
    end_fish, freq_fish = 1, sd_obs_fish = 0.1,
    make_plot = FALSE)
}
\arguments{
  \item{operating_path_in}{Folder path to the operating
  model}

  \item{assess_path_in}{Folder path to the assessment
  model}

  \item{dat_file_in}{Name of the data file to read in}

  \item{assess_path_out}{Folder path of the assessment
  model to write to.}

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
}
\description{
  This function is used to create an index of abundance
  sampled from the true biomass. It takes the biomass from
  the Report.sso file using the \code{r4ss} function and
  then splits it.
}
\examples{
\\notrun{
# Location of the R package code:
# (I can't get system.file() to bring over the package files with the
# correct permissions for SS_output() to work. Resorting to this in
# the meantime.)
f_op <- "~/Documents/github/ss3sim/inst/extdata/flatfish-operating"
f_as <- "~/Documents/github/ss3sim/inst/extdata/flatfish-assessment"
add_noise_to_index(f_op, f_as, "flatfish.dat", assess_path_out = ".",
dat_file_out = "flatfish-modified.dat", start_year = 1930,
end_year = 1981, 1, make_plot = TRUE)
}
}
\author{
  Cole Monnahan (modified by Kot)
}

