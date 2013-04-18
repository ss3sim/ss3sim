\name{add_noise_to_index}
\alias{add_noise_to_index}
\title{Create a jittered version of a survey index based on biomass data}
\usage{
  add_noise_to_index(operating.model.path,
    input.assessment.model.path, input.dat.filename,
    output.assessment.model.path, output.dat.filename,
    start.year, end.year, frequency = 1, sd.obs = 0.1,
    make.plot = FALSE)
}
\arguments{
  \item{operating.model.path}{}

  \item{assessment.model.path}{}

  \item{dat.filename}{}

  \item{start.year}{}

  \item{end.year}{}

  \item{frequency}{}

  \item{sd.obs}{}

  \item{make.plot}{}
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
add_noise_to_index(f_op, f_as, "flatfish.dat", ".",
"flatfish-modified.dat", 1930, 1981, 1, make.plot = TRUE)
}
}
\author{
  Cole Monnahan
}

