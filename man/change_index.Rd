\name{change_index}
\alias{change_index}
\title{Change index values}
\usage{
  change_index(operating.model.path, assessment.model.path,
    dat.filename, start.year, end.year, frequency = 1,
    sd.obs = 0.1, make.plot = FALSE)
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
  the Report.sso file using the r4ss function and then
  splits it.
}

