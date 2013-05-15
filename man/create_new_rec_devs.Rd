\name{create_new_rec_devs}
\alias{create_new_rec_devs}
\title{Generate a new recruitment deviation time series to put into a simulation}
\usage{
  create_new_rec_devs(rec_dev_dat, best_model)
}
\arguments{
  \item{rec_dev_dat}{Recruitment deviation data. A vector.
  Only used to get the length of recruitment deviations to
  create.}

  \item{best_model}{A model from arima(). Intended to be
  output from find_rec_dev_arma().}
}
\value{
  A vector of new recruitment deviations.
}
\description{
  Generate a new recruitment deviation time series to put
  into a simulation
}
\author{
  Kotaro Ono m <- arima(lh, order = c(1,0,0))
  create_new_rec_devs(rep(0, 20), m)
}

