\name{find_rec_dev_arma}
\alias{find_rec_dev_arma}
\title{Find the best ARMA model of the recruitment deviations}
\usage{
  find_rec_dev_arma(working_folder)
}
\arguments{
  \item{working_folder}{The folder with SS3 output.}
}
\value{
  An arima() model object.
}
\description{
  Find the best ARMA model of the recruitment deviations
}
\details{
  Fits all ARMA all models from AR [0:2] and MA [0:2] to
  the recruitment deviations. Returns the model with the
  lowest AIC value.
}

