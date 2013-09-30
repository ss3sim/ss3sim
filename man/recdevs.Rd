\docType{data}
\name{recdevs}
\alias{recdevs}
\title{Randomly generated standard-normal recruitment deviations}
\description{
  These data are randomly generated recruitment deviations
  with mean 0 and standard deviation 1. For legacy reasons
  (i.e. the matrix size was enlarged in the middle of a
  study) they were generated with the code listed in the
  example below.
}
\details{
  The columns are intended to be for iterations and the
  rows for years. The matrix has intentionally constructed
  at a large size to accommodate studies that work with
  simulations up to 125 years long and with up to 500
  iterations. If you want to work with more iterations or
  longer time series then you'll need to specify your own
  recdevs through the \code{user_recdevs} argument in
  \code{\link{ss3sim_base}}. The function
  \code{\link{change_rec_devs}} will throw out extra years
  of recruitment deviations.
}
\examples{
\dontrun{
recdevs <- matrix(nrow = 150, ncol = 1000)
set.seed(999)
recdevs[1:100,   1:100] <- matrix(nrow = 100, ncol = 100, data =
  rnorm(100*100))
recdevs[1:100, 101:500] <- matrix(nrow = 100, ncol = 400, data =
  rnorm(100*400))
recdevs[101:150, 1:500] <- matrix(nrow = 50,  ncol = 500, data =
  rnorm(50*500))
recdevs[1:150, 501:1000] <- matrix(nrow = 150, ncol = 500, data =
  rnorm(150*500))
recdevs <- recdevs[1:125, 1:500]
save(recdevs, file = "../data/recdevs.rda")
}
}
\keyword{data}
\keyword{datasets}

