\name{change_rec_devs}
\alias{change_rec_devs}
\title{Replace recruitment deviations}
\usage{
  change_rec_devs(recdevs_new, file_in = "ss3.par",
    file_out = "ss3.par")
}
\arguments{
  \item{recdevs_new}{A vector of new recruitment
  deviations.}

  \item{file_in}{Input SS3 par file.}

  \item{file_out}{Output SS3 par file.}
}
\description{
  This function replaces the recdevs in the ss3.par file
  with those specified in recdevs_new, as well as a comment
  (for debugging). It then writes a new file with name
  file_out into the working directory.
}
\examples{
change_rec_devs(recdevs_new=rlnorm(58), file_out="test.par") ## give it wrong num.years
change_rec_devs(recdevs_new=rlnorm(57), file_out="test.par") ## give it right ones
}
\author{
  Cole Monnahan
}

