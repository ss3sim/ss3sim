\name{write_rec_devs}
\alias{write_rec_devs}
\title{Replace recruitment deviations}
\usage{
  write_rec_devs(recdevs_new, file_in = "ss3.par",
    file_out = file_in)
}
\arguments{
  \item{recdevs_new}{A vector of new recruitment
  deviations.}

  \item{file_in}{Input SS3 par file.}

  \item{file_out}{Output SS3 par file. Defaults to
  overwriting file_in}
}
\description{
  This function replaces the recdevs in the ss3.par file
  with those specified in recdevs_new, as well as a comment
  (for debugging). It then writes a new file with name
  file_out into the working directory.
}
\examples{
write.recdevs(recdevs_new=rlnorm(58), file_out="test.par") ## give it wrong num.years
write.recdevs(recdevs_new=rlnorm(57), file_out="test.par") ## give it right ones
}

