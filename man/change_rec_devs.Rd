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
  This function replaces the recruitment deviations in the
  \code{ss3.par} file with those specified in
  \code{recdevs_new}, as well as a comment (for debugging).
  It then writes a new file with name \code{file_out} into
  the working directory.
}
\examples{
\dontrun{
par_file <- system.file("extdata", "models", "cod-om", "ss3.par",
  package = "ss3sim")
change_rec_devs(recdevs_new = rlnorm(100), file_in = par_file,
  file_out = "test.par")
# Now go look at the deviations after "recdev1:" in test.par in your
# working directory.
unlink("test.par")
}
}
\author{
  Cole Monnahan
}

