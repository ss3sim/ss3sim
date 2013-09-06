\name{verify_input}
\alias{verify_input}
\title{Verify and standardize SS3 input files}
\usage{
  verify_input(model_dir, type = c("om", "em"))
}
\arguments{
  \item{model_dir}{directory name for model}

  \item{type}{One of "om" or "em" for operating or
  estimating model}
}
\description{
  This function verifies the contents of operating model
  (om) and estimation model (em) folders. If the contents
  are correct, the \code{.ctl} and \code{.dat} files are
  renamed to standardized names and the `starter.ss` file
  is updated to reflect these names. If the contents are
  incorrect then a warning is issued and the simulation is
  aborted.
}
\details{
  This is a helper function to be used within the larger
  wrapper simulation functions.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
om <- paste0(d, "/ss3sim_base_eg/cod_om")
em <- paste0(d, "/ss3sim_base_eg/cod_em")
file.copy(om, ".", recursive = TRUE)
file.copy(em, ".", recursive = TRUE)
# verify the correct files exist and change file names:
verify_input(model_dir = "cod_om", type = "om")
verify_input(model_dir = "cod_em", type = "om")
# clean up:
unlink("cod_om", recursive = TRUE)
unlink("cod_em", recursive = TRUE)
}
}
\author{
  Curry James Cunningham; modified by Sean Anderson
}

