\name{verify_input}
\alias{verify_input}
\title{Function to verify and standardize SS3 input files}
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
  (om) and estimation model (em) folders. IF the contents
  are correct, .ctl and .dat files are renamed to
  standardized names, ELSE warning is issued and process
  aborted.

  Steps: a) Check to ensure correct files are specified and
  output warning if not - SANITY CHECK b) Rename .ctl and
  .dat files accordingly from specified OM and EM files c)
  Change input .ctl and .dat names in starter.SS file
}
\details{
  This is a helper function to be used within the larger
  SS3 simulation wrapper function.
}
\examples{
\dontrun{
verify.input(model_dir = "cod_om", type = "om")
verify.input(model_dir = "cod_em", type = "em")
}
}
\author{
  Curry James Cunningham
}

