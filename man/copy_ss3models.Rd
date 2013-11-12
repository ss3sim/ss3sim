\name{copy_ss3models}
\alias{copy_ss3models}
\title{Copy the operating and estimation models and create a folder
structure}
\usage{
  copy_ss3models(model_dir, scenarios, iterations = 1:100,
    type = c("om", "em"))
}
\arguments{
  \item{model_dir}{A directory containing the operating or
  estimation model. Each folder should be named according
  to a scenario ID. (See the vignette
  \code{vignette("ss3sim-vignette")} or
  \code{\link{get_caseargs}} for details on the scenario ID
  format.)}

  \item{iterations}{A numeric vector of the iterations to
  copy to. The function will create the folders as needed.}

  \item{scenarios}{Which scenarios to copy to. Supply a
  vector of character elements.}

  \item{type}{Are you copying operating or estimation
  models? This affects whether the model folder gets named
  "om" or "em"}
}
\value{
  A set of nested folders starting with the scenario ID,
  then the iterations, then "om" or "em", and then the SS
  model files.
}
\description{
  Copy the operating and estimation models and create a
  folder structure
}
\examples{
# Locate the package data:
om_folder <- system.file("extdata", "models", "cod-om", package =
  "ss3sim")

# Copy the operating model:
copy_ss3models(model_dir = om_folder, type = "om", iterations =
  1:3, scenarios = "D0-E0-F0-M0-R0-testing")
# Now look at your working directory in your file system

# Copy the estimation model with two scenario IDs:
copy_ss3models(model_dir = om_folder, type = "em", iterations = 1:2,
  scenarios = c("D1-E0-F0-M0-R0-testing", "D1-E1-F0-M0-R0-testing"))
# (Note that all the scenario argument does here is affect the
# folder names.)

# Clean up:
unlink("D0-E0-F0-M0-R0-testing", recursive = TRUE)
unlink("D1-E0-F0-M0-R0-testing", recursive = TRUE)
unlink("D1-E1-F0-M0-R0-testing", recursive = TRUE)
}
\author{
  Sean Anderson, Kelli Johnson
}

