\name{create_dirs}
\alias{create_dirs}
\title{Create simulation directories}
\usage{
  create_dirs(home_name = ".", scen_list, beg = 1,
    nsets = 100)
}
\arguments{
  \item{home_name}{The base working directory for the
  simulation.}

  \item{scen_list}{A list of vectors, where each vector
  specifies folder names for the scenario options. Enter
  vectors in order of their hierarchy. Names in the vector
  specifying the operating models should match the folder
  names that house the operating models in the master
  location. See the example below.}

  \item{beg}{A numeric value for the first iteration of the
  simulation.}

  \item{nsets}{A numeric value for the number of iterations
  to run.}
}
\description{
  Create a tree of file directories based on the specified
  scenarios for the simulation analyses and copy the
  respective operating models from the master location to
  the trial folders.
}
\examples{
\dontrun{
# Change the home_name directory based on where you'd like the
# example folders created:
create_dirs(scen_list = list(main = "ss3sim",
papers = "m", species = c("cod","flat","sardine"), scenarios =
c("down-base","up-base","contrast-base")), nsets = 100, copy_om = FALSE)
# cleanup
}
}
\author{
  Kelli Johnson
}

