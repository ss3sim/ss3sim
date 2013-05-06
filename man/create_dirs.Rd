\name{create_dirs}
\alias{create_dirs}
\title{Create simulation directories}
\usage{
  create_dirs(home_name, scenlist, beg = 1, nsets = 100,
    copy_om = TRUE)
}
\arguments{
  \item{home_name}{The base working directory for the
  simulation.}

  \item{scenlist}{A list of vectors, where each vector
  specifies folder names for the scenario options. Enter
  vectors in order of their hierarchy. Names in the vector
  specifying the operating models should match the folder
  names that house the operating models in the master
  location. See the example below.}

  \item{beg}{A numeric value for the first iteration of the
  simulation.}

  \item{nsets}{A numeric value for the number of iterations
  to run.}

  \item{copy_om}{A binary value indicating whether or not
  to copy the necessary operating model from the master
  location to each iteration folder.}
}
\description{
  Create a tree of file directories based on the specified
  scenarios for the simulation analyses and copy the
  respective operating models from the master location to
  the trial folders.
}
\examples{
\dontrun{
scenlist <- list()
scenlist[[1]] <- c("scen_a", "scen_b")
scenlist[[2]] <- c("sub_scen_a", "sub_scen_b")
# Change the home_name directory based on where you'd like the
example folders created:
create_dirs(home_name = "~/Desktop/", scenlist = scenlist, beg = 1,
  nsets = 5, copy_om = FALSE)
}
}
\author{
  Kelli Johnson
}

