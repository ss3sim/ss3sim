\name{setup_om}
\alias{setup_om}
\title{Setup operating models}
\usage{
  setup_om(home_name, om_location, ss_path)
}
\arguments{
  \item{home_name}{The base working directory for the
  simulation.}

  \item{om_location}{A vector of paths to folder locations
  for each of the necessary operating models. Folder names
  for the operating models should match how the scenarios
  label each of the operating models.}

  \item{ss_path}{Path to the Stock Synthesis executable
  file (e.g. in Windows: "c:\\SSFolder\\ss3.exe" or in
  Unix: "~/SSFolder/ss3")}
}
\description{
  Copy operating models from their home locations and place
  them in a master location for later use in the
  simulation.
}
\examples{
\dontrun{
# A Windows example:
setup_om(home_name = "c:\\\\ss", om_location = c(
"c:\\\\users\\\\kelli\\\\sardine", "c:\\\\users\\\\kelli\\\\cod"), ss_path =
"c:\\\\ss\\\\ss3.exe" )

# A Unix example:
setup_om(home_name = "~/Desktop/sim", om_location = c(
"~/Desktop/sardine", "~/Desktop/cod"), ss_path =
"~/Desktop/ss3" )
}
}
\author{
  Kelli Johnson
}

