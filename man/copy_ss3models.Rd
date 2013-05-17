\name{copy_ss3models}
\alias{copy_ss3models}
\title{Copy operating and estimation models while creating folder
structure}
\usage{
  copy_ss3models(model_dir, scenarios, iterations = 1:100,
    type = c("om", "em"))
}
\arguments{
  \item{model_dir}{A directory containing the operating or
  estimation models. Each folder should be named according
  to a scenario ID.}

  \item{iterations}{The iterations to copy to. The function
  will create the folders as needed.}

  \item{scenarios}{Which scenarios to copy to. Supply a
  vector of character elements.}

  \item{type}{Are you copying operating or estimation
  models? This affects whether the model folder gets named
  "om" or "em"}
}
\description{
  Copy operating and estimation models while creating
  folder structure
}
\examples{
\dontrun{
dir.create("oms/blockm-cod")
dir.create("oms/blockm-fla")
copy_ss3models(model_dir = "oms", type = "om", iterations = 1:3)
}
}
\author{
  Sean Anderson, Kelli Johnson
}

