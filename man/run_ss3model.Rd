\name{run_ss3model}
\alias{run_ss3model}
\title{Run the operating models for a specified set of scenario IDs}
\usage{
  run_ss3model(scenarios, iterations, type = c("om", "em"),
    ss3path = NULL, admb_options = "", hess = FALSE,
    ignore.stdout = TRUE)
}
\arguments{
  \item{scenarios}{Which scenarios to run. Controls which
  folder contains the model that SS3 should run on.}

  \item{iterations}{Which iterations to run. Controls which
  folder contains the model that SS3 should run on.}

  \item{type}{Are you running the operating or estimation
  models?}

  \item{ss3path}{The path to your SS3 binary the binary is
  not in your path. For example, if \code{SS3} was in the
  folder \code{/usr/bin/} then \code{ss3path =
  "/usr/bin/"}. Make sure to append a slash to the end of
  this path. Defaults to \code{NULL}, which means the
  function will assume the binary is already in your path.}

  \item{hess}{Calculate the Hessian on estimation model
  runs?}

  \item{admb_options}{Any additional options to pass to the
  SS3 command.}
}
\description{
  This function takes care of calling SS3. Importantly, it
  parses whether the user is on Unix or Windows and calls
  the binary correctly. This lower-level function is meant
  to be called by higher level functions such as
  \code{\link{run_ss3sim}} and \code{\link{run_fish600}}.
}
\author{
  Sean C. Anderson
}
\seealso{
  \code{\link{run_ss3sim}}, \code{\link{run_fish600}}
}

