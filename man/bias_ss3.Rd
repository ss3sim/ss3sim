\name{bias_ss3}
\alias{bias_ss3}
\title{Perform a single bias adjustment run}
\usage{
  bias_ss3(iter, dir)
}
\arguments{
  \item{iter}{Replicate number, used to identify this
  iteration if there are multiple adjustment runs.}

  \item{dir}{Passes \code{dir} from the function
  \code{\link{run_bias_ss3}} to \code{bias_ss3} In
  \code{\link{run_bias_ss3}} this is run within an
  \code{sapply} function for each of the bias adjustment
  runs.}
}
\description{
  This function is run within \code{\link{run_bias_ss3}}
  and for a single run it: \itemize{ \item uses \code{r4ss}
  function \code{SS_output} to read in the output from a
  single bias adjustment run \item uses \code{r4ss}
  function \code{SS_fitbiasramp} to calculate the bias
  adjustment parameters for that run \item Writes the bias
  adjustment parameters to the file \code{AdjustBias.DAT}
  within the \code{dir} folder, overwriting the file if
  \code{iter = 1} (the first run) and appending the file
  otherwise }
}
\author{
  Carey McGilliard
}
\references{
  Methot, R. D. and Taylor, I. G. (2011). Adjusting for
  bias due to variability of estimated recruitments in
  fishery assessment models. Can. J. Fish. Aquat. Sci.,
  68(10):1744–1760.
}
\seealso{
  \code{\link{run_bias_ss3}}, \code{\link{run_ss3sim}},
  \code{link{ss3sim_base}}
}

