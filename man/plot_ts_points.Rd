\name{plot_ts_points}
\alias{plot_ts_points}
\title{Plot timeseries points}
\usage{
  plot_ts_points(data, y, horiz = "species", vert = ".",
    vert2 = NULL, relative_error = FALSE, axes_free = TRUE,
    color = "max_grad")
}
\arguments{
  \item{data}{A data frame holding results}

  \item{y}{The y-value for all plots as a character
  string.}

  \item{horiz}{Character of a variable for which variable
  to use for grouping into horizontal rows.}

  \item{vert}{Character of a variable for which variable to
  use for grouping into vertical columns.}

  \item{vert2}{Character of a variable for how to further
  divide the vertical columns}

  \item{relative_error}{Boolean for whether the y-value is
  relative}

  \item{axes_free}{Boolean. If TRUE axes between different
  rows and columns (i.e. between levels of \code{horiz},
  \code{vert} and \code{vert2}) will be set automatically.
  If \code{FALSE} all x- and y-axes will have a shared
  scale.}

  \item{color}{Character of a variable for how to color the
  points. Useful variables are \code{max_grad} or other
  measures of convergence.}
}
\description{
  The same as \code{\link{plot_scalar_points}} except it
  produces points across years for a timeseries data frame.
  Intended use is to explore model convergence and
  diagnostics.
}
\examples{
final_results <- merge(timeseries,
scalars[,c("scenario", "replicate", "max_grad")])
plot_ts_points(final_results, y = "SpawnBio_em", vert = "D",
color = "max_grad")
}
\author{
  Cole Monnahan
}
\seealso{
  Other plot.results: \code{\link{plot_scalar_boxplot}},
  \code{\link{plot_scalar_points}},
  \code{\link{plot_ts_boxplot}}
}

