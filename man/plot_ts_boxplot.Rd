\name{plot_ts_boxplot}
\alias{plot_ts_boxplot}
\title{Plot timeseries boxplot}
\usage{
  plot_ts_boxplot(data, y, horiz = "species", vert = ".",
    vert2 = NULL, relative_error = FALSE, axes_free = TRUE)
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
}
\description{
  The same as \code{\link{plot_scalar_points}} except it
  produces boxplots across years for a timeseries data
  frame. Intended use is to compare performance metrics,
  particularly relative error.
}
\examples{
\dontrun{
# Not running this example because it can take a while to generate
plot_ts_boxplot(subset(final_results_ts, D == "D1"),
y = "SpawnBio_em", vert = "D")
}
}
\author{
  Cole Monnahan
}

