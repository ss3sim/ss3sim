\name{plot_scalar_boxplot}
\alias{plot_scalar_boxplot}
\title{Plot scalar boxplot}
\usage{
  plot_scalar_boxplot(data, x, y, horiz = "species",
    vert = ".", vert2 = NULL, relative_error = FALSE,
    axes_free = TRUE)
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

  \item{x}{The x-value for all plots as a character
  string.}
}
\description{
  The same as \code{\link{plot_scalar_points}} except it
  produces boxplots and therefore has no color parameter.
  Intended use is to compare performance metrics,
  particularly relative error.
}
\examples{
plot_scalar_boxplot(scalars, x = "SR_LN_R0_om", y =
"SR_LN_R0_em", vert = "D")
}
\author{
  Cole Monnahan
}

