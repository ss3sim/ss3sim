\name{plot_scalar_points}
\alias{plot_scalar_points}
\title{Plot scalar points}
\usage{
  plot_scalar_points(data, x, y, horiz = "species",
    vert = ".", vert2 = NULL, relative_error = FALSE,
    axes_free = TRUE, color = "max_grad")
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

  \item{color}{Character of a variable for how to color the
  points. Useful variables are \code{max_grad} or other
  measures of convergence.}
}
\description{
  Flexible function to make jittered scatter plots from a
  scalar data frame. Use it to quickly explore results by
  varying dimensions and $x$ and $y$ variables to explore
  patterns. Intended use is to explore model convergence
  and other issues to help identify outliers or model
  misspecification.
}
\examples{
# Explore the error in log R0 vs the maximum gradient. Outliers may
# be apparent
plot_scalar_points(scalars, x = "SR_LN_R0_om", y =
"SR_LN_R0_em", color = "max_grad", vert = "D")
}
\author{
  Cole Monnahan
}

