#' Helper function for building a ggplot facet
#'
#' Used internally by the plotting functions to create faceting formulas.
#'
#' @inheritParams plot_ss3sim
#' @author Cole Monnahan
#' @return A formula which can be used in [ggplot2::facet_grid()] or `NULL`
#' if all arguments are `NULL`.
#'
facet_form <- function(horiz = NULL, horiz2 = NULL, vert = NULL, vert2 = NULL) {
  h <- !is.null(horiz)
  h2 <- !is.null(horiz2)
  v <- !is.null(vert)
  v2 <- !is.null(vert2)
  ## All NULL means no faceting
  if (!h & !h2 & !v & !v2) {
    return(NULL)
  }
  ## If user provides horiz2 but not horiz1, switch them, likewise with vert
  if (!h & h2) {
    horiz <- horiz2
    horiz2 <- NULL
    h2 <- FALSE
    h <- TRUE
  }
  if (!v & v2) {
    vert <- vert2
    vert2 <- NULL
    v2 <- FALSE
    v <- TRUE
  }
  ## Build the formula, depending on nested cases
  if (!h & !h2) {
    if (v & !v2) {
      form <- stats::as.formula(paste(". ~", vert))
    } else {
      form <- stats::as.formula(paste(". ~", vert, "+", vert2))
    }
  } else if (h & !h2) {
    if (!v & !v2) {
      form <- stats::as.formula(paste(horiz, "~ ."))
    } else if (v & !v2) {
      form <- stats::as.formula(paste(horiz, "~", vert))
    } else {
      form <- stats::as.formula(paste(horiz, "~", vert, "+", vert2))
    }
  } else if (h & h2) {
    if (!v & !v2) {
      form <- stats::as.formula(paste(horiz, "+", horiz2, "~ ."))
    } else if (v & !v2) {
      form <- stats::as.formula(paste(horiz, "+", horiz2, "~", vert))
    } else {
      form <- stats::as.formula(paste(horiz, "+", horiz2, "~", vert, "+", vert2))
    }
  } else {
    stop("Incompatible horiz and vert arguments")
  }
  return(form)
}

#' Helper function for ensuring correct input for the plotting functions
#'
#' Used internally by the plotting functions to check that the arguments are
#' structured appropriately.
#'
#' @template plot-functions
#' @template plot-functions-color
#' @return Nothing is returned; an informative error is thrown if an
#' argument is invalid.
verify_plot_arguments <- function(data, x, y, horiz, horiz2, vert, vert2,
                                  color, relative.error, axes.free, print) {
  if (!is.data.frame(data)) {
    stop("data must be data.frame")
  } else if (nrow(data) < 2) {
    stop("data has too few rows")
  }
  if (!is.character(x) | !x %in% names(data)) {
    stop("x must be character matching column in data")
  }
  if (!is.character(y) | !y %in% names(data)) {
    stop("y must be character matching column in data")
  }
  if (!is.null(horiz)) {
    if (!is.character(horiz) | !horiz %in% names(data)) {
      stop("horiz must be character matching column in data")
    }
  }
  if (!is.null(horiz2)) {
    if (!is.character(horiz2) | !horiz2 %in% names(data)) {
      stop("horiz2 must be character matching column in data")
    }
  }
  if (!is.null(vert)) {
    if (!is.character(vert) | !vert %in% names(data)) {
      stop("vert must be character matching column in data")
    }
  }
  if (!is.null(vert2)) {
    if (!is.character(vert2) | !vert2 %in% names(data)) {
      stop("vert2 must be character matching column in data")
    }
  }
  if (!is.null(color)) {
    if (!is.character(color) | !color %in% names(data)) {
      stop("color must be character matching column in data")
    }
  }
  stopifnot(!is.na(relative.error))
  stopifnot(is.logical(axes.free))
  stopifnot(is.logical(print))
  ## No need to return anything, throws an error if something wrong
}
