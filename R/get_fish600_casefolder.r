#' Get the folder location of the FISH600 case files
#'
#' This function is used by some developers of \pkg{ss3sim} for simulations. This
#' function links to the "cases" folder in \code{extdata}.
#' @export
#' @return A character object showing the location of the FISH600 case
#' files in the package \code{extdata} folder.
get_fish600_casefolder <- function() {
  d <- system.file("extdata", package = "ss3sim")
  f <- paste0(d, "/cases/")
  f
}


