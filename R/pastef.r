#' Paste with "/" as the separator (deprecated)
#'
#' @param ... Objects to paste together
pastef <- function(...) {
  message("ss3sim utility function pastef is deprecated and should not be used",
          "in ss3sim. Using file.path instead.")
  #paste(..., sep = "/")
  file.path(...)
}
