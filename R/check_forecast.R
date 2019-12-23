#' Check input forecast file values
#' 
#' Ensure that the \code{forecast.ss} file is configured for use
#' in ss3sim.
#' \itemize{
#'   \item fish at F_{MSY}
#'   \item use relative benchmark years (i.e., \code{Bmark_years})
#'   \item use relative years for fishing specifications 
#'   (i.e., \code{Fcast_years})
#' }
#' 
#' @template for_list
#' 
#' @author Kelli Faye Johnson
#' @return A list object as returned by \code{\link[r4ss]{SS_readforecast}}
#' 
check_forecast <- function(for_list) {
  for_list$Forecast <- 2 #Fish at F(MSY)
  check_relativeyears <- function(x) {
    vals <- x[seq_along(x) %% 2 == 1]
    if (!all(vals <= 0)) stop("Some reference years in the forecast",
      " file are positive,\n  and only relative years from the end year",
      " are allowed in ss3sim.")
  }
  check_relativeyears(for_list$Bmark_years)
  check_relativeyears(for_list$Fcast_years)
  invisible(for_list)
}
