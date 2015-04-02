#' Calculate relative error
#'
#' Takes a scalar or time series data frame from an \pkg{ss3sim} run and
#' calculates relative error (em - om) / em.
#'
#' @param dat An input data frame. Should be either a scalar or time series
#'   data frame as returned from \code{\link{get_results_all}} or a related
#'   get results function. Specifically, the data frame needs to have columns
#'   with \code{_em} and \code{_om} as names.
#' @param add Logical: should the relative error columns be added to \code{dat}
#'   or should the original EM and OM columns be dropped? If \code{FALSE} then
#'   the returned data frame will have only the identifying columns and the new
#'   relative error columns. You could then merge selected columns back into
#'   \code{dat} if you wished.
#' @author Sean Anderson and Cole Monnahan
#' @seealso \code{\link{get_results_all}}, \code{link{get_results_scenario}}
#' @export
#' @examples
#' # Example with built in package data:
#' d1 <- system.file("extdata", "output", "ss3sim_ts.csv",
#'   package = "ss3sim")
#' d2 <- system.file("extdata", "output", "ss3sim_scalar.csv",
#'   package = "ss3sim")
#' ss3sim_ts <- read.csv(d1)
#' ss3sim_scalar <- read.csv(d2)
#'
#' head(calculate_re(ss3sim_ts))
#' head(calculate_re(ss3sim_ts, add = FALSE))
#' head(calculate_re(ss3sim_scalar, add = FALSE))
#'
#' \dontrun{
#' # Full example:
#' d <- system.file("extdata", package = "ss3sim")
#' om <- paste0(d, "/models/cod-om")
#' em <- paste0(d, "/models/cod-em")
#' case_folder <- paste0(d, "/eg-cases")
#'
#' run_ss3sim(iterations = 1, scenarios = "D0-F0-cod",
#'   case_folder = case_folder, om_dir = om, em_dir = em, ss_mode = "optimized")
#'
#' get_results_all(user_scenarios = "D0-F0-cod")
#' ss3sim_ts <- read.csv("ss3sim_ts.csv")
#' ss3sim_scalar <- read.csv("ss3sim_scalar.csv")
#'
#' head(calculate_re(ss3sim_ts))
#' head(calculate_re(ss3sim_scalar, add = FALSE))
#'
#' # clean up:
#' unlink("D0-F0-cod", recursive = TRUE)
#' unlink("ss3sim_ts.csv", recursive = TRUE)
#' unlink("ss3sim_scalar.csv", recursive = TRUE)
#' }
calculate_re <- function(dat, add = TRUE) {

  em_cols <- names(dat)[grep("_em", names(dat))]
  om_cols <- names(dat)[grep("_om", names(dat))]
  em_gsub <- gsub("_em", "", em_cols)
  om_gsub <- gsub("_om", "", om_cols)
  em_cols <- em_cols[em_gsub %in% om_gsub]
  om_cols <- om_cols[om_gsub %in% em_gsub]
  em_names <- em_cols[order(em_cols)]
  om_names <- om_cols[order(om_cols)]

  re <- (dat[, em_names] - dat[, om_names]) /
    dat[, om_names]
  names(re) <- gsub("_em", "_re", names(re))

  # strip out NLL if these are scalar data:
  re <- re[, !grepl("NLL", names(re))]

  if (!add) {
    data.frame(dat[,-which(names(dat) %in%
      c(om_names, em_names))], re)
  } else {
    data.frame(dat, re)
  }
}
