#' Change catch in the datafile
#'
#' Change catch in the datafile so at least all combinations of fleet, seas, yr,
#' needed for catch are available
#' @param dat_list A data file as read in using `r4ss::SS_readdat()`
#' @param f_params A list of parameters related to fishing mortality
#' @return a modified data file as a list in R.
#' @author Kathryn Doering
#'
change_catch <- function(dat_list, f_params) {
  # Note: would need to modify if did not want catch every year, potentially.
  dat_list[["catch"]] <- expand.grid(
    year = c(-999, dat_list$styr:dat_list$endyr),
    seas = seq_len(dat_list$nseas), fleet = f_params$fleets, catch = 1, catch_se = 0.01)
  dat_list[["catch"]][dat_list[["catch"]][, "year"] == -999, "catch"] <- 0
  dat_list
}
