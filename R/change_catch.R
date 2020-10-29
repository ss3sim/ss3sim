#' Change catch in the datafile.
#'
#' @param dat_list A data file as read in using `r4ss::SS_readdat()`
#' @param f_params A list of parameters related to fishing mortality
#' @return a modified data file as a list in R.
#' @author Kathryn Doering
#'
change_catch <- function(dat_list, f_params) {
  # Note: would need to modify if did not want catch every year, potentially.
  #if no seasons provided, assume seasons for each fleet.
  if(is.null(f_params[["seas"]])) {
    nseas <- dat_list$nseas
    f_params$seas <- lapply(f_params$fleets, function(x, n) seq_len(n),
                            n = nseas)
  }
  dat_list[["catch"]] <- NULL
  for(f in seq_len(f_params$fleets)) {
    tmp_catch <- expand.grid(
      year = c(-999, dat_list$styr:dat_list$endyr),
      seas = f_params$seas[[f]], fleet = f_params$fleets[f], catch = 1,
      catch_se = 0.01)
    dat_list[["catch"]] <- rbind(dat_list[["catch"]], tmp_catch)
  }
  dat_list[["catch"]][dat_list[["catch"]][, "year"] == -999, "catch"] <- 0
  dat_list
}
