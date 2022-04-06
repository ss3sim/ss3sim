#' Sample the catches with observation error
#'
#' This function creates a matrix of sampled catches from the expected
#' available catches for all fleets with catches.
#' The input value used for `catch_se` will be used to resample the catches.
#' There is a bit of a disconnect here because catches are defined by input `F`
#' values not absolute catches.
#' Let \eqn{D_y} be the discard
#' from the operating model for year y. Then the sampled value is calculated as:
#' \eqn{D_y*exp(stats::rnorm(1, 0, sds_obs)-sds_obs^2/2)}. The second term
#' adjusts the random samples so that their expected value is \eqn{D_y}, i.e.,
#' the log-normal bias correction.
#'
#' @template dat_list
#' @template outfile
#'
#' @template sampling-return
#'
#' @export
#' @author Kelli F. Johnson
#' @family sampling functions

sample_catch <- function(dat_list,
                         outfile = NULL) {
  if (!is.list(dat_list) | is.null(dat_list[["catch"]])) {
    stop("dat_list must be a list object read in using r4ss::SS_readdat().")
  }
  ev <- dat_list$catch # expected values.
  colnames(ev) <- gsub("catch$", "obsOLD", colnames(ev))

  new <- ev %>%
    dplyr::arrange(.data[["fleet"]], .data[["year"]], .data[["seas"]]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(catch = sample_lognormal(.data[["obsOLD"]], .data[["catch_se"]])) %>%
    dplyr::select(.data[["year"]]:.data[["fleet"]], .data[["catch"]], .data[["catch_se"]])

  ## Open the .dat file and find the right lines to overwrite
  dat_list$catch <- as.data.frame(new)
  if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }

  invisible(dat_list)
}
