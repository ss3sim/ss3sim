#' Change the bins for a composition object
#'
#' Change the bins of a data frame object from a `dat_list`,
#' filling in the columns with ones.
#'
#' @param object A data frame from a list object read in by [r4ss::SS_readdat()].
#'   The leading columns, typically `year`, `month`, ... can be anything,
#'   but there must be columns with names that start with a lower-case letter,
#'   followed by integers.
#' @param bins A vector of characters or whatever you want the names of the
#' new bins to be. Typically, this will be output from [setup_bins()].
#' @export
#' @return A modified data frame where columns holding old composition data are
#' removed in their entirety and new columns of ones are filled for each value in
#' `bins`.
change_dat_bin <- function(object, bins) {
  alt <- stats::setNames(rep(1, length(bins)), bins)
  out <- object |>
    dplyr::select(-dplyr::matches("^[abflm][0-9]+$")) |>
    dplyr::select(-dplyr::matches("^Ess", ignore.case = TRUE)) |>
    tibble::add_column(!!!alt) |>
    data.frame()
  return(out)
}
