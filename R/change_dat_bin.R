#' Change the bins for a composition object
#'
#' Change the bins of a data frame object from a \code{dat_list},
#' filling in the columns with ones.
#'
#' @param object A data frame from a \code{dat_list} list. The leading columns,
#' typically \code{Yr}, \code{Seas}, ... can be anything, but there must be columns
#' with names that start with a lower-case letter followed by integers.
#' @param bins A vector of characters or whatever you want the names of the
#' new bins to be. Typically, this will be output from \code{\link{setup_bins}}.
#' @export
#' @return A modified data frame where columns holding old composition data are
#' removed in their entiretly and new columns of ones are filled for each value in
#' \code{bins}.
change_dat_bin <- function(object, bins) {
  alt <- setNames(rep(1, length(bins)), bins)
  out <- object %>% dplyr::select(-dplyr::matches("^[abflm][0-9]+$")) %>%
    tibble::add_column(!!! alt) %>% data.frame()
  return(out)
}
