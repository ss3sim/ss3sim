#' Sample the discard with observation error
#'
#' This function creates an index of discards sampled from the expected
#' available discards for specified fleets in specified years. Let \eqn{D_y} be
#' the discard from the operating model for year y. Then the sampled value is
#' calculated as: \eqn{D_y*exp(stats::rnorm(1, 0, sds_obs)-sds_obs^2/2)}. The
#' second term adjusts the random samples so that their expected value is
#' \eqn{D_y}, i.e., the log-normal bias correction.
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param sds_obs,month A list the same length as `fleets`. The list should
#'   contain either single values or numeric vectors, where the length of each
#'   vector matches the length of each vector present in `years`. Single values
#'   are repeated for all years. `sds_obs` represents the standard deviation of
#'   the observation error and month is the month of the observation.
#' @param seas A deprecated argument.
#'
#' @template sampling-return
#'
#' @export
#' @author Kelli F. Johnson
#' @family sampling functions

sample_discard <- function(dat_list,
                           outfile = NULL,
                           fleets,
                           years,
                           sds_obs,
                           seas = lifecycle::deprecated(),
                           month = list(1)) {
  if (lifecycle::is_present(seas)) {
    lifecycle::deprecate_warn(
      "1.20.1",
      "sample_discard(seas)",
      details = "Please use `month` rather than `seas`"
    )
    month <- seas
  }
  if (!is.list(dat_list) || is.null(dat_list[["discard_data"]])) {
    stop("dat_list must be a list object read in using r4ss::SS_readdat().")
  }
  ev <- dat_list$discard_data # expected values.
  colnames(ev) <- gsub("obs", "obsOLD", colnames(ev))
  Nfleets <- length(fleets)
  if (FALSE %in% (fleets %in% unique(ev$fleet))) {
    stop("The specified fleet numbers do not match input file")
  }
  if (Nfleets != 0 && !inherits(sds_obs, "list") || length(sds_obs) != Nfleets) {
    stop("sds_obs needs to be a list of same length as fleets")
  }
  if (Nfleets != 0 && !inherits(years, "list") || length(years) != Nfleets) {
    stop("years needs to be a list of same length as fleets")
  }
  for (i in 1:Nfleets) {
    if (length(sds_obs[[i]]) > 1 && length(sds_obs[[i]]) != length(years[[i]])) {
      stop(
        "Length of sds_obs does not match length of years for fleet ",
        fleets[i]
      )
    }
  }
  if (length(month) != length(fleets) && length(month) == 1) {
    month <- rep(list(month), length(fleets))
  }

  ## Start of sampling from the indices. Create a new data frame based on input
  ## arguments and use dplyr::mutate to apply sample_lognormal to each row
  ## based on input sd and observed values
  xxx <- merge(
    do.call(rbind, mapply(data.frame,
      SIMPLIFY = FALSE,
      year = years,
      month = standardize_sampling_args(fleets, years, other_input = month),
      fleet = lapply(fleets, c),
      stderr = standardize_sampling_args(fleets, years, other_input = sds_obs)
    )),
    ev[, c("year", "month", "fleet", "obsOLD")],
    sort = FALSE
  )
  if (NROW(xxx) == 0) {
    stop(
      "The following specified years, month, index combinations are not in dat_list:",
      "\nyears:\n", years, "\nmonth:\n", month, "\nindex:\n", fleets,
      "\nThus, these expected values are not available."
    )
  }
  new <- xxx |>
    dplyr::arrange(fleet, year, month) |>
    dplyr::rowwise() |>
    dplyr::mutate(obs = sample_lognormal(obsOLD, stderr)) |>
    dplyr::select(year:fleet, obs, stderr)

  ## Open the .dat file and find the right lines to overwrite
  dat_list$discard_data <- as.data.frame(new)
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
