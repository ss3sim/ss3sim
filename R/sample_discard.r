#' Sample the discard with observation error
#'
#' This function creates an index of discards sampled from the expected
#' available discards for specified fleets in specified years. Let \eqn{D_y} be the discard
#' from the operating model for year y. Then the sampled value is calculated as:
#' \eqn{D_y*exp(stats::rnorm(1, 0, sds_obs)-sds_obs^2/2)}. The second term
#' adjusts the random samples so that their expected value is \eqn{D_y}, i.e.,
#' the log-normal bias correction.
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param sds_obs A list the same length as `fleets`. The list should
#'   contain either single values or numeric vectors of the same length as the
#'   number of years which represent the standard deviation of the observation
#'   error. Single values are repeated for all years.
#' @template seas
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
                           seas = list(1)) {
  if (!is.list(dat_list) | is.null(dat_list[["discard_data"]])) {
    stop("dat_list must be a list object read in using r4ss::SS_readdat().")
  }
  ev <- dat_list$discard_data # expected values.
  colnames(ev) <- gsub("Discard", "obsOLD", colnames(ev))
  Nfleets <- length(fleets)
  if (FALSE %in% (fleets %in% unique(ev$Flt))) {
    stop("The specified fleet numbers do not match input file")
  }
  if (Nfleets != 0 & class(sds_obs) != "list" | length(sds_obs) != Nfleets) {
    stop("sds_obs needs to be a list of same length as fleets")
  }
  if (Nfleets != 0 & class(years) != "list" | length(years) != Nfleets) {
    stop("years needs to be a list of same length as fleets")
  }
  for (i in 1:Nfleets) {
    if (length(sds_obs[[i]]) > 1 & length(sds_obs[[i]]) != length(years[[i]])) {
      stop(
        "Length of sds_obs does not match length of years for fleet ",
        fleets[i]
      )
    }
  }
  if (length(seas) != length(fleets) & length(seas) == 1) {
    seas <- rep(list(seas), length(fleets))
  }

  ## Start of sampling from the indices. Create a new data frame based on input
  ## arguments and use dplyr::mutate to apply sample_lognormal to each row
  ## based on input sd and observed values
  xxx <- merge(
    do.call(rbind, mapply(data.frame,
      SIMPLIFY = FALSE,
      Yr = years,
      Seas = standardize_sampling_args(fleets, years, other_input = seas),
      Flt = lapply(fleets, c),
      Std_in = standardize_sampling_args(fleets, years, other_input = sds_obs)
    )),
    ev[, c("Yr", "Seas", "Flt", "obsOLD")],
    sort = FALSE
  )
  if (NROW(xxx) == 0) {
    stop(
      "The following specified years, seas, index combinations are not in dat_list:",
      "\nyears:\n", years, "\nseas:\n", seas, "\nindex:\n", fleets,
      "\nThus, these expected values are not available."
    )
  }
  new <- xxx %>%
    dplyr::arrange(.data[["Flt"]], .data[["Yr"]], .data[["Seas"]]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Discard = sample_lognormal(.data[["obsOLD"]], .data[["Std_in"]])) %>%
    dplyr::select(.data[["Yr"]]:.data[["Flt"]], .data[["Discard"]], .data[["Std_in"]])

  ## Open the data file and find the right lines to overwrite
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
