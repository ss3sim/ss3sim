#' Sample the biomass with observation error
#'
#' Create an index of abundance sampled from the expected
#' available biomass for specified fleets in specified years.
#'
#' @details
#' Samples are generated using the following equation:
#' \deqn{
#'   B_y*exp(stats::rnorm(1, 0, sds_obs)-sds_obs^2/2)
#' },
#' where \eqn{B_y} is the expected biomass in year y and
#' \eqn{sds_obs} is the standard deviation of the normal distribution or
#' the standard error of the \eqn{log_e(B_y)}.
#' For the error term, this is the same
#' parameterization that is used in Stock Synthesis.
#' More details can be found in the
#' [section on indices in the Stock Synthesis manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual_release.html#indices)
#' The second term in the equation adjusts the random samples so their expected
#' value is \eqn{B_y}, i.e., the log-normal bias correction.
#'
#' If you only know the coefficient of variation (\eqn{CV}), then
#' the input error can be approximated using \eqn{\sqrt{log_e(1+CV^{2})}}.
#' Where, \eqn{CV} is assumed to be constant with mean changes in biomass.
#' The lognormal distribution can be approximated by a proportional
#' distribution or normal distribution only when the variance is low, i.e.,
#' \eqn{CV < 0.50} or log standard deviation of 0.22.
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param sds_obs A list the same length as `fleets` specifying the
#'   standard deviation of the observation error.
#'   List elements should be
#'   either single numeric values or numeric vectors the same length as the
#'   number of years sampled for each given fleet.
#'   Single values are repeated for all years.
#'   See details for more information, particularly the equations.
#' @param make_plot Deprecated with ss3sim version 1.1.5.
#'   A logical switch for whether to make a crude plot showing
#'   the results. Useful for testing and exploring the function.
#' @template seas
#'
#' @template sampling-return
#'
#' @export
#' @author Cole Monnahan, Kotaro Ono
#' @examples
#' # Find the example data location:
#' set.seed(3)
#' dat_list <- r4ss::SS_readdat(
#'   file = file.path(
#'     system.file("extdata", "example-om", package = "ss3sim"),
#'     "ss3_expected_values.dat"
#'   ),
#'   verbose = FALSE
#' )
#' # Look at expected values for the index data
#' # fleet 2, every other year from 76 to 100
#' # dat_list$CPUE
#' sam_yrs <- seq(76, 100, by = 2)
#' ex1 <- sample_index(dat_list,
#'   outfile = NULL,
#'   fleets = 2,
#'   seas = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "seas"]
#'   )),
#'   years = list(sam_yrs),
#'   sds_obs = list(seq(0.001, 0.1, length.out = length(sam_yrs)))
#' )
#' \dontshow{
#' testthat::expect_equivalent(
#'   ex1[["CPUE"]][1:2, "obs"],
#'   c(1472202421, 1554321845)
#' )
#' }
#' \dontrun{
#' ex1$CPUE
#' # could sample from less years, but not more:
#' ex2 <- sample_index(dat_list,
#'   outfile = NULL,
#'   fleets = 2,
#'   seas = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "seas"]
#'   )),
#'   years = list(sam_yrs[c(-1, -2)]),
#'   sds_obs = list(seq(0.001, 0.1, length.out = length(sam_yrs) - 2))
#' )
#' ex2$CPUE
#' # sd can be fixed across years:
#' ex3 <- sample_index(dat_list,
#'   outfile = NULL,
#'   fleets = 2,
#'   seas = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "seas"]
#'   )),
#'   years = list(sam_yrs),
#'   sds_obs = list(0.01)
#' )
#' ex3$CPUE
#' # If fleet 1 also had expected values in the index that you wanted to sample:
#' testthat::expect_error(
#'   ex4 <- sample_index(dat_list,
#'     outfile = NULL,
#'     fleets = c(1, 2),
#'     years = list(sam_yrs, sam_yrs),
#'     sds_obs = list(0.01, 0.01)
#'   )
#' )
#' }
#' @family sampling functions

sample_index <- function(dat_list,
                         outfile = NULL,
                         fleets,
                         years,
                         sds_obs,
                         make_plot = lifecycle::deprecated(),
                         seas = list(1)) {

  ## Check inputs for errors
  if (lifecycle::is_present(make_plot)) {
    lifecycle::deprecate_warn(
      when = "1.1.5",
      what = "ss3sim::sample_index(make_plot = )"
    )
  }
  if (!is.list(dat_list) | is.null(dat_list[["CPUE"]])) {
    stop("dat_list must be a list object read in using r4ss::SS_readdat().")
  }
  cpue <- dat_list$CPUE # CPUE expected values.
  colnames(cpue) <- gsub("obs", "obsOLD", colnames(cpue))
  Nfleets <- length(fleets)
  if (FALSE %in% (fleets %in% unique(cpue$index))) {
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
      year = years,
      seas = standardize_sampling_args(fleets, years, other_input = seas),
      index = lapply(fleets, c),
      se_log = standardize_sampling_args(fleets, years, other_input = sds_obs)
    )),
    cpue[, c("year", "seas", "index", "obsOLD")],
    sort = FALSE
  )
  if (NROW(xxx) == 0) {
    stop(
      "The following specified years, seas, index combinations are not in dat_list:",
      "\nyears:\n", years, "\nseas:\n", seas, "\nindex:\n", fleets,
      "\nThus, these expected values are not available."
    )
  }
  cpue.new <- xxx %>%
    dplyr::arrange(.data[["index"]], .data[["year"]], .data[["seas"]]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(obs = sample_lognormal(.data[["obsOLD"]], .data[["se_log"]])) %>%
    dplyr::select(.data[["year"]]:.data[["index"]], .data[["obs"]], .data[["se_log"]])

  ## Open the .dat file and find the right lines to overwrite
  dat_list$CPUE <- as.data.frame(cpue.new)
  dat_list$N_cpue <- ifelse(Nfleets > 0, nrow(cpue.new), )
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
