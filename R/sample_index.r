#' Sample the biomass with observation error
#'
#' Create indices of abundance sampled from the expected available biomass for
#' specified fleets in specified years.
#'
#' @details
#' Samples are generated using the following equation:
#' \deqn{
#'   B_y*exp(stats::rnorm(1, 0, sds_obs)-sds_obs^2/2)
#' },
#' where \eqn{B_y} is the expected biomass in year y and \eqn{sds_obs} is the
#' standard deviation of the normally distributed biomass or the standard error
#' of the \eqn{log_e(B_y)}. For the error term, this is the same
#' parameterization that is used in Stock Synthesis. More details can be found
#' in the [section on indices in the Stock Synthesis
#' manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual_release.html#indices)
#' The second term in the equation adjusts the random samples so their expected
#' value is \eqn{B_y}, i.e., the log-normal bias correction.
#'
#' If you only know the coefficient of variation (\eqn{CV}), then the input
#' error can be approximated using \eqn{\sqrt{log_e(1+CV^{2})}}. Where,
#' \eqn{CV} is assumed to be constant with mean changes in biomass. The
#' log-normal distribution can be approximated by a proportional distribution
#' or normal distribution only when the variance is low, i.e., \eqn{CV < 0.50}
#' or log standard deviation of 0.22.
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param sds_obs,sds_out A list the same length as `fleets` specifying the
#'   standard deviation of the observation error used for the sampling and the
#'   value used in the returned output. Thus, `sds_obs` is what is actually
#'   used to sample and `sds_out` can be used to test what happens when the
#'   input values to your model are biased. If `sds_out` is missing, then
#'   `sds_obs` will be used for the output as well as the input. List elements
#'   should be either single numeric values or numeric vectors the same length
#'   as the number of years sampled for each given fleet. Single values are
#'   repeated for all years. See details for more information, particularly for
#'   the equations.
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
#' # dat_list[["CPUE"]]
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
#' ex1[["CPUE"]]
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
#' ex2[["CPUE"]]
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
#' ex3[["CPUE"]]
#' # If fleet 1 also had expected values in the index that you wanted to sample:
#' testthat::expect_error(
#'   ex4 <- sample_index(dat_list,
#'     outfile = NULL,
#'     fleets = c(1, 2),
#'     years = list(sam_yrs, sam_yrs),
#'     sds_obs = list(0.01, 0.01)
#'   )
#' )
#' # sd in the returned file can be different than what is used to sample:
#' ex5 <- sample_index(dat_list,
#'   outfile = NULL,
#'   fleets = 2,
#'   seas = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "seas"]
#'   )),
#'   years = list(sam_yrs),
#'   sds_obs = list(0.01),
#'   sds_out = list(0.20)
#' )
#' ex5[["CPUE"]]
#' testthat::expect_true(all(ex5[["CPUE"]][["se_log"]] == 0.2))
#' }
#' @family sampling functions

sample_index <- function(dat_list,
                         outfile = NULL,
                         fleets,
                         years,
                         sds_obs,
                         sds_out,
                         seas = list(1)) {
  # Set up inputs
  cpue <- dat_list[["CPUE"]]
  colnames(cpue) <- gsub("obs", "obsOLD", colnames(cpue))
  Nfleets <- length(fleets)
  if (missing(sds_out)) {
    sds_out <- sds_obs
  }

  # Checks
  # Check that sampling should occur, else exit early
  if (Nfleets == 0) {
    dat_list[["CPUE"]] <- dat_list[["CPUE"]][0, ]
    dat_list[["N_cpue"]] <- 0
    if (!is.null(outfile)) {
      r4ss::SS_writedat(
        datlist = dat_list,
        outfile = outfile,
        overwrite = TRUE,
        verbose = FALSE
      )
    }
    return(invisible(dat_list))
  }
  # Check that dat_list was read in using {r4ss}
  if (!inherits(dat_list, "list") || is.null(dat_list[["CPUE"]])) {
    cli::cli_abort(c(
      "{.var dat_list} must be an object returned from r4ss::SS_readdat().",
      i = "{.var dat_list} is a {class(dat_list)}.",
      i = "{.var dat_list$cpue} contains {NROW(cpue)} rows."
    ))
  }
  # Check that wanted fleets are present in CPUE data
  if (!all(fleets %in% cpue[["index"]])) {
    missing_fleets <- fleets[!fleets %in% cpue[["index"]]]
    cli::cli_abort(c(
      "All {.var fleets} must be found in {.var dat_list$cpue$index}",
      i = "You tried to sample from fleets {fleets}.",
      x = paste(
        "{cli::qty(length(missing_fleets))} Fleet{?s} {missing_fleets}",
        "{cli::qty(length(missing_fleets))} {?is/are} missing."
      )
    ))
  }

  # Start of sampling from the indices. Create a new data frame based on input
  # arguments and use dplyr::mutate() to apply sample_lognormal to each row
  # based on input sd and observed values
  xxx <- merge(
    do.call(rbind, mapply(data.frame,
      SIMPLIFY = FALSE,
      year = years,
      seas = standardize_sampling_args(fleets, years, other_input = seas),
      index = lapply(fleets, c),
      se_in = standardize_sampling_args(fleets, years, other_input = sds_obs),
      se_log = standardize_sampling_args(fleets, years, other_input = sds_out)
    )),
    cpue[, c("year", "seas", "index", "obsOLD")],
    sort = FALSE
  )
  if (NROW(xxx) == 0) {
    cli::cli_abort(c(
      "None of the desired fleets, years, seasons were in the CPUE data.",
      i = "Check your input values against {.var dat_list$CPUE}.",
      i = "sample_index() can't sample non-existent years, fleets, seasons."
    ))
  }
  cpue.new <- xxx %>%
    dplyr::arrange(.data[["index"]], .data[["year"]], .data[["seas"]]) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      dist = dat_list[["CPUEinfo"]][["Errtype"]][
        match(.data[["index"]], dat_list[["CPUEinfo"]][["Fleet"]])
      ],
      obs = ifelse(
        test = .data[["dist"]] == 0,
        yes = sample_lognormal(.data[["obsOLD"]], .data[["se_in"]]),
        no = stats::rnorm(n = 1, mean = .data[["obsOLD"]], .data[["se_in"]])
      )
    ) %>%
    dplyr::select(.data[["year"]]:.data[["index"]], .data[["obs"]], .data[["se_log"]])

  # Overwrite .dat file
  dat_list$CPUE <- as.data.frame(cpue.new)
  dat_list$N_cpue <- ifelse(Nfleets > 0, nrow(cpue.new), 0)
  if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }

  return(invisible(dat_list))
}
