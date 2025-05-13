#' Sample the CPUE/index data with observation error
#'
#' Create new catch-per-unit-effort (CPUE)/indices of abundance that are
#' based on the numbers in a data file. Typically the data file will be filled
#' with expected values rather than observed data but it does not have to be.
#' Sampling can only occur on fleets, years, and months that have current
#' observations. If rows of information are not sampled from, then they are
#' removed. So, you can take away rows of data but you cannot add them with
#' this function.
#'
#' @details
#' Limitations to the functionality of this function are as follows:
#' * you can only generate observations from rows of data that are present,
#'   e.g., you cannot make a new observation for a year that is not present in
#'   the passed data file;
#' * no warning will be given if some of the desired year, month, fleet
#'   combinations are available but not all, instead just the combinations
#'   that are available will be returned in the data list object; and
#' * sampling uses a log-normal distribution when the log-normal distribution
#'   is specified in `CPUEinfo[["errtype"]]` and a normal distribution for all
#'   other error types, see below for details on the log-normal sampling.
#'
#' Samples are generated using the following equation when the log-normal
#' distribution is specified:
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
#' @param dat_list  A Stock Synthesis data list returned from
#'   [r4ss::SS_readdat()]. Typically, this will be read from a file that
#'   contains expected values rather than input values but any Stock Synthesis
#'   data file is fine.
#' @param fleets An integer vector specifying which fleets to sample from. The
#'   order of the fleets matters here because you must retain the ordering for
#'   all of the remaining input arguments. For example, both `fleets = c(1, 2)`
#'   and `fleets = c(2, 1)` will work but `years` will expect the years you
#'   want sampled for fleet 2 to be in the second position in the list in the
#'   former and the first position in the latter case. So, if you change the
#'   order of your input, you will also have to modify all of the remaining
#'   arguments. An entry of `fleets = NULL` will lead to no CPUE samples in the
#'   returned object.
#' @param years A list the same length as `fleets` specifying the years you
#'   want samples from. There must be an integer vector in the list for every
#'   fleet specified in `fleets`. The function assumes that the information for
#'   the first fleet specified in `fleets` will be the first object in the list
#'   and so on so order matters here.
#' @param outfile A deprecated argument.
#' @param sds_obs,sds_out,month A list the same length as `fleets` specifying the
#'   standard deviation of the observation error used for the sampling; the
#'   standard deviation of the observation error you would like listed in the
#'   returned output, which might not always equal what was actually used for
#'   sampling; and the months you want to sample from. Each list element
#'   should contain a single numeric value or a vector, where vectors need to
#'   match the structure of `years` for the relevant fleet. If single values
#'   are passed, then, internally, they will be repeated for each year. If you
#'   want to repeat a single value for every year and fleet combination, then
#'   just pass it as a list with one entry, e.g., `month = list(1)` will sample
#'   from month one for all fleets and years --- this is the default for
#'   month. The default for `sds_obs` is 0.01 and if `sds_out` is missing,
#'   then `sds_obs` will be used for the output as well as the input.
#' @param seas A deprecated argument.
#' @return
#' A Stock Synthesis data file list object is returned. The object will be a
#' modified version of `dat_list`.
#' @family sampling functions
#'
#' @export
#' @author Cole Monnahan, Kotaro Ono
#' @examples
#' # Add a list from [r4ss::SS_readdat()] to your workspace, this is example
#' # data that is saved in the ss3sim package.
#' # Index data are saved in `dat_list[["CPUE"]]`
#' dat_list <- r4ss::SS_readdat(
#'   file = file.path(
#'     system.file("extdata", "example-om", package = "ss3sim"),
#'     "ss3_expected_values.dat"
#'   ),
#'   verbose = FALSE
#' )
#'
#' \dontshow{
#' set.seed(3)
#' }
#' # Sample from each available year from fleet 2 with an increasing trend in
#' # the observation error, i.e., the most recent year has the highest
#' # likelihood to be the furthest from the input data
#' ex1 <- sample_index(
#'   dat_list,
#'   fleets = 2,
#'   month = list(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "month"]
#'   ),
#'   years = list(dat_list[["CPUE"]][["year"]]),
#'   sds_obs = list(
#'     seq(0.001, 0.1, length.out = length(dat_list[["CPUE"]][["year"]]))
#'   )
#' )
#' \dontshow{
#' # Test to see if example 1 gives the values expected for 76 and 78
#' testthat::expect_equivalent(
#'   ex1[["CPUE"]][1:2, "obs"],
#'   c(1472202421, 1554321845)
#' )
#' }
#'
#' \dontrun{
#' # Sample from less years, note that sampling from more years than what is
#' # present in the data will not work
#' ex2 <- sample_index(dat_list,
#'   fleets = 2,
#'   month = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "month"]
#'   )),
#'   years = list(dat_list[["CPUE"]][["year"]][-c(1:2)]),
#'   sds_obs = list(0.001)
#' )
#'
#' # sd in the returned file can be different than what is used to sample, this
#' # is helpful when you want to test what would happen if the estimation method
#' # was improperly specified
#' ex3 <- sample_index(
#'   dat_list = dat_list,
#'   fleets = 2,
#'   month = list(unique(
#'     dat_list[["CPUE"]][dat_list[["CPUE"]][, "index"] == 2, "month"]
#'   )),
#'   years = list(dat_list[["CPUE"]][["year"]]),
#'   sds_obs = list(0.01),
#'   sds_out = list(0.20)
#' )
#' ex3[["CPUE"]][["se_log"]]
#' }
#' # Sample from two fleets after adding fake CPUE data for fleet 1
#' dat_list2 <- dat_list
#' dat_list2[["CPUE"]] <- rbind(
#'   dat_list[["CPUE"]],
#'   dat_list[["CPUE"]] |>
#'     dplyr::mutate(index = 1, month = 1)
#' )
#' dat_list2[["N_cpue"]] <- NROW(dat_list2[["CPUE"]])
#' ex4 <- sample_index(
#'   dat_list = dat_list2,
#'   fleets = 1:2,
#'   month = list(1, 7),
#'   # Subset two years from each fleet
#'   years = list(c(76, 78), c(80, 82)),
#'   # Use the same sd values for both fleets
#'   sds_obs = list(0.01),
#'   sds_out = list(0.20)
#' )
sample_index <- function(dat_list,
                         outfile = lifecycle::deprecated(),
                         fleets,
                         years,
                         sds_obs = list(0.01),
                         sds_out,
                         seas = lifecycle::deprecated(),
                         month = list(1)) {
  # Set up inputs
  Nfleets <- length(fleets)
  if (missing(sds_out)) {
    sds_out <- sds_obs
  }

  # Checks
  if (lifecycle::is_present(outfile)) {
    lifecycle::deprecate_warn(
      "1.20.1",
      "sample_index(outfile)",
      details = "Please save the returned output using `r4ss::SS_writedat()`"
    )
  }
  if (lifecycle::is_present(seas)) {
    lifecycle::deprecate_warn(
      "1.20.1",
      "sample_index(seas)",
      details = "Please use `month` rather than `seas`"
    )
    month <- seas
  }
  # Check that dat_list was read in using {r4ss}
  if (!inherits(dat_list, "list") || is.null(dat_list[["CPUE"]])) {
    cli::cli_abort(c(
      "{.var dat_list} must be an object returned from r4ss::SS_readdat().",
      i = "{.var dat_list} is a {class(dat_list)}.",
      i = "{.var dat_list$CPUE} contains {NROW(dat_list$CPUE)} rows."
    ))
  }
  # Check that sampling should occur, else exit early
  if (Nfleets == 0) {
    dat_list[["CPUE"]] <- dat_list[["CPUE"]][0, ]
    dat_list[["N_cpue"]] <- 0
    return(invisible(dat_list))
  }
  # Check that wanted fleets are present in CPUE data
  if (!all(fleets %in% dat_list[["CPUE"]][["index"]])) {
    missing_fleets <- fleets[!fleets %in% dat_list[["CPUE"]][["index"]]]
    cli::cli_abort(c(
      "All {.var fleets} must be found in {.var dat_list$cpue$index}",
      i = "{cli::qty(length(fleets))} You tried to sample from fleet{?s}
           {fleets}.",
      x = "{cli::qty(length(missing_fleets))} fleet{?s} {missing_fleets}
           {cli::qty(length(missing_fleets))} {?is/are} missing."
    ))
  }

  # Create a new data frame based on input arguments and use dplyr::mutate() to
  # apply sample_lognormal to each row based on input sd and observed values
  xxx <- merge(
    x = do.call(
      what = rbind,
      args = mapply(
        FUN = data.frame,
        SIMPLIFY = FALSE,
        year = years,
        month = standardize_sampling_args(fleets, years, other_input = month),
        index = lapply(fleets, c),
        se_in = standardize_sampling_args(fleets, years, other_input = sds_obs),
        se_log = standardize_sampling_args(fleets, years, other_input = sds_out)
      )
    ),
    y = dat_list[["CPUE"]] |>
      dplyr::rename(obsOLD = obs) |>
      dplyr::select(-se_log),
    sort = FALSE
  )
  if (NROW(xxx) == 0) {
    cli::cli_abort(c(
      "None of the desired fleets, years, months were in the CPUE data.",
      i = "Check your input values against {.var dat_list$CPUE}.",
      i = "sample_index() can't sample non-existent years, fleets, months."
    ))
  }

  dat_list[["CPUE"]] <- xxx |>
    dplyr::arrange(index, year, month) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      distribution = dat_list[["CPUEinfo"]][["errtype"]][
        match(index, dat_list[["CPUEinfo"]][["fleet"]])
      ],
      obs = ifelse(
        test = distribution == 0,
        yes = sample_lognormal(obsOLD, se_in),
        no = stats::rnorm(n = 1, mean = obsOLD, sd = se_in)
      )
    ) |>
    dplyr::select(year:index, obs, se_log) |>
    as.data.frame()

  dat_list[["N_cpue"]] <- ifelse(Nfleets > 0, nrow(dat_list[["CPUE"]]), 0)

  return(invisible(dat_list))
}
