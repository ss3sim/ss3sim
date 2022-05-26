#' Change the data that is available from a list object
#'
#' Alter the structure of data that is available from a Stock Synthesis
#' operating model (OM), which in turn leads to changes
#' in the output and ability to sample data after running the model.
#'
#' @template dat_list
#' @template outfile
#' @param fleets A numeric vector of fleets.
#' @param years A numeric vector of years.
#' @param types A vector that can take combinations of the following entries:
#'   `"len"`, `"age"`, `"cal"`, `"mla"`.
#'   `types` controls what data structures the function acts on, with
#'   `"len"` augmenting the length-composition data,
#'   `"age"` augmenting the age-composition data,
#'   `"cal"` augmenting the conditional age-at-length data, and
#'   `"mla"` augmenting the mean length-at-age data.
#' @param age_bins A numeric vector of age bins to use. If left as `NULL`,
#'   the age bin structure will be taken from the OM.
#' @param len_bins A numeric vector of length bins to use. If left as
#'   `NULL`, the length bin structure will be taken from the OM.
#'   For conditional age-at-length data,
#'   the last value provided to `len_bins` will be used for `Lbin_lo` and
#'   -1 will be used for `Lbin_hi` for the largest length bin category, i.e.,
#'   row of conditional age-at-length data.
#' @param pop_binwidth Population length bin width. Note that this value must
#'   be smaller than the bin width specified in length-composition data
#'   `len_bins` or Stock Synthesis will fail,
#'   see notes in the
#'   [Stock Synthesis manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual.html).
#' @param pop_minimum_size Population minimum length bin value.
#' @param pop_maximum_size Population maximum length bin value.
#' @param lcomp_constant The robustification constant for length-composition data.
#'   Must be a numeric value, as a proportion.
#'   For example, 0.1 means 10 percent.
#'   See the notes in the
#'   [Stock Synthesis manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual.html).
#'   A `NULL` value indicates no action resulting in using the current value, and
#'   a value of 0 will throw an error because
#'   zero leads to an error when zeroes exist in the data.
#'   Instead use a very small value like 1e-07.
#' @param tail_compression Tail compression value to be used in Stock Synthesis. Must
#'   be a numeric value, as a proportion. For example 0.1 means 10 percent.
#'   See the notes in the
#'   [Stock Synthesis manual](https://nmfs-stock-synthesis.github.io/doc/SS330_User_Manual.html).
#'   A `NULL` value indicates no action,
#'   a negative value turns the feature off in Stock Synthesis.
#' @template nsex
#'
#' @details
#' `change_data()` is called internally within ss3sim, but it can be used
#' to manipulate data or to prepare a new OM for use in a simulation.
#' Original data is removed and dummy data is added to the Stock Synthesis `.dat` object.
#' The dummy data expands the data structure to provide information for all years
#' and fleets, potentially adding many rows of data.
#'
#' Currently, `.dat` files with multiple sexes cannot be manipulated with `change_data()`.
#'
#' The robustification constant is added to both the observed and
#' expected proportions of length composition data, before being normalized
#' internally. It is designed to help stabilize the model, but is unclear how
#' and when to use it for optimal effect. The same value is used for all
#' length data.
#'
#' @return An invisible data list, and a file is written to the disk if an
#' entry other than the default of `NULL` is provided for `outfile`.
#' @family change functions
#'
#' @export
#' @seealso See [clean_data()] for a counter function.
#' @author Cole C. Monnahan, Ian G. Taylor, Sean C. Anderson, Kelli F. Johnson
#'
change_data <- function(dat_list, outfile = NULL, fleets, years,
                        types = c("len", "age", "cal", "mla", "mwa"),
                        age_bins = NULL, len_bins = NULL, pop_binwidth = NULL,
                        pop_minimum_size = NULL, pop_maximum_size = NULL,
                        lcomp_constant = NULL, tail_compression = NULL,
                        nsex = 1) {

  # TODO: pop length bins must not be wider than the length data bins, but the
  # boundaries of the bins do not need to align (from Stock Synthesis manual)
  # this is also checked within Stock Synthesis and will create a fatal error

  check_data(dat_list)
  ## Input checks:
  types <- match.arg(types, several.ok = TRUE)
  if ((!is.atomic(fleets)) | (!is.atomic(years))) {
    stop("fleets and years input both need to be numeric vectors")
  }
  if (any(years < dat_list$styr) | any(years > dat_list$endyr)) {
    stop("Some years specified in years are not within the model years of dat_list")
  }
  if (any(!fleets %in% 1:dat_list$Nfleets)) {
    stop("Some fleets specified in fleets are not included in dat_list")
  }

  ## TODO: Need to do things like change age matrices?
  ## TODO: Change the data vectors if specified?

  # population bins:
  # change_pop_bin() deals with NULLs internally by not changing values:
  dat_list <- change_pop_bin(dat_list,
    binwidth = pop_binwidth,
    minimum_size = pop_minimum_size,
    maximum_size = pop_maximum_size
  )

  if (is.null(len_bins)) len_bins <- dat_list$lbin_vector
  if (is.null(age_bins)) age_bins <- dat_list$agebin_vector

  ## Now modify each data type in turn
  if ("len" %in% types) {
    dat_list$lencomp <- make_dummy_dat_lencomp(
      fleets = fleets, years = years,
      len_bins = len_bins, nsex = nsex
    )
    dat_list$lbin_vector <- len_bins
    dat_list$N_lencomp <- nrow(dat_list$lencomp)
    dat_list$N_lbins <- length(len_bins)
  }
  ## Need to split calcomp and agecomp data as separate cases
  if ("age" %in% types) {
    conditional_data <- dat_list$agecomp[dat_list$agecomp$Lbin_lo >= 0, ]
    new.agecomp <- make_dummy_dat_agecomp(
      fleets = fleets, years = years,
      age_bins = age_bins, nsex = nsex
    )
    dat_list$agecomp <- rbind(new.agecomp, conditional_data)
    dat_list$agebin_vector <- age_bins
    dat_list$N_agecomp <- nrow(dat_list$agecomp)
    dat_list$N_agebins <- length(age_bins)
  }
  ## If we don't use this structure to get expected value we don't need
  ## to print them, so don't need it. TODO check this is right. It can
  ## seriously slow down the OM to write uncessary calcomp data.
  if ("cal" %in% types) {
    agecomp <- dat_list$agecomp[dat_list$agecomp$Lbin_lo %in% c(-1, 0) &
      dat_list$agecomp$Lbin_hi %in% c(-1, 0), ]
    if (!is.null(agecomp) && any(agecomp$Lbin_lo == 0 | agecomp$Lbin_hi == 0)) {
      warning(
        "Some regular age comp data (i.e., not conditional on length) ",
        "had Lbin_lo and/or Lbin_high values as 0. It is safer to have ",
        " all these values as -1 according to the Stock Synthesis user ",
        "manual, so changing all to -1."
      )
      age_comp$Lbin_lo <- -1
      age_comp$Lbin_hi <- -1
    }

    new.calcomp <- make_dummy_dat_calcomp(
      fleets = fleets, years = years,
      age_bins = age_bins, len_bins = len_bins, nsex = nsex
    )
    dat_list$agecomp <- rbind(agecomp, new.calcomp)
    dat_list$agebin_vector <- age_bins
    dat_list$N_agecomp <- nrow(dat_list$agecomp)
  }

  if ("mla" %in% types) {
    dat_list$MeanSize_at_Age_obs <- make_dummy_dat_mlacomp(
      fleets = fleets,
      years = years, age_bins = age_bins
    )
    dat_list$N_MeanSize_at_Age_obs <- nrow(dat_list$MeanSize_at_Age_obs)
  }

  if (!is.null(lcomp_constant)) {
    dat_list <- change_lcomp_constant(
      lcomp_constant = lcomp_constant,
      dat_list = dat_list, outfile = NULL
    )
  }
  if (!is.null(tail_compression)) {
    dat_list <- change_tail_compression(
      tail_compression = tail_compression,
      dat_list = dat_list, outfile = NULL
    )
  }

  if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list, outfile = outfile,
      overwrite = TRUE, verbose = FALSE
    )
  }
  invisible(dat_list)
}

#' Calculate fleets, years, and data types needed given sampling parameters
#'
#' Given the sampling arguments that are specified in `..._params`, e.g.,
#' `index_params`, calculate the super set of fleets, years, and data
#' types that will be needed in the data file of expected values that is
#' generated by the OM.
#'
#' @author Cole C. Monnahan
#' @param index_params Named lists containing the arguments for
#'   [sample_index()].
#' @param lcomp_params Named lists containing the arguments for
#'   [sample_lcomp()].
#' @param agecomp_params Named lists containing the arguments for
#'   [sample_agecomp()].
#' @param calcomp_params Named lists containing the arguments for
#'   [sample_calcomp()].
#' @param mlacomp_params Named lists containing the arguments for
#'   [sample_mlacomp()].
#' @param wtatage_params Named lists containing the arguments for
#'   [sample_wtatage()].
#' @seealso See further examples in [clean_data] and [change_data]
#' @note A superset by nature is larger than the individual sets used to
#' create it (unless all sampling arguments are identical), so that the
#' returned list will created some unnecessary combinations. This was done
#' intentionally for simplicity but may be changed later.
#' @return A list with the following three elements:
#'  * fleets,
#'  * years, and
#'  * types.
#' @examples
#' ## Only one fleet
#' calculate_data_units(lcomp_params = list(fleets = 1, years = c(3, 4, 6)))
#' ## Add new fleet
#' morefleets <- calculate_data_units(
#'   lcomp_params = list(fleets = 1, years = c(3, 4, 6)),
#'   agecomp_params = list(fleets = 2, years = 5)
#' )
#' \dontshow{
#' testthat::expect_equal(morefleets[["years"]], 3:6)
#' }
#' ## Add length or age if missing and conditional-age-at-length is included
#' test <- mapply(calculate_data_units,
#'   SIMPLIFY = FALSE,
#'   lcomp_params = list(NULL, list(fleets = 1, years = 1:10)),
#'   agecomp_params = list(NULL, NULL),
#'   MoreArgs = list(calcomp_params = list(fleets = 1, years = 1:10))
#' )
#' \dontshow{
#' testthat::expect_equal(sort(test[[1]][["types"]]), c("age", "cal", "len"))
#' testthat::expect_equal(sort(test[[2]][["types"]]), c("age", "cal", "len"))
#' }
#' rm(test)
#' @export
#'
calculate_data_units <- function(index_params = NULL, lcomp_params = NULL,
                                 agecomp_params = NULL, calcomp_params = NULL,
                                 mlacomp_params = NULL, wtatage_params = NULL) {
  sample_args <- list(
    index = index_params,
    len = lcomp_params,
    age = agecomp_params,
    cal = calcomp_params,
    mla = mlacomp_params,
    wtatage = wtatage_params
  )
  sample_args_null <- vapply(
    X = sample_args,
    # FUN = "[[", "fleets", # benchmarked slower than unnamed fxn
    FUN = function(i) is.null(i$fleets),
    FUN.VALUE = logical(1L)
  )
  ## Exit if nothing specified to prevent error.
  if (!any(!sample_args_null)) {
    stop("No data passed: all arguments NULL")
  }
  ## Get the sorted superset of fleets and years
  fleets <- sort(unique(unlist(lapply(sample_args, function(x) x$fleets))))
  years <- sort(unique(unlist(lapply(sample_args, function(x) x$years))))

  ## Now figure out which data types need to be in the OM for sampling (but
  ## not necessarily the EM).
  types <- names(sample_args)[!sample_args_null]
  if ("cal" %in% types) {
    types <- c(types, "len", "age")
  }
  if ("wtatage" %in% types) {
    types <- c(types, "age", "mla")
  }
  if ("mla" %in% types) {
    types <- c(types, "age")
  }

  return(list(
    fleets = fleets,
    years = years,
    types = unique(types)
  ))
}

#' Set up population length bin structure
#'
#' The population length bins in Stock Synthesis structure size data and
#' empirical weight-at-age data.
#' `change_pop_bin` changes the data file to contain
#' specifications to create a vector (length-bin method of 2) rather than
#' the actual bins from the length data (length-bin method of 1) or
#' an actual vector (length-bin method of 3).
#'
#' The only required argument is `dat_list` and the remaining arguments
#' default to a value of `NULL`, which leads to the data file not being
#' changed.
#' @template dat_list
#' @param binwidth A numeric value specifying the width of the size bins.
#' @param minimum_size The smallest size bin.
#' @param maximum_size The largest size bin.
#' @param maximum_age The highest age. Used to structure the maximum age of
#' the population and the ageing-error matrix, which will be assumed
#' to have no bias and maximum precision for any added ages.
#' @return A modified Stock Synthesis data file in list form. The list
#' is only returned if it is assigned to an object.
#'
change_pop_bin <- function(dat_list, binwidth = NULL, minimum_size = NULL,
                           maximum_size = NULL, maximum_age = NULL) {
  if (length(binwidth) > 1 | length(minimum_size) > 1 |
    length(maximum_size) > 1) {
    warning(
      "Some inputs to function had length > 1. Using first value of ",
      "vector input only."
    )
  }
  dat_list$lbin_method <- 2
  if (!is.null(binwidth)) dat_list$binwidth <- binwidth[1]
  if (!is.null(minimum_size)) dat_list$minimum_size <- minimum_size[1]
  if (!is.null(maximum_size)) dat_list$maximum_size <- maximum_size[1]
  # according to Stock Synthesis manual 3.30.14,
  # this is how the number of bins is calculated.
  nlbin_pop <- (dat_list$maximum_size - dat_list$minimum_size) / dat_list$binwidth + 1
  dat_list$lbin_vector_pop <- seq(dat_list$minimum_size, dat_list$maximum_size,
    length.out = nlbin_pop
  )
  if (!is.null(maximum_age)) {
    if (dat_list$N_ageerror_definitions > 0) {
      if (NCOL(dat_list$ageerror) < (maximum_age + 1)) {
        dat_list$ageerror[, NCOL(dat_list$ageerror):maximum_age + 1] <-
          rep(c(-1, 0), dat_list$N_ageerror_definitions)
      } else {
        dat_list$ageerror <- dat_list$ageerror[, 1:(maximum_age + 1)]
      }
    }
    dat_list$Nages <- maximum_age
  }

  invisible(dat_list)
}

#' Check that the Stock Synthesis data file looks correct
#'
#' @param x A Stock Synthesis data list object as read in by [r4ss::SS_readdat()].
#' @export

check_data <- function(x) {
  if (!is.list(x)) {
    stop("data file isn't a list; should be output from r4ss::SS_readdat()")
  }

  if (is.null(x$type)) {
    stop(
      "dat_list must be an r4ss data file read into R using ",
      "r4ss::SSreaddat()"
    )
  }

  if (x$type != "Stock_Synthesis_data_file") {
    stop(
      "dat_list must be an r4ss data file read into R using ",
      "r4ss::SSreaddat()"
    )
  }

  if (!is.null(x$lbin_method)) {
    if (x$lbin_method > 2) {
      stop("lbin_method in the SS data file should be either 1 or 2")
    }
  }

  if (!identical(x$Lbin_method, 3) & !is.null(x$lencomp)) {
    stop(
      "Lbin_method must be 3 to specify how the conditional",
      "\nage-at-length data are represented in the SS data file.",
      "\nSee the Stock Synthesis manual."
    )
  }

  if (!identical(x$N_areas, 1)) {
    stop("N_areas in the Stock Synthesis data file must be set to 1.")
  }

  if (!identical(x$areas, rep(1, x$Nfleets))) {
    stop(
      "_area_assignments_for_each_fishery_and_survey must be set to 1",
      " for all fleets in the Stock Synthesis data file."
    )
  }
}
