#' Sample composition data from expected values
#'
#' Apply the multinomial or Dirichlet distribution to sample
#' composition data, creating a data frame that mimics
#' observed composition data.
#'
#' @details
#' Sample size, i.e., 'Nsamp', is used as a measure of precision,
#' where higher sample sizes lead to simulated samples that more accurately
#' represent the truth provided in `data`.
#'
#' @param data A data frame with informational columns followed by
#' columns of compositional data.
#' The informational columns must include columns labeled
#' 'year' and 'fleet' and end with a column labeled 'Nsamp'.
#' Columns of compositional data should follow 'Nsamp'.
#' Rows of compositional data do not need to sum to one.
#' @param Nsamp A numeric list of the same length as `fleets`. Either single
#'   values or vectors of the same length as the number of years can be passed
#'   through. Single values are repeated for all years. If no fleet collected
#'   samples, keep the value as `Nsamp = NULL`.
#' @param fleets A vector of integers specifying which fleets to include. The
#'   order of the fleets pertains to the input order of other arguments. An
#'   entry of `fleets = NULL` leads to zero samples for any fleet.
#' @param years A list the same length as `fleets` giving the years as numeric
#'   vectors. If no fleet collected samples, keep the value as `years = NULL`.
#' @param ESS The final effective sample size (ESS) associated with the
#'   simulated data. The ESS is not used to generate the simulated data but can
#'   be used as an input sample size in subsequent models that estimate
#'   population parameters or status. The default, `NULL`, leads to the true
#'   internally calculated ESS being used, which is `Nsamp` for the
#'   multinomial case or given by the formula under `cpar` for the Dirichlet
#'   case. At least one value must be provided for each fleet or a vector of
#'   year-specific values can be used for any given fleet. The argument accepts
#'   a list with entries, either a single integer or a vector of integers, for
#'   each fleet.
#' @param cpar A numeric value or vector the same length as `fleets`
#'   controlling the variance of the Dirichlet distribution used for sampling.
#'   A value of `1` leads to the same standard deviation as a multinomial of
#'   the given `Nsamp`, `2` indicates twice, etc. Values greater than one
#'   indicate overdispersion, and less than one underdispersion. `NULL` or `NA`
#'   for a given fleet will lead to no dispersion.
#' @param ... Any argument you want to be a column in the new data frame of
#'   composition data. All extra arguments should be named columns in `data`.
#'   Each argument needs to be a list of length `length(fleets)`. Or, you can
#'   use a single value that will be repeated for each combination of fleet,
#'   year, and the rest of the input data.
#'
#' @author Kelli F. Johnson
#' @return A data frame of observed composition data.
#'
sample_comp <- function(data,
                        Nsamp,
                        fleets,
                        years,
                        ESS = NULL,
                        cpar = 1,
                        ...) {
  #### Perform input checks
  if (is.null(fleets)) {
    return(data[0, ])
  }
  if (is.null(Nsamp)) {
    return(data[0, ])
  }
  cpar <- switch(class(cpar),
    list = ifelse(mapply(is.null, cpar), NA, unlist(cpar, recursive = FALSE)),
    numeric = cpar,
    logical = NA,
    vector = cpar,
    NULL = NA
  )

  # ESS can be (1) user input, (2) NULL -> Nsamp, (3) Dirichlet calculated ESS
  useESS <- ifelse(is.null(ESS), FALSE, TRUE)
  if (is.null(ESS)) {
    ESS <- Nsamp
  }
  if (useESS) { # in case there are NA values.
    ESS <- mapply(function(ess, nsamp) {
      if (any(is.na(ess))) {
        if (length(ess != nsamp) & length(nsamp == 1)) {
          nsamp <- rep(nsamp, length.out = length(ess))
        }
        new_ess <- mapply(function(e, n) ifelse(is.na(e), n, e),
          e = ess, n = nsamp, SIMPLIFY = FALSE
        )
        new_ess <- unlist(new_ess)
      } else {
        new_ess <- ess
      }
      new_ess
    }, ess = ESS, nsamp = Nsamp, SIMPLIFY = FALSE)
  }

  # Check for bad inputs
  lapply(
    list(years, fleets, Nsamp, ESS, cpar, ...),
    function(x, fleetN = length(fleets)) {
      if (!length(x) %in% c(1, fleetN)) {
        stop(
          call. = FALSE,
          "Bad input to ss3sim sampling function.\n",
          "There is only ", fleetN, " fleets, yet your input was a ",
          class(x), " with a length of ", length(x), ". See below:\n", x
        )
      }
    }
  )

  # Repeat short inputs
  new <- dplyr::bind_cols(tibble::tibble(fleet = fleets), tibble::tibble(
    year = years,
    newN = Nsamp, ESS = ESS, cpar = cpar, ...
  )) |>
    dplyr::rowwise() |>
    tidyr::unnest(dplyr::everything()) |>
    dplyr::bind_rows()
  # todo: make the rename above more generic

  #### Multinomial or DM sampling based on case_when with cpar
  # Results are slightly different because of some seed thing with dplyr
  # sample_dm or sample_mn will give same values if used in loop
  # or force seed in the function
  all <- dplyr::inner_join(
    x = data,
    y = new,
    by = stats::na.omit(colnames(new)[match(colnames(data), colnames(new))])
  ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      comp = dplyr::case_when(
        is.na(cpar) ~ list(sample_mn(
          data = dplyr::c_across(dplyr::matches("[0-9]+")),
          n = newN
        )),
        is.numeric(cpar) ~ list(sample_dm(
          data = dplyr::c_across(matches("[0-9]+")),
          n = newN, par = cpar
        ))
      ),
      ncalc = dplyr::case_when(
        is.na(cpar) ~ newN,
        is.numeric(cpar) ~ newN / cpar^2
      ),
      useESS_var = useESS,
      Nsamp = ifelse(useESS_var, ESS, ncalc)
    ) |>
    dplyr::select(
      1:(dplyr::matches("Nsamp") - 1),
      Nsamp,
      comp
    )
  comp <- NULL # To remove "no visible binding for global variable 'comp'"
  return(
    cbind(
      dplyr::select(all, -comp),
      do.call("rbind", all$comp)
    ) |>
      `colnames<-`(colnames(data))
  )
}
