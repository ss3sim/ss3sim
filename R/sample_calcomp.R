#' Sample conditional age-at-length data
#'
#' Sample conditional age-at-length (CAAL) data from
#' expected values of length proportions and
#' expected values of age proportions (conditional on length)
#' from the operating model (OM) and writes the samples to file for use by the
#' estimation model (EM).
#'
#' @details
#' There are many steps needed to sample CAAL data because
#' ages are not independent from lengths.
#' The data is located in the `.dat` file alongside age compositions.
#' CAAL have the added complexity of one line per length bin.
#' Thus, each row represents the observed age distribution for
#' a length bin conditioned on the fish lengths that were observed in the length compositions.
#' The age distribution will be truncated for older or younger fish.
#' Often, many rows will be empty because no fish of that length bin were observed.
#' These empty rows are not needed in the .dat file.
#'
#' The sampling process includes the following steps:
#'
#' 1. Lengths are sampled based on the desired number of lengths, $N$.
#'    $N$ is the maximum amount that could be aged.
#' 2. Those lengths are binned to create a length distribution, i.e.,
#'    numbers of fish in each length bin.
#' 3. Ages are sampled from fish that contributed to the length distribution.
#'    Several strategies are possible for sampling ages from those fish
#'
#'   1. age all fish,
#'   2. _take random subset of fish independent of length bin_, or
#'   3. take a fixed number of fish from each length bin.
#'
#' ss3sim can currently only handle randomly sampling ages from lengthed fish.
#' Future versions could include the last option;
#' please contact the developers if you are interested in helping facilitate this.
#'
#' Note that the overall total sample size for all CAAL bins is specified by
#' the user for the given fleet and year in `Nsamp_ages`.
#' These sample sizes and the expected values of age proportions
#' (conditional on length) are used to sample for realistic age proportions.
#' If all fish are aged,
#' then no resampling is performed.
#' If no fish are aged for a row of age proportions in conditional age at length data,
#' then that row is discarded.
#' If all fish are not aged,
#' then a new sample size must be drawn.
#' This new sample size must be less than or equal to the number of fish that were sampled for their length.
#' This new sample size is used to draw ages randomly from the expected values.
#' If we consider all rows for a fleet and year (one for each length bin),
#' then the sum of those will be the sample size for the CAAL data.
#' However, if the CAAL sample size is less than the length sample size,
# " we need to be careful to not age more fish in a length bin than were in that length bin in the first place.
#' We accomplish this in the code by
#' doing sampling without replacement for vectors of length bins equal to the number of fish in them.
#' This ensures realistic sampling.
#' If the option (3) above were implemented,
#' a different strategy would need to be implemented.
#' For instance,
#' if the user wants 10 fish from each length bin but only 5 fish were observed,
#' what to do?
#' A value of NULL for fleets indicates to delete the CAAL data but
#' not the marginal age data.
#'
#' When Dirichlet sampling is used for length compositions,
#' the number of fish observed will be real-valued and not whole fish.
#' One cannot simply multiply by the length composition sample size to get whole numbers because
#' they are real and
#' rounding or truncating would be unsatisfactory.
#' Currently, the function simply draws a multinomial sample from the length compositions of specified size (`Nsamp`).
#' However, this does not guarantee that fewer fish are aged than lengthed.
#' If you are specifying a small number of fish to age relative to length,
#' then this might be alright.
#' However, *we discourage the use of Dirichlet length samples when using CAAL data* as currently implemented.
#'
#' Note that this function cannot handle all types of CAAL sampling.
#' This function requires that there be a row of CAAL data
#' for each length data bin (for each year and fleet that sampling is specified to be performed),
#' where Lbin_lo and Lbin_hi are the same value.
#' Note also that this sampling procedure represents simple random sampling for CAAL, where
#' (1) lengths are sampled randomly,
#' (2) fish are lengthed and placed into bins, and
#' (3) a subset of lengthed fish are aged,
#'     where a constant proportion from each length bin are selected for aging.
#' This does not represent length stratified sampling where a subset of
#' lengthed fish are aged, and a constant number from each length bin is
#' selected for aging, although these data could also be put into a Stock
#' Synthesis model as CAAL.
#'
#' @author Cole Monnahan, Kotaro Ono
#' @template lcomp-agecomp-index
#' @template dat_list
#' @param exp_vals_list This is a data list containing all expected values. It
#'   should not be modified by previous sampling functions to contain sampled
#'   data.
#' @template outfile
#' @param Nsamp_lengths A numeric list of the same length as fleets. Either
#'  single values or vectors of the same length as the number of years can be
#'  passed through. Single values are repeated for all years. If no fleet
#'  collected samples, specify `Nsamp_lengths = NULL`. Specifically, for
#'  `sample_calcomp`, `Nsamp_lengths` denotes the total number of
#'  length samples for a given year and fleet across all length bins that can be
#'  used to then sample the conditional age at length samples.
#'  `Nsamp_lengths` must be greater than or equal to `Nsamp_ages`.
#' @param Nsamp_ages A numeric list of the same length as fleets. Either single
#'  values or vectors of the same length as the number of years can be passed
#'  through. Single values are repeated for all years. If no fleet collected
#'  samples, specify `Nsamp_ages = NULL`. Specifically, for
#'  `sample_calcomp`, `Nsamp_ages` denotes the total number of
#'  conditional age at length samples for a given year and fleet across all
#'  length bins. `Nsamp_ages` must be less than `Nsamp_lengths`.
#' @param method The method used to sample ages from the lengths. Options are
#'   "simple_random" and "length_stratified". In "simple_random" (the default
#'   option), the fish aged are randomly sampled from the age bins, so the number
#'   sampled in each age bin is not equal. In "length_stratified", an equal
#'   number of fish are aged from each length bin.
#' @param ESS_lengths  The final effective sample size (ESS) associated with the
#'  simulated length data generated for conditional age at length samples. The
#'  ESS is not used to generate the simulated data but can be used as an input
#'  sample size in subsequent models that estimate population parameters or
#'  status. The default, NULL, leads to the true (internally calculated)
#'  effective sample size being used, which is `Nsamp_lengths` for the
#'  multinomial case. `ESS_lengths` should be a numeric list of the same
#'  length as fleets. Either single values or vectors of the same length as the
#'  number of years can be passed through. Single values are repeated for all
#'  years. Note that the dimensions of ESS_lengths must be compatible with the
#'  dimensions of `Nsample_lengths`.
#' @param ESS_ages The final effective sample size (ESS) associated with the
#'  simulated conditional age at length data. The ESS is not used to generate
#'  the simulated data but can be used as an input sample size in subsequent
#'  models that estimate population parameters or status. The default, NULL,
#'  leads to the true (internally calculated) effective sample size being used,
#'  which is Nsamp_ages for the multinomial case. `ESS_ages` should be
#'  a numeric list of the same length as fleets. Either single values or vectors
#'  of the same length as the number of years can be passed through. Single
#'  values are repeated for all years. Note that the dimensions of ESS_lengths
#'  must be compatible with the dimensions of `Nsample_ages`. The input
#'  value will be apportioned among the conditional age at length bins as the
#'  `Nsamp_ages` is and therefore can be a fractional value.
#' @param lcomps_sampled Have marginal length comps already been sampled and are
#'  included in `dat_list[["lencomp"]]`? If `FALSE`, expected values are in
#'  present in `datlist[["lencomp"]]`.
#' @template sampledots
#' @template sampling-return
#' @family sampling functions
#' @export

sample_calcomp <- function(dat_list, exp_vals_list, outfile = NULL, fleets,
                           years,
                           Nsamp_lengths, Nsamp_ages,
                           method = "simple_random", ESS_lengths = NULL,
                           ESS_ages = NULL,
                           lcomps_sampled = FALSE, ...) {
  # TODO: add in length_stratified
  method <- match.arg(arg = unlist(method), choices = c("simple_random"))
  ## A value of NULL for fleets indicates not to sample and strip out the
  ## CAL data from the file.
  # Divide up age comp into marginal and CAL; we will only be sampling from
  # cal in this function, but want to retain the marginal age comps.
  agecomp.age <- dat_list$agecomp[dat_list$agecomp$Lbin_lo == -1, ] # marginal
  agecomp.cal <- dat_list$agecomp[dat_list$agecomp$Lbin_lo != -1, ] # CAL
  newfile <- dat_list
  if (is.null(fleets)) {
    newfile$agecomp <- agecomp.age # only leave in maraginal age comps
    if (!is.null(outfile)) {
      r4ss::SS_writedat(
        datlist = newfile,
        outfile = outfile,
        overwrite = TRUE,
        verbose = FALSE
      )
    }
    return(invisible(newfile))
  }
  # input checks and standardize the arg dimensions
  years <- standardize_sampling_args(
    fleets = fleets, years = years,
    other_input = Nsamp_lengths,
    return_val = "years",
    other_input_name = "Nsamp_lengths"
  )
  Nsamp_lengths <- standardize_sampling_args(
    fleets = fleets, years = years,
    other_input = Nsamp_lengths,
    other_input_name = "Nsamp_lengths"
  )
  Nsamp_ages <- standardize_sampling_args(
    fleets = fleets, years = years,
    other_input = Nsamp_ages,
    other_input_name = "Nsamp_ages"
  )
  if (!is.null(ESS_lengths)) {
    ESS_lengths <- standardize_sampling_args(
      fleets = fleets, years = years,
      other_input = ESS_lengths,
      other_input_name = "ESS_lengths"
    )
  }
  if (!is.null(ESS_ages)) {
    ESS_ages <- standardize_sampling_args(
      fleets = fleets, years = years,
      other_input = ESS_ages,
      other_input_name = "ESS_ages"
    )
  }
  ## Input checks
  Nfleets <- NROW(fleets)
  check_data(dat_list)

  # Get necessary values
  if (lcomps_sampled == TRUE) {
    lencomp_marginal <- dat_list$lencomp
  } else {
    lencomp_marginal <- NULL
  }

  # create dataframe of sampling arguments
  useESS_lengths <- ifelse(is.null(ESS_lengths), FALSE, TRUE)
  if (is.null(ESS_lengths)) {
    ESS_lengths <- Nsamp_lengths
  }
  useESS_ages <- ifelse(is.null(ESS_ages), FALSE, TRUE)
  if (is.null(ESS_ages)) {
    ESS_ages <- Nsamp_ages
  }
  new <- dplyr::bind_cols(
    tibble::tibble(FltSvy = fleets),
    tibble::tibble(
      Yr = years, Nsamp_lengths = Nsamp_lengths,
      Nsamp_ages = Nsamp_ages, ESS_lengths = ESS_lengths,
      ESS_ages = ESS_ages, ...
    )
  ) %>%
    dplyr::rowwise() %>%
    tidyr::unnest(dplyr::everything()) %>%
    dplyr::bind_rows()
  colnames(new) <- gsub("part", "Part", colnames(new))
  colnames(new) <- gsub("seas", "Seas", colnames(new))
  # will use this later
  new <- dplyr::mutate(new, ESS_ages_mult = ESS_ages / Nsamp_ages)


  ## If not, do additional argument checks
  if (nrow(agecomp.cal) == 0) { # TODO: maybe turn this into a warning instead?
    stop("No conditional age-at-length expected values found")
  }
  Nfleets <- length(fleets)
  if (any(!fleets %in% unique(agecomp.cal$FltSvy))) {
    stop(
      "A fleet specified in fleets was not found in ",
      "the fleets in age comps for dat_list."
    )
  }
  if (any(!fleets %in% unique(exp_vals_list$lencomp$FltSvy))) {
    stop(
      "A fleet specified in fleets was not found in the fleets in len ",
      "comps for exp_vals_list."
    )
  }
  if (!all(agecomp.cal$Lbin_lo == agecomp.cal$Lbin_hi)) {
    stop(
      "In order to use sample_calcomp, for each row of conditional age at",
      " length data, Lbin_lo must equal Lbin_hi. Currently, this is not ",
      "the case for all conditional age at length data within dat_list."
    )
  }
  # check that bin compression is turned off. Bin compression should never be
  # used with CAAL. If it is not turned off, warn the user.
  if (any(dat_list$age_info$mintailcomp[fleets] >= 0)) {
    stop(
      "Bin compression cannot be used for any fleets with CAL data. ",
      "Please turn off bin compression by making mintailcomp in the ",
      "age_info table negative for any fleet with CAL",
      "data and rerun ss3sim. This should be done in the skeleton OM data ",
      "file or, if using mla, can be done by specifying the tail ",
      "compression in a simdf"
    )
  }
  # check that the sample size for lengths are always greater than ages.
  Msamp_check <- mapply(function(l, a) {
    if (length(l) == 1) {
      l <- rep(l, length(a))
    }
    if (length(a) == 1) {
      a <- rep(a, length(l))
    }
    if (length(l) != length(a)) {
      stop(
        "Nsamp_lengths and Nsamp_ages should either have the same length ",
        "for each fleet or have length of 1."
      )
    }
    if (any(l < a)) {
      invalid_input <- TRUE
    } else {
      invalid_input <- FALSE
    }
    invalid_input
  }, l = Nsamp_lengths, a = Nsamp_ages, SIMPLIFY = FALSE)

  if (any(unlist(Msamp_check) == TRUE)) {
    stop(
      "More age samples specified than fish collected for calcomps. ",
      "Please adjust the Nsamp_ages specified via sample_calcomp function",
      "input so that it is less than the Nsamp_lengths for the same fleet",
      "and year."
    )
  }
  # Check that the years specified are valid
  check_yrs <- function(f, y, d) {
    yrs_in_mod <- unique(d[d$FltSvy == f, "Yr"])
    if (any(!y %in% yrs_in_mod)) {
      stop(
        "A year specified in years was not found in the expected value ",
        "age comps or length comps for fleet ", f, ". Years in expected ",
        "values (length or age comps): ",
        paste0(yrs_in_mod, collapse = ", "), "; Years ",
        "specified through years:", paste0(y, collapse = ", ")
      )
    }
  }
  # check years in both age and length comp
  mapply(check_yrs,
    f = fleets, y = years,
    MoreArgs = list(d = exp_vals_list[["lencomp"]])
  )
  mapply(check_yrs,
    f = fleets, y = years,
    MoreArgs = list(d = agecomp.cal)
  )
  ## End input checks
  # sample the length comp data that will then be used to get conditional age
  # at length samples.
  CAL_lencomp <- sample_comp(exp_vals_list[["lencomp"]],
    Nsamp = Nsamp_lengths,
    fleets = fleets,
    years = years, ESS = ESS_lengths, ...
  )
  # Loop through each line of CAL_lencomp for each line of the sampled length
  # data (CAL_lencomp).
  newcomp_list <- vector(mode = "list", length = nrow(CAL_lencomp))
  for (r in seq_len(nrow(CAL_lencomp))) {
    tmp_CAL_lencomp <- CAL_lencomp[r, , drop = FALSE]
    # for this line, subset the expected values of CAL that match it.
    newcomp <- agecomp.cal[
      agecomp.cal$Yr == tmp_CAL_lencomp$Yr &
        agecomp.cal$Seas == tmp_CAL_lencomp$Seas &
        agecomp.cal$FltSvy == tmp_CAL_lencomp$FltSvy &
        agecomp.cal$Gender == tmp_CAL_lencomp$Gender &
        agecomp.cal$Part == tmp_CAL_lencomp$Part,
    ]
    # get the sample size to use
    tmp_nsamp_ages <- dplyr::left_join(tmp_CAL_lencomp, new)
    tmp_nsamp_ages <- tmp_nsamp_ages[1, "Nsamp_ages"]
    # check that all necessary values are available
    if (nrow(newcomp) != length(dat_list$lbin_vector)) {
      stop(
        "The number of conditional age at length data rows for ",
        "fleet ", tmp_CAL_lencomp$FltSvy, " and year ", tmp_CAL_lencomp$Yr, " is not the same as the number",
        " of length bins. For each fleet and year, please make sure ",
        "there is row where Lbin_lo and Lbin_hi are equal to each of ",
        "the values in lbin_vector: ",
        paste0(dat_list$lbin_vector, collapse = ", "), ". Note that Lbin_lo and",
        " Lbin_hi if conditional age at length data should always have",
        " the same values in order for sample_calcomp() to work."
      )
    }
    # extract those expected conditional vals removing the metadata
    prob.len <- as.numeric(tmp_CAL_lencomp[, -(1:6)])
    # finally, use this information to determine number of samples for each bin of CAl,
    if (any(prob.len > 1)) {
      ## This code creates a vector of empirical samples of
      ## length, such that each length bin is repeated equal to
      ## the number of observed fish in that bin
      prob.len.inits <- unlist(mapply(function(ind, vec) {
        rep(ind, vec[ind])
      },
      ind = seq_along(prob.len),
      MoreArgs = list(vec = prob.len),
      SIMPLIFY = FALSE
      ))
      ## Now resample from it, ensuring that the sample size doesn't exceed
      temp <- sample(
        x = prob.len.inits, size = tmp_nsamp_ages,
        replace = FALSE
      )
      Nsamp.ages.per.lbin <- unlist(mapply(function(ind, samples) {
        sum(samples == ind)
      },
      ind = seq_along(prob.len),
      MoreArgs = list(samples = temp),
      SIMPLIFY = FALSE
      ))
    } else {
      Nsamp.ages.per.lbin <- stats::rmultinom(
        n = 1,
        size = tmp_nsamp_ages,
        prob = prob.len
      )
    }
    # extract expected conditional
    if (any(is.na(Nsamp.ages.per.lbin))) {
      stop("Invalid age sample size for a length bin in calcomp")
    }
    # then sample each row of CAL one at a time.
    # Sample conditional age at length. Loop through each
    # length bin and sample # fish in each age bin, given expected
    # conditional age-at-length
    newcomp$Nsamp <- Nsamp.ages.per.lbin
    for (ll in seq_len(nrow(newcomp))) {
      N.temp <- newcomp$Nsamp[ll]
      if (N.temp > 0) {
        cal.temp <-
          stats::rmultinom(
            n = 1,
            size = Nsamp.ages.per.lbin[ll],
            prob = as.numeric(newcomp[ll, -(1:9)])
          )
      } else {
        cal.temp <- -1 # placeholders
      }
      newcomp[ll, -(1:9)] <- cal.temp
    }
    newcomp <- newcomp[newcomp$Nsamp > 0, ] # get rid of placeholders
    # replace the Nsamp with ESS_ages, if applicable
    if ("ESS_ages" %in% colnames(new)) {
      tmp_metadat <- dplyr::left_join(tmp_CAL_lencomp, new)
      tmp_mult <- tmp_metadat[1, "ESS_ages_mult"]
      newcomp[["Nsamp"]] <- newcomp[["Nsamp"]] * tmp_mult
    }
    newcomp_list[[r]] <- newcomp
  }
  ## Combine back together into final data frame with the different data
  ## types
  newcomp.final <- do.call(rbind, newcomp_list)

  # Effective sample sizes for length ----
  # Rewrite to loop through each row of CAL_lencomp and add in the right ESS Val
  if ("ESS_lengths" %in% colnames(new)) {
    for (r in seq_len(nrow(CAL_lencomp))) {
      tmp_lencomp <- CAL_lencomp[r, ]
      tmp_ESS_lengths <- dplyr::left_join(tmp_lencomp, new)
      CAL_lencomp[r, "Nsamp"] <- tmp_ESS_lengths[1, "ESS_lengths"]
    }
  }

  # add the new length comp ----
  if (!is.null(lencomp_marginal)) {
    newfile[["lencomp"]] <- rbind(CAL_lencomp, lencomp_marginal)
  } else {
    newfile[["lencomp"]] <- CAL_lencomp
  }
  # age comps
  if (NROW(agecomp.age) > 0 & NROW(newcomp.final) > 0) {
    newfile[["agecomp"]] <- rbind(agecomp.age, newcomp.final)
  }
  if (NROW(agecomp.age) == 0 & NROW(newcomp.final) > 0) {
    newfile[["agecomp"]] <- newcomp.final
  }
  if (NROW(agecomp.age) > 0 & NROW(newcomp.final) == 0) {
    newfile[["agecomp"]] <- agecomp.age
  }
  if (NROW(agecomp.age) == 0 & NROW(newcomp.final) == 0) {
    newfile[["agecomp"]] <- NULL
  }
  ## Write the modified file
  if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = newfile,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }
  invisible(newfile)
}
