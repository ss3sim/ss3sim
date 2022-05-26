#' Change composition data to dummy data for running the operating model
#'
#' Change the composition data in a Stock Synthesis data list object or file
#' to include rows of data that are desired. Typically, this will be an
#' operating model (OM) because only dummy-data observations are used here,
#' i.e., all compositions are set to a value of one. Creating these dummy
#' observations is helpful before running your OM because it will facilitate
#' the creation of observed values for each desired combination.
#'
#' @template dat_list
#' @param type The sample type you want.
#' See the function call for available types,
#' e.g., `formals(change_comp)$type`;
#' the first value will be used as the default if user input is not provided.
#' @param paramlist A list of parameter values derived from the data frame used to set up
#' your simulation. E.g., `setup_scenarios(setup_scenarios_defaults())[[1]]`
#' will give you defaults that you can extract from. Typically,
#' `mylist[[c("agecomp_params", "lcomp_params")]]` are passed. Make sure that you
#' only pass the portion of the list that pertains to the data you want.
#' @param nsex An integer value between one and two specifying the number of sexes in the
#' model, where 1 is based on females only for spawning stock biomass and two-sex models
#' allow for sex-specific parameters.
#' @param bins A vector of bins for the composition data. The bins do not
#' need to be named because they will be renamed with their value and a
#' leading character based on what type of data they are.
#' @examples
#' # todo: remove this example when testing is complete
#' \dontrun{
#' change_comp(
#'   dat_list = dat, type = "len",
#'   paramlist = scenariol[[1]][[c("lcomp_params", "agecomp_params")]]
#' )
#' }
change_comp <- function(dat_list,
                        type = c("len", "age", "cal"),
                        paramlist,
                        nsex = 1,
                        bins) {
  type <- match.arg(type, several.ok = FALSE)
  if (missing(bins)) {
    lbin_vector <- dat_list[["lbin_vector"]]
    agebin_vector <- dat_list[["agebin_vector"]]
  } else {
    if (type == "cal") {
      message(
        "You really should not be changing bins if you are using\n",
        "CAAL data because this will also lead to marginal comps using those\n",
        "same bins. Please set them using lcomp_params and agecomp_params.\n",
        "If you feel this feature should be added please contact\n",
        "the package maintainer `utils::maintainer('ss3sim')`."
      )
    }
    lbin_vector <- bins
    agebin_vector <- bins
  }

  out <- dplyr::left_join(
    # Not really joining at all if no columns match
    by = character(),
    # Data frame from parameter list
    tibble::as_tibble(paramlist) %>%
      dplyr::select(-dplyr::matches("cpar|Nsamp")) %>%
      dplyr::rename_all(tolower) %>%
      dplyr::distinct(),
    # Default, behind-the-scenes data frame
    data.frame(
      seas = 1,
      gender = ifelse(nsex == 1, 0, 3),
      part = 0,
      nsamp = 10
    )
  ) %>%
    # Get rid of default columns if parameter list had them
    dplyr::select(-dplyr::matches("\\.y")) %>%
    dplyr::rename_all(~ gsub("\\.x", "", .x)) %>%
    dplyr::rename(
      Yr = dplyr::matches("years"),
      FltSvy = dplyr::matches("fleets")
    ) %>%
    dplyr::rename_all(~ gsub("(^[a-z]{1})", "\\U\\1", .x, perl = TRUE)) %>%
    dplyr::relocate(dplyr::any_of(c(
      "Yr",
      "Seas",
      "FltSvy",
      "Gender",
      "Part",
      "Nsamp"
    ))) %>%
    # Make all combinations by fleet
    tidyr::unnest(dplyr::everything())

  ## Length data
  if (type %in% c("len", "cal")) {
    if (type == "len") {
      old <- dat_list[["lencomp"]][0, ]
      # todo: I think this is redundant with code below
      old <- change_dat_bin(old, setup_bins(lbin_vector, nsex = nsex, leader = "l"))
    } else {
      old <- dat_list[["lencomp"]]
    }

    final <- change_dat_bin(out, setup_bins(lbin_vector, nsex = nsex, leader = "l"))

    dat_list[["lencomp"]] <- dplyr::bind_rows(old, final) %>%
      dplyr::distinct(.keep_all = TRUE) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("[bflm][0-9]"),
        ~ tidyr::replace_na(.x, 1)
      )) %>%
      as.data.frame()
    dat_list[["lbin_vector"]] <- lbin_vector
    dat_list[["N_lencomp"]] <- NROW(dat_list[["lencomp"]])
    dat_list[["N_lbins"]] <- length(lbin_vector)
  }

  ## Age data
  if (type %in% c("age", "cal")) {
    if (type == "age") {
      old <- dat_list[["agecomp"]] %>% dplyr::filter(.data[["Lbin_lo"]] > 0)
    } else {
      old <- dat_list[["agecomp"]] %>% dplyr::filter(.data[["Lbin_lo"]] <= -1)
    }

    if (type == "cal") {
      out <- tidyr::crossing(out, data.frame(Lbin_lo = lbin_vector, Lbin_hi = lbin_vector))
    }

    final <- change_dat_bin(out, setup_bins(agebin_vector, nsex = nsex, leader = "a"))

    if (nsex == 2 & type == "cal") {
      # todo: determine what we want to do about Gender for CAAL
      female <- final %>% dplyr::mutate(Gender = 1)
      male <- final %>% dplyr::mutate(Gender = 2)
      final <- rbind(female, male)
    }
    # todo: get rid of join warning
    dat_list$agecomp <- dplyr::full_join(
      by = colnames(final),
      old,
      final
    ) %>%
      dplyr::mutate(dplyr::across(
        dplyr::matches("[abflm][0-9]"),
        ~ tidyr::replace_na(.x, 1)
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::starts_with("Lbin"),
        ~ tidyr::replace_na(.x, -1)
      )) %>%
      dplyr::mutate(dplyr::across(
        dplyr::starts_with("Age"),
        ~ tidyr::replace_na(.x, 1)
      )) %>%
      as.data.frame()
    dat_list[["N_agecomp"]] <- NROW(dat_list[["agecomp"]])
    dat_list[["agebin_vector"]] <- agebin_vector
    dat_list[["N_agebins"]] <- length(dat_list[["agebin_vector"]])
  }

  return(dat_list)
}
