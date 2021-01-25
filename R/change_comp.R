#' Change composition data to dummy data
#' @template dat_list
#' @param type The sample type you want. See the default argument for the available types.
#' @param paramlist A list of parameter values derived from the data frame used to set up
#' your similation. For example, \code{setup_scenarios(setup_scenarios_defaults())[[1]]}
#' will give you defaults that you can extract from. Typically,
#' \code{mylist[[c("agecomp_params", "lcomp_params")]]} are passed. Make sure that you
#' only pass the portion of the list that pertains to the data you want.
#' @param nsex An integer value between one and two specifying the number of sexes in the
#' model, where 1 is based on females only for spawning stock biomass and two-sex models
#' allow for sex-specific parameters.
#' @param bins A vector of bins for the composition data. The bins do not
#' need to be named because they will be renamed with their value and a
#' leading character based on what type of data they are.
#' @importFrom magrittr %>%
#' @examples
#' # todo: remove this example when testing is complete
#' \dontrun{
#' change_comp(dat_list = dat, type = "len",
#'   paramlist = scenariol[[1]][[c("lcomp_params","agecomp_params")]])
#' }
change_comp <- function(
  dat_list,
  type = c("len", "age", "cal"),
  paramlist,
  nsex = 1,
  bins
  ) {

  helper <- function(data) {
    xx <- tibble::as_tibble(data) %>% 
      dplyr::select(-dplyr::matches("cpar|Nsamp")) %>% 
      tidyr::unnest(dplyr::everything())
    if (!"Seas" %in% colnames(xx)) xx[["Seas"]] <- 1
    return(xx)
  }

  type <- match.arg(type, several.ok = FALSE)
  if (missing(bins)) {
    lbin_vector <- dat_list[["lbin_vector"]]
    agebin_vector <- dat_list[["agebin_vector"]]
  } else {
    lbin_vector <- bins
    agebin_vector <- bins
  }

  out <- dplyr::bind_cols(
    lapply(paramlist, helper) %>%
    dplyr::bind_rows() %>% dplyr::distinct(),
    data.frame(
    Gender = ifelse(nsex == 1, 0, 3),
    Part = 0,
    Nsamp = 10)
    ) %>%
    dplyr::rename(Yr = dplyr::matches("years"), FltSvy = dplyr::matches("fleets")) %>%
    dplyr::relocate(dplyr::any_of(c("years", "Seas", "fleets")))

  ## Length data
  if (type %in% c("len", "cal")) {
    if (type == "len") {
      old <- dat_list[["lencomp"]][0, ]
      old <- change_dat_bin(old, setup_bins(lbin_vector, nsex = nsex, leader = "l"))
    } else {
      old <- dat_list[["lencomp"]]
    }

    final <- change_dat_bin(out, setup_bins(lbin_vector, nsex = nsex, leader = "l"))

    # todo: remove duplicated rows
    dat_list[["lencomp"]] <- dplyr::bind_rows(old, final)  %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[bflm][0-9]"),
        ~tidyr::replace_na(.x, 1))) %>% as.data.frame()
    dat_list[["lbin_vector"]] <- lbin_vector
    dat_list[["N_lencomp"]] <- NROW(dat_list[["lencomp"]])
    dat_list[["N_lbins"]] <- length(lbin_vector)
  }

  ## Age data
  if (type %in% c("age", "cal")){
    if (type == "age") {
      old <- dat_list[["agecomp"]] %>% dplyr::filter(.data[["Lbin_lo"]] > 0)
    } else {
      old <- dat_list[["agecomp"]] %>% dplyr::filter(.data[["Lbin_lo"]] <= -1)
    }

    if (type == "cal") {
      out <- tidyr::crossing(out, data.frame(Lbin_lo = lbin_vector, Lbin_hi = lbin_vector))
    }
    # todo: name the bin based on data type, a, l, f, or m
    final <- change_dat_bin(out, setup_bins(agebin_vector, nsex = nsex, leader = "a"))

    if (nsex == 2 & type == "cal") {
      # todo: determine what we want to do about Gender for CAAL
      female <- final %>% dplyr::mutate(Gender = 1)
      male <- final %>% mutate(Gender = 2)
      final <- rbind(female, male)
    }
    # todo: get rid of join warning
    dat_list$agecomp <- dplyr::full_join(old, final) %>%
      dplyr::mutate(dplyr::across(dplyr::matches("[abflm][0-9]"),
        ~tidyr::replace_na(.x, 1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Lbin"),
        ~tidyr::replace_na(.x, -1))) %>%
      dplyr::mutate(dplyr::across(dplyr::starts_with("Age"),
        ~tidyr::replace_na(.x, 1))) %>% as.data.frame()
    dat_list[["N_agecomp"]] <- NROW(dat_list[["agecomp"]])
    dat_list[["agebin_vector"]] <- agebin_vector
    dat_list[["N_agebins"]] <- length(dat_list[["agebin_vector"]])
  }

  return(dat_list)
}
