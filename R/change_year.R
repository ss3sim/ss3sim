#' Change the years estimated
#'
#' Keep all of the data in the model but change the years that are estimated
#' in the model. First year of the model will be first year of non-zero catch.
#' Main recruitment period starts 1/2 generation time before first year
#' of compositional data included in the model.
#' Late recruitment is the last year of the model by default and cannot be
#' modified using this function, neither can early recruitment, which starts
#' in year 1.
#'
#' @template dat_list
#' @template ctl_list
change_year <- function(dat_list, ctl_list) {
  # Find first year of compositional data
  firstyearcomp <- min(c(dat_list$lencomp$Yr, dat_list$agecomp$Yr))
  halfage50mat <- ctl_list$MG_parms[
    grep("50\\%", row.names(ctl_list$MG_parms)), "INIT"
  ] / 2
  dat_list <- change_startyear(dat_list)
  ctl_list <- change_recyear(ctl_list,
    main = floor(firstyearcomp - halfage50mat)
  )
  invisible(list("dat_list" = dat_list, "ctl_list" = ctl_list))
}
#'
#' Change start year of the data file
#'
#' @template dat_list
#' @param firstyear An integer value specifying the year in which you want to start
#' fitting your data. The default is `NULL`, which will look to the first year
#' with a non-zero catch in `dat_list` and use that as the first year.
#'
#' @return
#' A [r4ss::SS_readdat()] list with an augmented start year.
#' @author Kelli Faye Johnson
#'
change_startyear <- function(dat_list, firstyear = NULL) {
  if (is.null(firstyear)) {
    firstyear <- min(c(
      dat_list$catch$year[dat_list$catch$year >= 0 & dat_list$catch$catch > 0],
      dat_list$CPUE$year[dat_list$CPUE$obs > 0]
    ))
  }

  dat_list$styr <- firstyear
  invisible(dat_list)
}
#'
#' Change start year main recruitment deviations in control file
#'
#' @template ctl_list
#' @param main An integer value specifying the year in which you want to
#' start the main period of recruitment.
#'
#' @return
#' A [r4ss::SS_readctl()] list with an augmented start year of
#' the recruitment deviations in the main period.
#' @author Kelli Faye Johnson
#'
change_recyear <- function(ctl_list, main) {
  ctl_list$MainRdevYrFirst <- main
  invisible(ctl_list)
}
