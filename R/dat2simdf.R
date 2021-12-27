#' Create a simulation data frame from a data file
#'
#' Create a simulation data frame (`simdf`) from a data input file for a
#' Stock Synthesis operating model. Typically, this data file is not used
#' to inform the data structure. Here we reverse engineer the ss3sim process
#' of using `simdf` to dictate what the data file looks like and use the data
#' file to dictate what `simdf` looks like.
#'
#' @template dat_list
#'
#' @export
#' @author Kelli F. Johnson
#' @examples
# dat_list <- r4ss::SS_readdat("OM.dat")
# test <- dat2dimdf(dat_list)
# str(test)
dat2dimdf <- function(dat_list) {
  catches <- dat_list[["catch"]]
  agecomp <- dat_list[["agecomp"]]
  simdf <- data.frame(
    cf.years.1 = catches %>% filter(year > 0) %>% pull(year) %>% rlang::expr_text(),
    # Need estimates of F rather than catch values, which requires Report.sso
    cf.fvals.1 = catches %>% filter(year > 0) %>% rlang::expr_text(),
    si.years.2 = dat_list[["CPUE"]][, "year"] %>% rlang::expr_text(),
    # Check that these values do not vary by year
    si.sds_obs.2 = dat_list[["CPUE"]][1, "se_log"],
    si.seas.2 = dat_list[["CPUE"]][1, "seas"],
    sl.Nsamp.1 = NA,
    sl.Nsamp.2 = NA,
    sa.Nsamp.1 = agecomp %>% filter(FltSvy == 1) %>% pull(Nsamp) %>% rlang::expr_text(),
    sa.years.1 = agecomp %>% filter(FltSvy == 1) %>% pull(Yr) %>% rlang::expr_text(),
    # Doesn't work because there are no ages for fleet 2
    sa.Nsamp.2 = agecomp %>% filter(FltSvy == 2) %>% pull(Nsamp) %>% rlang::expr_text(),
    sa.years.2 = agecomp %>% filter(FltSvy == 2) %>% pull(Nsamp) %>% rlang::expr_text(),
    sa.cpar = "NULL"
  )
}
