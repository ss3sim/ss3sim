#' Standardize column name for FltSvy in event \pkg{r4ss} is not the newest version.
#'
#' \code{change_fltname} alters the name for the fleet/survey column
#' which is typically named FltSvy by \code{\link[r4ss]{SS_readdat}}, but was
#' inconsistent in older versions (.e.g. Fleet was used for mean size-at-age).
#'
#' @template dat_list
#'
#' @return An invisible data list.
#'
#' @export
#'
#' @author Kelli Johnson
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' file_in <- file.path(d, "Simple", "simple.dat")
#' # Here one should include the argument \code{section = 2}
#' # but this \code{.dat} file does not have multiple sections.
#' dat_in <- r4ss::SS_readdat(file_in, verbose = FALSE)
#' dat_fixed <- change_fltname(dat_in)
#' # Check mean size-at-age
#' names(dat_fixed$MeanSize_at_Age_obs)[3] == "FltSvy"

change_fltname <- function(dat_list){
    use <- "FltSvy"
    try <- c("fl", "fleet", "flt")

    truenames <- tolower(names(dat_list$lencomp))
    if(any(try %in% truenames)) {
        names(dat_list$lencomp)[grepl("fl", truenames)] <- use
    }

    truenames <- tolower(names(dat_list$agecomp))
    if(any(try %in% truenames)) {
        names(dat_list$agecomp)[grepl("fl", truenames)] <- use
    }

    truenames <- tolower(names(dat_list$MeanSize_at_Age_obs))
    if(any(try %in% truenames)) {
        names(dat_list$MeanSize_at_Age_obs)[grepl("fl", truenames)] <- use
    }

    invisible(return(dat_list))
}
