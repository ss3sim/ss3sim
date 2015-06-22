#' Standardize column name for FltSvy in event \pkg{r4ss} is not the newest version.
#'
#' \code{change_fltname} alters the name for the fleet/survey column
#' which is typically named FltSvy by \code{\link[r4ss]{SS_readdat}}, but was
#' inconsistent in older versions (.e.g. Fleet was used for mean size-at-age).
#'
#' @template datfile
#'
#' @return An invisible data list.
#'
#' @export
#'
#' @author Kelli Johnson
# #' @examples
# #' ## These examples are in development still and untested
# #' d <- system.file("extdata", package = "ss3sim")
# #' file_in <- paste0(d, "/Simple/simple.dat")
# #' dat_in <- r4ss::SS_readdat(file_in, verbose = FALSE)
# #' dat_fixed <- change_fltname(dat_in)
# #' ## Check mean size-at-age
# #' names(dat_fixed$MeanSize_at_Age_obs)[3] == "FltSvy"

change_fltname <- function(datfile){
    use <- "FltSvy"
    try <- c("fl", "fleet", "flt")

    truenames <- tolower(names(datfile$lencomp))
    if(any(try %in% truenames)) {
        names(datfile$lencomp)[grepl("fl", truenames)] <- use
    }

    truenames <- tolower(names(datfile$agecomp))
    if(any(try %in% truenames)) {
        names(datfile$agecomp)[grepl("fl", truenames)] <- use
    }

    truenames <- tolower(names(datfile$MeanSize_at_Age_obs))
    if(any(try %in% truenames)) {
        names(datfile$MeanSize_at_Age_obs)[grepl("fl", truenames)] <- use
    }

    invisible(return(datfile))
}
