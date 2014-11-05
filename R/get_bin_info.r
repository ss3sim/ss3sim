#' Get the parameter values for change_bin
#'
#' This function organizes arguments for other functions needed by 
#' \code{change_bin}.
#'
#' @param dat A list of sample arguments from all sampling functions
get_bin_info <- function(dat) {
    dat <- dat[!vapply(dat, is.null, FUN.VALUE = logical(1L))]
    dummy_dat_list <- do.call("rbind", 
      lapply(seq_along(dat), function(type) {
      do.call("rbind",
      lapply(seq_along(dat[[type]]$fleets), function(fleet) {
      data.frame(
        "fleet" = dat[[type]]$fleets[fleet],
        "year"  = dat[[type]]$years[[fleet]],
        "type"  = names(dat)[type],
        stringsAsFactors = FALSE)}
      ))
    }))
    invisible(dummy_dat_list)
}