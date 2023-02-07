#' Change the recdev options
#'
#' @template ctl_list
#' @param recdev_option The recruitment deviation option number to change
#' to. Must be between 1 and 4. (May need to modify additional lines to 
#' to option 0)
#' the recdev options are:
#' 0 = none,
#' 1 = devvector (R=F(SSB)+dev, 
#' 2 = deviations (R=F(SSB)+dev), 
#' 3 = deviations (R=R0*dev; dev2=R-f(SSB)), and 
#' 4 = like 3 with sum(dev2) adding penalty

change_recdev_option <- function(ctl_list, recdev_option = 1) {
    ctl_list$do_recdev <- recdev_option
    ctl_list
}