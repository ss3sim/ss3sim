#' @param dat_list  A Stock Synthesis  data list object as read in from
#' \code{\link[r4ss]{SS_readdat}}.
#' Be sure to correctly specify which section of the data file you want
#' to work with when reading it in using the \code{section} argument.
#' Where, \code{section = 1} reads in the input values used to run the model
#' and \code{section = 2} reads in the expected values generated given all the
#' input to the OM. \code{section = 3} is not used within ss3sim, but this
#' section provides bootstrapped data sets that have been sampled internally
#' within SS.
