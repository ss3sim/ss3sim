#' Calculate bias adjustment for recruitment deviations
#' 
#' Bias adjustment is performed to ensure that the only the most informative
#' data available are used when estimating recruitment deviations.
#' This process involves running the estimation method with the hessian
#' both this function and then running the estimation method again with
#' the new values.
#' 
#' \itemize{
#' \item Estimate recruitment and the standard error about those estimates.
#' \item Correct the estimates given their estimated uncertainty using a ramp.
#' \item Save a new control file.
#' \item Estimate model parameters.
#' }
#' 
#' @param dir A character string specifying the path to the folder with the
#' results from the preliminary run of the estimation model.
#' @template ctl_file_in
#' 
#' @author Kelli Faye Johnson
#' @return
#' A list of bias adjustment parameters.
#'
calculate_bias <- function(dir, ctl_file_in) {
  dir_all <- list.dirs(dir, recursive = FALSE)
  dir_num <- length(grep("bias_[0-9]+$", dir_all))
  dir_bias <- file.path(dir, sprintf("bias_%02d", dir_num))
  dir.create(dir_bias, showWarnings = FALSE, recursive = TRUE)
  file_list <- list.files(dir, full.names = TRUE, include.dirs = FALSE)
  ignore <- file.copy(file_list, dir_bias)
  ctl_file_in <- basename(ctl_file_in)

  replist <- r4ss::SS_output(dir = dir, 
    repfile = "Report.sso", compfile = "none", covarfile = "covar.sso",
    forecast = FALSE, verbose = FALSE, printstats = FALSE,
    NoCompOK = TRUE, ncols = NULL)
  
  bias <- r4ss::SS_fitbiasramp(replist, plotdir = dir_bias, 
    oldctl = file.path(dir, ctl_file_in), 
    newctl = file.path(dir, ctl_file_in),
    startvalues = NULL, 
    method = "BFGS", # c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
    altmethod = "nlminb", 
    verbose = FALSE, transform = FALSE, exclude_forecast = FALSE,
    twoplots = TRUE, plot = FALSE, print = TRUE, shownew = TRUE,
    pwidth = 6.5, pheight = 5.0, punits = "in", 
    ptsize = 10, res = 300, cex.main = 1)
  if (bias$newbias$convergence != 0) {
    bias <- r4ss::SS_fitbiasramp(replist, plotdir = dir_bias, 
      oldctl = file.path(dir, ctl_file_in), 
      newctl = file.path(dir, ctl_file_in),
      startvalues = bias$newbias$par, 
      method = "BFGS", # c("Nelder-Mead", "BFGS", "CG", "L-BFGS-B", "SANN")
      altmethod = "alt", 
      verbose = FALSE, transform = FALSE, exclude_forecast = FALSE,
      twoplots = TRUE, plot = FALSE, print = TRUE, shownew = TRUE,
      pwidth = 6.5, pheight = 5.0, punits = "in", 
      ptsize = 10, res = 300, cex.main = 1)
  }
  return(bias)
}
