#' Perform a single bias adjustment run
#'
#' This function is run within \code{\link{run_bias_ss3}} and for a
#' single run it: \itemize{
#'   \item uses \pkg{r4ss} function \code{\link[r4ss]{SS_output}} to read in the
#' output from a single bias adjustment run
#'   \item uses \pkg{r4ss} function \code{\link[r4ss]{SS_fitbiasramp}} to
#'     calculate the bias adjustment parameters for that run
#' \item Writes the bias adjustment parameters to the file \code{AdjustBias.DAT}
#'   within the \code{dir} folder, overwriting the file if \code{iter = 1} (the
#'   first run) and appending the file otherwise
#' }
#' @author Carey McGilliard
#' @param iter Replicate number. Used to identify this iteration if there are
#'   multiple adjustment runs.
#' @param dir Passes \code{dir} from the function \code{\link{run_bias_ss3}} to
#'   \code{bias_ss3}. In \code{\link{run_bias_ss3}} this is run within an
#'   \code{\link[base]{sapply}} function for each of the bias adjustment runs.
#' @seealso \code{\link{run_bias_ss3}}, \code{\link{run_ss3sim}},
#'   \code{\link{ss3sim_base}}
#' @importFrom r4ss SS_output SS_fitbiasramp
#' @export
#' @return
#' A plain text file containing the bias adjustment variables is created at
#' \code{dir/AdjustBias.DAT}. A PDF figure is created in
#' \code{dir/biasramp-N.pdf}, where \code{N} represents the iteration number.
#' @references
#' Methot, R. D. and Taylor, I. G. (2011). Adjusting for bias due to variability
#' of estimated recruitments in fishery assessment models. Can. J. Fish. Aquat.
#' Sci., 68(10):1744-1760.

bias_ss3 <- function(iter, dir) {
  outfile = "AdjustBias.DAT"
  #Check that the run converged and that SS_output will not give you a stop()
  test <- readLines(file.path(dir, iter, "em", "Report.sso"))
  check <- sum(grepl("na", grep("SPB_[0-9]+", test, value = TRUE),
    ignore.case = TRUE))
  if(check > 2) {
    biasvars = list()
    biasvars$df = matrix(rep(NA, 5), nrow = 5)
  } else {
    if(file.exists(paste0(dir, "/", iter, "/em/covar.sso"))==TRUE)
    {
      myoutput = suppressWarnings(
        SS_output(dir = paste0(dir, "/", iter, "/em"), repfile =
        "Report.sso", compfile = "none", covarfile =
        "covar.sso", forecast = FALSE, verbose = FALSE, ncols = 300,
        printstats = FALSE, NoCompOK = TRUE))
      pdf(paste0(dir, "/biasramp-", iter, ".pdf"))
      biasvars = try(SS_fitbiasramp(replist = myoutput), TRUE)
      dev.off()
    }

    if(file.exists(paste0(dir, "/", iter, "/em/covar.sso"))==FALSE)
    {
      biasvars = list()
      biasvars$df = matrix(rep(NA,5),nrow=5)
    }
  }

  if (is.list(biasvars) == TRUE) {
    bias.df = data.frame(Sim = iter, bias1 = biasvars$df[1,
      1], bias2 = biasvars$df[2, 1], bias3 = biasvars$df[3,
      1], bias4 = biasvars$df[4, 1], bias5 = biasvars$df[5,
      1])
  }
  if (iter == 1) {
    write.table(bias.df, file = paste0(dir, "/", outfile),
      row.names = FALSE, col.names = FALSE, quote = FALSE,
      append = FALSE)
  }
  else {
    write.table(bias.df, file = paste0(dir, "/", outfile),
      row.names = FALSE, col.names = FALSE, quote = FALSE,
      append = TRUE)
  }
}
