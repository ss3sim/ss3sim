#' Perform a single bias adjustment run
#'
#' This function is run within \code{\link{run_bias_ss3}} and for a
#' single run it: \itemize{
#' \item uses \code{r4ss} function \code{SS_output} to read in the
#' output from a single bias adjustment run
#' \item uses \code{r4ss} function \code{SS_fitbiasramp} to calculate
#' the correct 5 bias adjustment parameters for that run
#' \item Writes the correct bias adjustment parameters to the file
#' \code{CorrectBias.DAT} within the \code{dir} folder, overwriting
#' the file if \code{iter = 1} (the first run) and appending the file
#' otherwise
#' }
#' @author Carey McGilliard
#' @param iter Replicate number, used to identify this iteration 
#' if there are multiple adjustment runs.
#' @param dir Passes \code{dir} from the function
#' \code{\link{run_bias_ss3}} to \code{bias_ss3} In
#' \code{\link{run_bias_ss3}} this is run within an \code{sapply}
#' function for each of the bias adjustment runs.
#' @export

bias_ss3 <- function(iter, dir) {
  outfile = "CorrectBias.DAT"
  myoutput = r4ss::SS_output(dir = paste0(dir, iter, "/em"), repfile =
    "Report.sso", compfile = "CompReport.sso", covarfile =
    "covar.sso", forecast = FALSE)
  biasvars = try(r4ss::SS_fitbiasramp(replist = myoutput), TRUE)
  if (is.list(biasvars) == TRUE) {
    bias.df = data.frame(Sim = iter, bias1 = biasvars$df[1, 
      1], bias2 = biasvars$df[2, 1], bias3 = biasvars$df[3, 
      1], bias4 = biasvars$df[4, 1], bias5 = biasvars$df[5, 
      1])
  }
  if (iter == 1) {
    write.table(bias.df, file = paste0(dir, outfile), 
      row.names = FALSE, col.names = FALSE, quote = FALSE, 
      append = F)
  }
  else {
    write.table(bias.df, file = paste0(dir, outfile), 
      row.names = FALSE, col.names = FALSE, quote = FALSE, 
      append = T)
  }
}

