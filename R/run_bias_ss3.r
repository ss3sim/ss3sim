#' Determine level of bias adjustment for SS3 runs
#'
#' Determine level of bias adjustment from multiple SS3 runs. IMPORTANT: The
#' Hessian must be calculated for the SS3 runs that this function uses.
#'
#' @details
#' This function: \itemize{
#' \item uses the \pkg{r4ss} package to read in output from n SS3 runs,
#' \item uses Ian Taylor's \pkg{r4ss} function to find values for the n bias
#'   adjustment parameters for each run,
#' \item takes the average over runs for each bias adjustment parameter
#' \item writes out the unaveraged and averaged (\code{AdjustBias.DAT} and
#'   \code{AvgBias.DAT}, respectively) bias adjustment parameters to the
#'   \code{dir} folder
#' \item takes a \code{control.ss_new} file from one of the n SS runs, changes
#'   the n bias adjustment parameters, and writes the whole updated
#'   \code{control.ss_new} file with new bias adjustment parameters to an
#'   \code{em.ctl} file
#' }
# The new \code{em.ctl} file (that now contains the updated bias adjustment
# parameters) can then be copied to the folders for each run of the scenario.
# \code{run_bias_ss3} needs to be run for each scenario, but results can be used
# for all runs of that scenario.
#' @author Carey McGilliard
#' @export
#' @param dir Folder for all of the bias adjustment runs (e.g.
#' \code{"F1-D1-cod/bias"} which must contain numbered
#' folders for the \code{nsim} runs, e.g.
#' \code{"F1-D1-cod/bias/1/"},
#' \code{"F1-D1-cod/bias/2/"}, ...,
#' \code{"F1-D1-cod/bias/10/"} if there are \code{nsim =
#' 10} bias adjustment runs)
#' @param outdir Folder containing the run folders for a given scenario (e.g.
#'   \code{"F1-D1-cod"} that contains \code{"F1-D1-cod/1/"}
#'   \code{"F1-D1-cod/2/"}, etc.)
#' @param nsim number of bias adjustment runs conducted for a particular
#'   scenario (e.g. \code{10})
#' @param conv_crit The maximum percentage of bias iterations that can produce
#'   a non-invertible Hessian before a warning will be produced.  If this
#'   percentage is exceeded then a file \code{WARNINGS.txt} will be produced.
#'   Currently, the simulations will continue to run.
#' @seealso \code{\link{run_ss3sim}}, \code{\link{ss3sim_base}},
#' \code{\link{run_ss3model}}, \code{\link{bias_ss3}}
#' @references
#' Methot, R. D. and Taylor, I. G. (2011). Adjusting for bias due to
#' variability of estimated recruitments in fishery assessment models.
#' Can. J. Fish. Aquat. Sci., 68(10):1744-1760.
#' @examples \dontrun{
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-bias-example")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' d <- system.file("extdata", package = "ss3sim")
#' case_folder <- paste0(d, "/eg-cases")
#' om <- paste0(d, "/models/cod-om")
#' em <- paste0(d, "/models/cod-em")
#' wd <- getwd()
#' setwd(temp_path)
#' # (Note that bias_nsim should be bigger, say 10, but it is set to 2
#' # here so the example runs faster.)
#' run_ss3sim(iterations = 1:1, scenarios = "D1-F0-cod",
#'   case_folder = case_folder, om_dir = om, em_dir = em,
#'   bias_adjust = TRUE, bias_nsim = 2)
#' setwd(wd)
#' }

run_bias_ss3 <-function(dir, outdir, nsim, conv_crit = 0.2) {
  outfile = "AdjustBias.DAT"
  mysims = 1:nsim
  sapply(mysims, bias_ss3, dir = dir)

  # Read in the raw bias adjustment parameters for each run,
  # calculated from Ian Taylor's r4ss function:
  bias.table <-read.table(paste(dir,"/",outfile,sep=""),header = FALSE)

  #rename some variables to enhance code interpretability
  names(bias.table)[names(bias.table) == "V1"] = "Sim"
  names(bias.table)[names(bias.table) == "V2"] = "bias1"
  names(bias.table)[names(bias.table) == "V3"] = "bias2"
  names(bias.table)[names(bias.table) == "V4"] = "bias3"
  names(bias.table)[names(bias.table) == "V5"] = "bias4"
  names(bias.table)[names(bias.table) == "V6"] = "bias5"

  #Find the average over nsim runs of each bias adjustment parameter
  avg.df = data.frame(
    bias1 = mean(bias.table$bias1, na.rm=TRUE),
    bias2 = mean(bias.table$bias2, na.rm=TRUE),
    bias3 = mean(bias.table$bias3, na.rm=TRUE),
    bias4 = mean(bias.table$bias4, na.rm=TRUE),
    bias5 = mean(bias.table$bias5, na.rm=TRUE))

  #Write avg.df values to the the file AvgBias.DAT under the dir folder
  write.table(avg.df, file = paste0(dir, "/", "AvgBias.DAT"),
    row.names = FALSE, col.names = TRUE, quote = FALSE, append = F)

  # If the number of NAs (i.e not invertible hessian) > conv_crit of the cases,
  # then create a WARNING.txt file
  if(sum(as.numeric(is.na(bias.table$bias1)))/length(bias.table$bias1) > conv_crit) {
    WARNINGS <- paste(
"WARNINGS: more than conv_crit% of cases produced non invertible Hessians.
These are iterations ",
      paste(which(is.na(bias.table$bias1)), collapse = ","))
    write.table(WARNINGS, file = paste0(dir, "/WARNINGS.txt"))
  }

  # Open the control.ss_new file from one of the bias adjustment runs,
  # find where bias adjustment parameters are specified,
  # and update the bias adjustment parameters to the values in avg.df
  # (average bias adjustment parameters)
  # write the updated .ctl file to em.ctl within the dir directory:

  # This loop and other changes would be necessary if .ctl files
  # differed within scenario (among runs for the same scenario):
  # for (iter in 1:nsim)
  # {
  # read in a ctl.ss_new file, replace the bias adjust params, and
  # write out a new em.ctl file

  # grab the "ctl" file and make a change to it for the bias adjustement period
  SS_ctl <- readLines(con = paste0(dir, "/", 1, "/em/em.ctl"))
  SS_ctlB = SS_ctl

  #grab the line number on which this text occurs
  ParamLine1 <- grep("#_last_early_yr_nobias_adj_in_MPD", SS_ctl)

  #in what column does the character string start?
  colnum1 <- regexpr("#_last_early_yr_nobias_adj_in_MPD", SS_ctl[ParamLine1])[1]

  # what is the value before the character string?
  # val1 <- as.numeric(as.vector(substr(SS_ctl[ParamLine1], start=1, stop=colnum1-1)))

  SS_ctlB[ParamLine1] =
    paste0(avg.df$bias1, " #_last_early_yr_nobias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 1] =
    paste0(avg.df$bias2, " #_first_yr_fullbias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 2] =
    paste0(avg.df$bias3, " #_last_yr_fullbias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 3] =
    paste0(avg.df$bias4, " #_first_recent_yr_nobias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 4] =
    paste0(avg.df$bias5,
      " #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)")

  writeLines(SS_ctlB, con = paste(dir, "/", "em.ctl", sep = ""))
  #}

  # place the new em.ctl file in the em folder for each model realization,
  # assuming that the .ctl file does not change between realizations!!!
  #for (iRealSim in iter) {
    #file.copy(from = paste0(dir, "em.ctl"), to = paste0(outdir,
        #iRealSim, "/em/"), overwrite = T, copy.mode = TRUE)
  #}
}
