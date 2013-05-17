#' Determine level of bias adjustment for SS runs
#'
#' Determine level of bias adjustment from multiple SS runs.
#' IMPORTANT: The Hessian must be calculated for the SS the runs that
#' this function uses.
#'
#' @details
#' This function: \itemize{
#' \item uses the \code{r4ss} package to read in output from 5 SS
#' runs, 
#' \item uses Ian Taylor's \code{r4ss} function to find values for the
#' 5 bias adjustment parameters for each run,
#' \item takes the average over runs for each bias adjustment
#' parameter
#' \item writes out the unaveraged and averaged
#' (\code{CorrectBias.dat} and \code{AvgBias.dat}, respectively) bias
#' adjustment parameters to the \code{dir} folder
#' \item takes a \code{control.ss_new} file from one of the 5 SS runs,
#' changes the 5 bias adjustment parameters, and writes the whole
#' updated \code{control.ss_new} file with new bias adjustment
#' parameters to an \code{em.ctl} file
#' }
# The new \code{em.ctl} file (that now contains the updated
# bias adjustment parameters) can then be copied to the folders for
# each run of the scenario \code{run_bias_ss3} needs to be run for
# each scenario, but results can be used for all runs of that
# scenario
#' @author Carey McGilliard
#' @export
#'
#' @param dir Folder for all of the bias adjustment runs (e.g.
#' \code{"M1-F1-D1-R1-cod/bias"} which must contain numbered
#' folders for the \code{nsim} runs, e.g.
#' \code{"M1-F1-D1-R1-cod/bias/1/"},
#' \code{"M1-F1-D1-R1-cod/bias/2/"}, ...,
#' \code{"M1-F1-D1-R1-cod/bias/5/"} if there are \code{nsim =
#' 5} bias adjustment runs)
#' @param outdir Folder containing the run folders for a given
#' scenario (e.g. \code{"M1-F1-D1-R1-cod"} that contains
#' \code{"M1-F1-D1-R1-cod/1/"} \code{"M1-F1-D1-R1-cod/2/"}, etc.)
#' @param nsim number of bias adjustment runs conducted for a
#' particular scenario (e.g. \code{5})
# @param iter number of runs conducted for a scenario after bias
# adjustment is complete (e.g. \code{1:100})
#'
#' @examples \dontrun{
#' # Main Function call example:
#' # Note that usually nsim will be 5, not 2,
#' # nsim = 2 is just faster for an example
#' run_bias_ss3(dir = "M1-F1-D1-R1-cod/", outdir = "M1-F1-D1-R1-cod/",
#' nsim = 2)
#' }

run_bias_ss3 <-function(dir, outdir, nsim) {
  outfile = "CorrectBias.DAT"
  mysims = 1:nsim
  sapply(mysims, bias_ss3, dir = dir)


  # Read in the raw bias adjustment parameters for each run,
  # calculated from Ian Taylor's r4ss function:
  bias.table <-read.table(paste(dir,outfile,sep=""),header = FALSE)

  #rename some variables to enhance code interpretability
  names(bias.table)[names(bias.table) == "V1"] = "Sim"
  names(bias.table)[names(bias.table) == "V2"] = "bias1"
  names(bias.table)[names(bias.table) == "V3"] = "bias2"
  names(bias.table)[names(bias.table) == "V4"] = "bias3"
  names(bias.table)[names(bias.table) == "V5"] = "bias4"
  names(bias.table)[names(bias.table) == "V6"] = "bias5"


  #Find the average over nsim runs of each bias adjustment parameter
  avg.df = data.frame(
    bias1 = mean(bias.table$bias1),
    bias2 = mean(bias.table$bias2),
    bias3 = mean(bias.table$bias3),
    bias4 = mean(bias.table$bias4),
    bias5 = mean(bias.table$bias5))

  #Write avg.df values to the the file AvgBias.DAT under the dir folder  
  write.table(avg.df, file = paste0(dir, "AvgBias.DAT"), 
    row.names = FALSE, col.names = TRUE, quote = FALSE, append = F)

  # Open the control.ss_new file from one of the bias adjustment runs,
  # find where bias adjustment parameters are specified,
  # and update the bias adjustment parameters to the values in avg.df
  # (average corrected bias adjustment parameters)
  # write the updated .ctl file to em.ctl within the dir directory:

  # This loop and other changes would be necessary if .ctl files
  # differed within scenario (among runs for the same scenario):
  # for (iter in 1:nsim) 
  # {
  # read in a ctl.ss_new file, replace the bias adjust params, and
  # write out a new em.ctl file      

  #grab the control.ss_new file
  SS_ctl <- readLines(con = paste0(dir,1,"/control.ss_new")) 
  SS_ctlB = SS_ctl

  #grab the line number on which this text occurs
  ParamLine1 <- grep("#_last_early_yr_nobias_adj_in_MPD", SS_ctl) 

  #in what column does the character string start?
  colnum1 <- regexpr("#_last_early_yr_nobias_adj_in_MPD", SS_ctl[ParamLine1])[1] 

  # what is the value before the character string?
  # val1 <- as.numeric(as.vector(substr(SS_ctl[ParamLine1], start=1, stop=colnum1-1))) 

  SS_ctlB[ParamLine1] = paste0(avg.df$bias1, " #_last_early_yr_nobias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 1] = paste0(avg.df$bias2, " #_first_yr_fullbias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 2] = paste0(avg.df$bias3, " #_last_yr_fullbias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 3] = paste0(avg.df$bias4, " #_first_recent_yr_nobias_adj_in_MPD")
  SS_ctlB[ParamLine1 + 4] = paste0(avg.df$bias5, " #_max_bias_adj_in_MPD (-1 to override ramp and set biasadj=1.0 for all estimated recdevs)")

  writeLines(SS_ctlB, con = paste(dir, "em.ctl", sep = ""))
  #}

  # place the new em.ctl file in the em folder for each model realization,
  # assuming that the .ctl file does not change between realizations!!!
  #for (iRealSim in iter) {
    #file.copy(from = paste0(dir, "em.ctl"), to = paste0(outdir, 
        #iRealSim, "/em/"), overwrite = T, copy.mode = TRUE)
  #}
}

