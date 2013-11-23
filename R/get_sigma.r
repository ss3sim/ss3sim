#' Get recruitment deviation sigma
#' 
#' Use the name of the operating model to open the ctl file and obtain the 
#' INIT value for sigmaR (recruitment deviations sigma)
#'
#' @param om The name of the operating model, which should be the prefix of
#' the \code{.ctl} file, eg. "myOM".
#' @author Kelli Johnson
#' @export

get_sigmar <- function(om) {
  ctlFileName <- paste( om, ".ctl", sep = "" )
  if (!file.exists ( ctlFileName ) ) 
    stop ( "Cannot find the .ctl file for the specified operating model." )
  ctlFile <- readLines ( ctlFileName )
 # The line contains multiple values, I want the third value which is the INIT: 
  sigmaRLoc <- grep ( "SR_sigmaR", ctlFile ) 
  sigmaRValue <- ctlFile[sigmaRLoc]
  Vals = (strsplit(sigmaRValue, " " )[[1]])
  if(Vals[1]=="") sigR= as.numeric(Vals[4])
  if(Vals[1]!="") sigR=as.numeric(Vals[3])
  return(sigR)
}
