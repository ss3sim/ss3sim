#' @description Function to standardize the bounds of the control file in the estimation model.
#' This function first checks to ensure the initial values in the estimation model control file are set to
#' the true values of the operating model control file and if not sets them for every parameter.
#' Next, the function adjusts the LO and HI values in the estimation model control file to be a 
#' fixed percentage of the initial value for every parameter.
#' @param percent.df is a data.frame with nine rows and three columns. The first column is the parameter
#' @param OM.ctl.file is a string with the path and name of the operating model control file
#' @param EM.ctl.file is a string with the path and name of the estimation model control file
#' name. The second column is what % of the initial parameter value LO should be set to. The third column
#' is what % of the initial parameter value HI should be set to. 
standardize_bounds<-function(percent.df, OM.ctl.file, EM.ctl.file){
  #First, ensure the initial value in the EM control file is set to the OM true value
  #Read in OM true value
  om.pars<-SS_parlines(ctlfile=OM.ctl.file)
  #Read in EM initial value
  em.pars<-SS_parlines(ctlfile=EM.ctl.file)
  #If they are not equal, set the EM initial value to the OM true value
  if(om.pars[,3]==)
  
  #Second, use the input data frame to set the LO and HI values of the EM control file
  #To a fixed % of the init value as provided in the user input
  
  #Check input parameter names are valid - do we have an SS_New file yet?
  #Read in parameter names from SS_New
  em.pars<-SS_parlines(ctlfile=EM.ctl.file)
  #Do these match the data frame first column?
  
}