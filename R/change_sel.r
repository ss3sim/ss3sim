#' Methods to include time varying selectivity features
#'
#' This function takes generic OM files for sardine, flatfish, and cod and
#' inserts time-varying selectivity. 
#'
#' @param use Logical. Should this function do anything?
#' @param dev A vector of environmental data of \code{length = length of *.dat
#' endyr-startyr+1}
#' @param how_time_varying The method for including time-varying M in
#' the SS input files the 3 options are: \code{"env"} \code{"block"}
#' or \code{"dev"}. 
#' \itemize{
#' \item \code{"env"} is to include an *additive* functional linkage
#' between environmental data and sel where a link parameter is
#' estimated: \code{M\' (y) = sel + link * env(y)}
#' \item \code{"block"} is to allow sel to be estimated separately in different
#' pre-specified time blocks
#' \item \code{"dev"} is to estimate a mean sel and to estimate deviations about
#' that mean
#' }
#' @param ctl_file_in Input control file name (before sel estimation method is
#' chosen)
#' @param ctl_file_out Output control file name (resulting control file with M
#' changed)
#' @param dat_file Input \code{.dat} file name
#' @param dat_file_out Output data file name
#' @param par_file \code{.par} Input file name 
#' @param par_file_out \code{.par} Output file name 
#' @param starter_file Input \code{starter.ss} file location
#' @param starter_file_out Output \code{starter.ss} file location
#' @param report_file Input \code{Report.sso} file
#' @param n_blocks The number of time blocks within which sel is estimated
#' separately; Equal to 1 unless \code{how_time_varying} is \code{"block"}, but
#' \code{n_blocks} is not used in the function unless \code{how_time_varying} is
#' \code{"block"}
#' @param block_pattern Block pattern. A vector of years marking time blocks
#' with separate Ms in each block - doesn't get used in the function if
#' \code{how_time_varying} does not equal \code{"block"}
#' @param ss3path The path to your SS3 binary if the binary is not in
#' your path. For example, if \code{SS3} was in the folder
#' \code{/usr/bin/} then \code{ss3path = "/usr/bin/"}. Make sure to
#' append a slash to the end of this path. Defaults to \code{NULL},
#' which means the function will assume the binary is already in your
#' path.
#' @author Kotaro Ono and Carey McGilliard, modified by Juan Valero
#' and Felipe Hurtado-Ferro for changing selectivity
#' @export 
#'
#' @details Although it appears that 3 options exist for how
#' time-varying sel is modeled within SS and within this function, right
#' now only the \code{how_time_varying = "env"} (making sel vary over time by
#' including an *additive* environmental linkage to M) has been
#' tested. The \code{ctl_file_in} parameter needs to be a
#' \code{.ss_new} file because the documentation in \code{.ss_new}
#' files are automated and standardized; this function takes advantage
#' of standard documentation used to figure out where additional lines
#' need to be added to \code{.ctl}, \code{.par}, and \code{.dat} files
#' to implement time-varying sel. 
#' 
#' NOTE: the user has to define an environmental data series such that
#' the additive linkage creates the desired time-varying pattern in
#' sel. 
#'
#' @examples 
#' \dontrun{
#' # sardine model
#'  change_sel(how_time_varying="env",ctl_file_in = "control.ss_new",dat_file =
#'  "SardOM.dat",par_file = "SS3.par",n_blocks = 1,block_pattern =
#'  c(1990,2001),dev = rnorm(100,mean = 0,sd = 0.6))
#' 
#' # flatfish model
#'  change_sel(how_time_varying="env",
#'           ctl_file_in = "control.ss_new",ctl_file_out="Flatfish_m.ctl",
#'           dat_file = "Flatfish.dat",dat_file_out = "Flatfish_m.dat",
#'           par_file = "ss3.par",par_file_out = "ss3.par",
#'           n_blocks = 2,block_pattern = c(1990,2001),
#'           dev = c(rep(0,length = 50),rep(0.1,length=50)))
#' # 
#' # sardine model
#'  change_sel(how_time_varying="env",
#'           ctl_file_in = "control.ss_new",ctl_file_out="SardOM_m.ctl",
#'           dat_file = "SardOM.dat",dat_file_out = "SardOM_m.dat",
#'           par_file = "ss3.par",par_file_out = "ss3.par",
#'           n_blocks = 2,block_pattern = c(1990,2001),
#'           dev = c(rep(0,length = 50),rep(0.1,length=50)))
#' }

change_sel <- function(use=FALSE, dev, how_time_varying = "env", ctl_file_in =
  "control.ss_new", ctl_file_out = "Simple.ctl", dat_file =
  "Simple.dat", dat_file_out = "Simple.dat", par_file = "ss3.par",
  par_file_out="ss3.par", starter_file = "starter.ss", starter_file_out
  = "starter.ss", report_file = "Report.sso", n_blocks = 1,
  block_pattern = NA, ss3path = NULL) {
  
 
  if(use == FALSE)
    return()

  # how_time_varying <- how_time_varying[1] #In the future if people want to
  # include more than one kind of time-varying M 
  # we can make how_time_varying into a vector, but now it's not.
  # Test to see if the argument how_time_varying was input correctly; stop
  # running if it's wrong.
  if(!how_time_varying %in% c("env", "block", "dev")) 
    stop("how_time_varying must be equal to env, block, or dev")
  
  # Read in the control and dat files and create R objects to modify
  ## afterwards, read in .dat file
  SS_ctl <- readLines(ctl_file_in)
  SS_dat <- r4ss::SS_readdat(dat_file, verbose = FALSE, echoall = FALSE, section = NULL)
  SS_par <- readLines(con = par_file)  

  year.beg=SS_dat$styr
  year.end=SS_dat$endyr
  
  # Find name of fleet to use in grep later to find the fleet selectivity
  Fleetname=SS_dat$fleetnames[1]
  FleetSelLabel=paste("# ","SizeSel_1P_1_",Fleetname[1],sep="")
  
  #Copy original files (this is just a failsafe to prevent loss of info)
  file.copy(from = ctl_file_in, to = "control_pre_change_sel.ss_new",
    overwrite = T, copy.mode = TRUE)
  file.copy(from = par_file, to = "pre_change_sel.par", overwrite = T,
    copy.mode = TRUE)
  file.copy(from = dat_file, to = "pre_change_sel.dat", overwrite = T, copy.mode = TRUE)

  # how_time_varying = "env" may be the only how_time_varying option that's
  # working!!! Use it!!!
  # "env" is the preferred method of doing time-varying M that we decided on as
  # a class
  if(how_time_varying == "env") {
    # add the time varying feature into the ctl file   
    # find the line specifying sel params
    
    # Find the line # of the line with the character string "#
    # VonBert_K_Fem_GP_1" in SS_ctl
    ch3a <- grep(FleetSelLabel, SS_ctl) 
    ch3<-ch3a[1]
    
    # Extract every value that comes before FleetSelLabel on
    # line ch3 as characters in the character vector "val"
    ch4 <- regexpr(FleetSelLabel, SS_ctl[ch3])[1] 
    
    # Extract every value that comes before FleetSelLabel on
    # line ch3 as characters in the character vector "val"
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]  
    val = val[-which(val=="")]
    val = as.numeric(val)	# change val to a numeric vector
    check = (is.na(val)) #check for missing values
    if (sum(check)>0) #Extra QAQC check
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"),
             print("Something is wrong. Please check the control.ss.new file"))					
    }	
    # find the line specifying sel params and specify
    # the environmental link
    val[8] <- -2
    val[10] <- year.beg
    val[11] <- year.end
    SS_ctl[ch3] <- paste("",c(val, FleetSelLabel), collapse=" ")
    # Now specify the custom sel-env setup
    ch5 <- grep("#_Cond 0 #_custom_sel-env_setup (0/1)", SS_ctl, fixed=TRUE)
    ch6 <- regexpr(" #_custom_sel-env_setup (0/1)", SS_ctl[ch5], fixed=TRUE)[1]
    SS_ctl[ch5] <- paste(0, "#custom_sel-env_setup (0/1)")
    val2 <- c(-1, 2, 1, 0, -1, 99, -2)
    SS_ctl[ch5+1] <- paste(c(val2, " # env link specification i.e fixed to 1"), collapse=" ")
    ch7 <- grep("#_env/block/dev_adjust_method", SS_ctl)[2]
	SS_ctl[ch7] <- paste(1, "#_env/block/dev_adjust_method", sep=" ")
		
    writeLines(SS_ctl, con= ctl_file_out)

    # add the time varying feature into the dat file	 
    # Now put the environmental covariate into the dat file
    SS_data <- readLines(con = dat_file)
    ch1d <- grep("#_N_environ_variables", SS_data) 
    ch2d <- regexpr(" #_N_environ_variables", SS_data[ch1d], fixed=TRUE)[1]
    SS_data[ch1d] <- paste(2, " #_N_environ_variables")
    ch3d <- grep(" #_N_environ_obs", SS_data, fixed=TRUE)
    ch4d <- regexpr(" #_N_environ_obs", SS_data[ch3d], fixed=TRUE)[1]
    SS_data[ch3d] <- paste((year.end-year.beg+1)*2, " #_N_environ_obs")
    
    ch5d <- grep("# N sizefreq methods", SS_data, fixed=TRUE) -1

    endfile = length(SS_data)
    First_piece = SS_data[1:ch5d]
    Last_piece = SS_data[(ch5d+1):endfile]				
    # to see whether such environemental data exist already or not
#     ch5d <- grep("# N sizefreq methods to read", SS_data, fixed=TRUE) 
    env.dat <- data.frame(Year=year.beg:year.end, Variable=2, Value=dev)
#     if((ch5d-ch3d)>1) { env.dat <- c() ; print("WARNINGS: env data already exist. Check") }
    # combine back everything
    New.dat=c(First_piece, apply(env.dat, 1, function(x) paste(x, collapse=" ")), Last_piece)
    
    # write output	
    writeLines(New.dat, con= dat_file_out)
    # Add sel environm link to .par
    lastselpar <- grep("# selparm",SS_par)[length(grep("# selparm",SS_par))]
    selnumbounds <- c(regexpr("[",SS_par[lastselpar],fixed=T)[1],regexpr("]",SS_par[lastselpar],fixed=T)[1])
    selnum <- as.numeric(substr(SS_par[lastselpar],start=selnumbounds[1]+1, stop=selnumbounds[2]-1))+1
    SS_par = c(SS_par, paste("# selparm[",selnum,"]:",sep=""), "1.00000000")     
   
    #CRM 5/19/2013: modify the *.par file to include the environmental link parameter (MGparam[17])
    
    #run SS with with no estimation and no hessian
    #first change starter file option to use .par to .ctl
    SS_Starter <- readLines(con = starter_file) 
    UseParLine =grep("# 0=use init values in control file; 1=use ss3.par", SS_Starter, fixed=TRUE) 
    
   SS_Starter[UseParLine] = "0 # 0=use init values in control file; 1=use ss3.par"
   #SS_Starter[UseParLine-2] = dat_file_out #This could create a mess up but probably not
   #SS_Starter[UseParLine-1] = ctl_file_out
   writeLines(SS_Starter,con = starter_file_out)
   #
   #
   ##Call ss3 for a run that includes the environmental link
   if(is.null(ss3path)) {
     system("SS3 -noest")
   } else {
     system(paste0(ss3path, "SS3 -noest"))
   }
   #
   ##Change starter file option back to using .par!
   SS_Starter[UseParLine] = "1 # 0=use init values in control file; 1=use ss3.par"
   writeLines(SS_Starter,con = starter_file_out)
   writeLines(SS_par,con=par_file_out)  
   #
   ##Dig through Report.sso file to find out the par number associated with environmental link parameter
   #SS_Report = readLines(con = report_file)
	#EnvLineNum =grep(paste(FleetSelLabel,"_ENV",sep=""), SS_Report, fixed=TRUE) 
   #LinkLine = SS_Report[EnvLineNum]
   #EnvCol <- regexpr(paste(FleetSelLabel,"_ENV",sep=""), SS_Report[EnvLineNum], fixed=TRUE)[1]
   #MGParmNum = as.numeric(substr(LinkLine,start = 1, stop = (EnvCol-1)))
   #
   ##Remake the ss3.par file with the enviro link param...
   #ParSearchPhrase = paste0("# MGparm[",MGParmNum-1,"]:")
   #ParLineA = grep(ParSearchPhrase,SS_par,fixed=TRUE)
   #FirstParFile = SS_par[1:(ParLineA+1)]
   #LastParFile = SS_par[(ParLineA+2):(length(SS_par))]
   ##write a new par file that merges original par file info with inclusion of enviro link param
   #NewParLine1 = paste0("# MGparm[",MGParmNum,"]:")
   #NewParLine2 = "1.00000000000"
   #New.par=c(FirstParFile,NewParLine1,NewParLine2, LastParFile)
   #writeLines(New.par, con= par_file_out)
   #
  
  
  }
  
   
  
  # We are not currently using how_time_varying = "dev" and this section may
  # need updating!!!
  if(how_time_varying == "dev") {
    # add the time varying feature into the ctl file	 
    # find the line specifying sel params
    ch3 <- grep(FleetSelLabel, SS_ctl)
    ch4 <- regexpr(FleetSelLabel, SS_ctl[ch3])[1]
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]	# extract the element of the character vector
    val = as.numeric(val)	# make them a numeric vector
    check = (is.na(val))
    if (sum(check)>0) 
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"), print("Something is wrong. Please check the control.ss.new file"))					
    }	
    # find the line specifying sel params and specify
    # the environmental link
    val[9] <- 2
    val[10] = year.beg
    val[11] = year.end
    val[12] = 1				
    SS_ctl[ch3] <- paste(c(val, FleetSelLabel), collapse=" ")
    # Now specify the custom MG-env setup		
    writeLines(SS_ctl, con= ctl_file_out)
    
    # add grwoth dev estimates into the par file	 
    SS_par <- readLines(con = par_file)
    ch1 <- grep("# SR_parm[1]:", SS_par, fixed=TRUE)
    MGdev.exist = sum(as.numeric((grep("# MGparm_dev:", SS_par, fixed=TRUE))))
    length.file=length(SS_par)
    beg.file=SS_par[1:(ch1-1)]
    end.file=SS_par[(ch1):length.file]
    if(MGdev.exist==0) {to.add <- c("# MGparm_dev:", paste(dev, collapse=" "))} 
    if(MGdev.exist!=0) {print("WARNINGS: MGparm_dev line already exist"); to.add <- NULL}
    new.par=c(beg.file, to.add, end.file)
    writeLines(new.par, con = par_file)
  }
  
  # We are not currently using how_time_varying = "block" and this section may
  # need updating!!!
  if(how_time_varying == "block") {
    # change the block pattern
    ch1 <- grep("#_Nblock_Patterns", SS_ctl)
    ch2 <- regexpr("#_Nblock_Patterns", SS_ctl[ch1])[1]
    val1 <- as.numeric(as.vector(substr(SS_ctl[ch1], start=1, stop=ch2-1)))
    if (val1>1) print("Beware! There are more than ONE block pattern in this ctl file")
    SS_ctl[ch1] = paste(1, "#_Nblock_Patterns", sep=" ")
    SS_ctl[ch1+1] = paste(n_blocks, "#_blocks_per_pattern ", sep=" ")
    SS_ctl[ch1+3] = paste(block_pattern, collapse=" ")
    # now add the time varying feature into the model   
    # find the line specifying sel params
    ch3 <- grep(FleetSelLabel, SS_ctl)
    ch4 <- regexpr(FleetSelLabel, SS_ctl[ch3])[1]
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]  # extract the element of the character vector
    val = as.numeric(val)	# make them a numeric vector
    check = (is.na(val))
    if (sum(check)>0) 
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"), print("Something is wrong. Please check the control.ss.new file"))					
    }	
    
    # find the line specifying sel params and specify
    # the environmental link
    val[8] <- -1
    SS_ctl[ch3] <- paste(c(val, FleetSelLabel), collapse=" ")
    # Now specify the custom MG-env setup
    ch5 <- grep("#_Cond 0  #custom_MG-env_setup (0/1)", SS_ctl, fixed=TRUE)
    ch6 <- regexpr(" #custom_MG-env_setup (0/1)", SS_ctl[ch5], fixed=TRUE)[1]
    SS_ctl[ch5] <- paste(0, "#custom_MG-env_setup (0/1)")
    val2 <- c(-1, 2, 1, 0, -1, 99, -2)
    SS_ctl[ch5+1] <- paste(c(val2, " # env link specification i.e fixed to 1"), collapse=" ")
  }
  
  
}

## choose the directory to work in 
#dir_folder <- "C:\\Users\\Kot\\Dropbox\\Kot working folder\\Fish600\\"
#dir_folder <- "C:\\Users\\Kotaro Ono\\Dropbox\\Kot working folder\\Fish600\\"
#working_folder <- paste(dir_folder, "trial", sep="")
## to do some checking of the model convergence
#m1 <- SS_output(dir=working_folder, model="ss3", covar=FALSE, forecast=FALSE)
## To read in the bootstrap data file and create an R object to modify afterwards
#SS_dat <- SS_readdat(file= paste(working_folder, "/YTF.dat", sep=""), verbose = TRUE, echoall = FALSE, section = NULL)
## To read in the control file and create an R object to modify afterwards
#SS_ctl <- readLines(con = paste(working_folder, "/control.ss_new", sep=""))
## To read in the par file
#SS_par <- scan(file=paste(working_folder, "/ss3.par", sep=""), what="character")

# SS_ctl[1:50]

# # extract the starting/ending year in the .dat file 
# year.beg=SS_dat$styr
# year.end=SS_dat$endyr
# # specifying the different blockpattern (only used for the operating model)
# block_pattern = c(year.beg, year.beg+50, year.beg+51, year.end)
# # Block value
# Env = c(rep(0, 50), rep(0.5, 50))

### Add some time varying param using the env link
## block patterm
#dir_folder <- "C:\\Users\\Kot\\Dropbox\\Kot working folder\\Fish600\\"
#working_folder <- paste(dir_folder, "trial", sep="")
# SS_data <- readLines(con = paste(working_folder, "/", "YTF.dat", sep=""))
# year.beg=SS_dat$styr
# year.end=SS_dat$endyr
#dev = c(rep(0.2, (2000-1913+1)), rep(0.1, (2012-2000)))-0.2
#Include_timevarying_param(working_folder=working_folder, ctl_file_in ="control.ss_new", ctl_file_out ="YTF.ctl", dat_file="YTF.dat", par_file="ss3.par", n_blocks, block_pattern, dev=dev, how_time_varying="dev")

# direc = paste(dir_folder, "/Yellowtail - step1 - Copy", sep="")
# direc = paste(dir_folder, "trial", sep="")
# direc = paste(dir_folder, "time varying trial", sep="")
# m1 = SS_output(dir=direc, model="ss3", covar=FALSE, forecast=FALSE)
# SS_plots(m1, uncertainty=F)


