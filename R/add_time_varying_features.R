#' Methods to include time varying param features

#' Documentation not complete
#' Include_timevarying_param accept three different options
#' 1. using the "block" option for time varying param
#' 2. using the "dev" option for time varying param
#' 3. using the "env" option for time varying param

#' @export
			
add_time_varying_features <- function(working_folder=working_folder,
  ctl.file.in ="control.ss_new", ctl.file.out ="YTF.ctl",
  Dat.file="YTF.dat", par.file="ss3.par", Nblocks, Blockpattern, Dev,
  how.time.varying="env") {

  require(r4ss)

  ## To read in the control file and create an R object to modify afterwards
  SS_ctl <- readLines(con = paste(working_folder, "/", ctl.file.in, sep=""))
  ## To read in the dat file 
  SS_dat <- SS_readdat(file= paste(working_folder, "/", Dat.file,
      sep=""), verbose = FALSE, echoall = FALSE, section = NULL)
  year.beg=SS_dat$styr
  year.end=SS_dat$endyr

  if(how.time.varying == "block")
  {
    # change the block pattern
    ch1 <- grep("#_Nblock_Patterns", SS_ctl)
    ch2 <- regexpr("#_Nblock_Patterns", SS_ctl[ch1])[1]
    val1 <- as.numeric(as.vector(substr(SS_ctl[ch1], start=1, stop=ch2-1)))
    if (val1>1) print("Beware! There are more than ONE block pattern in this ctl file")
    SS_ctl[ch1] = paste(1, "#_Nblock_Patterns", sep=" ")
    SS_ctl[ch1+1] = paste(Nblocks, "#_blocks_per_pattern ", sep=" ")
    SS_ctl[ch1+3] = paste(Blockpattern, collapse=" ")
    # now add the time varying feature into the model	 
    # find the line specifying natural mortality params
    ch3 <- grep("# NatM_p_1_Fem_GP_1", SS_ctl)
    ch4 <- regexpr("# NatM_p_1_Fem_GP_1", SS_ctl[ch3])[1]
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]	# extract the element of the character vector
    val = as.numeric(val)	# make them a numeric vector
    check = (is.na(val))
    if (sum(check)>0) 
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"), print("Something is wrong. Please check the control.ss.new file"))					
    }	

    # find the line specifying natural mortality params and specify the environmental link
    val[8] <- -1
    SS_ctl[ch3] <- paste(c(val, "# NatM_p_1_Fem_GP_1"), collapse=" ")
    # Now specify the custom MG-env setup
    ch5 <- grep("#_Cond 0  #custom_MG-env_setup (0/1)", SS_ctl, fixed=TRUE)
    ch6 <- regexpr(" #custom_MG-env_setup (0/1)", SS_ctl[ch5], fixed=TRUE)[1]
    SS_ctl[ch5] <- paste(0, "#custom_MG-env_setup (0/1)")
    val2 <- c(-1, 2, 1, 0, -1, 99, -2)
    SS_ctl[ch5+1] <- paste(c(val2, " # env link specification i.e fixed to 1"), collapse=" ")
  }

  if(how.time.varying == "env")
  {
    # add the time varying feature into the ctl file	 
    # find the line specifying natural mortality params
    ch3 <- grep("# NatM_p_1_Fem_GP_1", SS_ctl)
    ch4 <- regexpr("# NatM_p_1_Fem_GP_1", SS_ctl[ch3])[1]
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]	# extract the element of the character vector
    val = as.numeric(val)	# make them a numeric vector
    check = (is.na(val))
    if (sum(check)>0) 
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"),
        print("Something is wrong. Please check the control.ss.new file"))					
    }	
    # find the line specifying natural mortality params and specify the environmental link
    val[8] <- -1
    SS_ctl[ch3] <- paste(c(val, "# NatM_p_1_Fem_GP_1"), collapse=" ")
    # Now specify the custom MG-env setup
    ch5 <- grep("#_Cond 0  #custom_MG-env_setup (0/1)", SS_ctl, fixed=TRUE)
    ch6 <- regexpr(" #custom_MG-env_setup (0/1)", SS_ctl[ch5], fixed=TRUE)[1]
    SS_ctl[ch5] <- paste(0, "#custom_MG-env_setup (0/1)")
    val2 <- c(-1, 2, 1, 0, -1, 99, -2)
    SS_ctl[ch5+1] <- paste(c(val2, " # env link specification i.e fixed to 1"), collapse=" ")

    writeLines(SS_ctl, con= paste(working_folder, "/", ctl.file.out, sep=""))

    # add the time varying feature into the dat file	 
    # Now put the environmental covariate into the dat file
    SS_data <- readLines(con = paste(working_folder, "/", Dat.file, sep=""))
    ch1d <- grep("#_N_environ_variables", SS_data) 
    ch2d <- regexpr(" #_N_environ_variables", SS_data[ch1d], fixed=TRUE)[1]
    SS_data[ch1d] <- paste(1, " #_N_environ_variables")
    ch3d <- grep("#_N_environ_obs", SS_data, fixed=TRUE) 
    ch4d <- regexpr(" #_N_environ_obs", SS_data[ch3d], fixed=TRUE)[1]
    SS_data[ch3d] <- paste((year.end-year.beg+1), " #_N_environ_obs")
    endfile = length(SS_data)
    First_piece = SS_data[1:ch3d]
    Last_piece = SS_data[(ch3d+1):endfile]				
    # to see whether such environemental data exist already or not
    ch5d <- grep("#_N_sizefreq_methods", SS_data, fixed=TRUE) 
    if((ch5d-ch3d)==1) env.dat <- data.frame(Year=year.beg:year.end, Variable=1, Value=Dev)
    if((ch5d-ch3d)>1) { env.dat <- c() ; print("WARNINGS: env data already exist. Check") }
    # combine back everything
    New.dat=c(First_piece, apply(env.dat, 1, function(x) paste(x, collapse=" ")), Last_piece)
    # write output	
    writeLines(New.dat, con= paste(working_folder, "/", Dat.file, sep=""))
  }

  if(how.time.varying == "dev")
  {
    # add the time varying feature into the ctl file	 
    # find the line specifying natural mortality params
    ch3 <- grep("# NatM_p_1_Fem_GP_1", SS_ctl)
    ch4 <- regexpr("# NatM_p_1_Fem_GP_1", SS_ctl[ch3])[1]
    val = strsplit(substr(SS_ctl[ch3], start=1, stop=ch4-1), " ")[[1]]	# extract the element of the character vector
    val = as.numeric(val)	# make them a numeric vector
    check = (is.na(val))
    if (sum(check)>0) 
    {
      print("Some values are removed. Normally it is the white space but checking it...")
      val = val[check==FALSE]	# only keep the ones that are non NAs
      ifelse(length(val)==14, print("Everything is fine"), print("Something is wrong. Please check the control.ss.new file"))					
    }	
    # find the line specifying natural mortality params and specify the environmental link
    val[9] <- 2
    val[10] = year.beg
    val[11] = year.end
    val[12] = 1				
    SS_ctl[ch3] <- paste(c(val, "# NatM_p_1_Fem_GP_1"), collapse=" ")
    # Now specify the custom MG-env setup		
    writeLines(SS_ctl, con= paste(working_folder, "/", ctl.file.out, sep=""))

    # add grwoth dev estimates into the par file	 
    SS_par <- readLines(con = paste(working_folder, "/", par.file, sep=""))
    ch1 <- grep("# SR_parm[1]:", SS_par, fixed=TRUE)
    MGdev.exist = sum(as.numeric((grep("# MGparm_dev:", SS_par, fixed=TRUE))))
    length.file=length(SS_par)
    beg.file=SS_par[1:(ch1-1)]
    end.file=SS_par[(ch1):length.file]
    if(MGdev.exist==0) {to.add <- c("# MGparm_dev:", paste(Dev, collapse=" "))} 
    if(MGdev.exist!=0) {print("WARNINGS: MGparm_dev line already exist"); to.add <- NULL}
    new.par=c(beg.file, to.add, end.file)
    writeLines(new.par, con = paste(working_folder, "/", par.file, sep=""))
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
		# Blockpattern = c(year.beg, year.beg+50, year.beg+51, year.end)
	# # Block value
		# Env = c(rep(0, 50), rep(0.5, 50))


### Add some time varying param using the env link
## block patterm
#dir_folder <- "C:\\Users\\Kot\\Dropbox\\Kot working folder\\Fish600\\"
#working_folder <- paste(dir_folder, "trial", sep="")
# SS_data <- readLines(con = paste(working_folder, "/", "YTF.dat", sep=""))
# year.beg=SS_dat$styr
# year.end=SS_dat$endyr
#Dev = c(rep(0.2, (2000-1913+1)), rep(0.1, (2012-2000)))-0.2
#Include_timevarying_param(working_folder=working_folder, ctl.file.in ="control.ss_new", ctl.file.out ="YTF.ctl", Dat.file="YTF.dat", par.file="ss3.par", Nblocks, Blockpattern, Dev=Dev, how.time.varying="dev")

# direc = paste(dir_folder, "/Yellowtail - step1 - Copy", sep="")
# direc = paste(dir_folder, "trial", sep="")
# direc = paste(dir_folder, "time varying trial", sep="")
# m1 = SS_output(dir=direc, model="ss3", covar=FALSE, forecast=FALSE)
# SS_plots(m1, uncertainty=F)
