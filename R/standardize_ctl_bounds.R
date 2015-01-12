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
standardize_bounds<-function(percent.df, OM.ctl.file="", EM.ctl.file){
  #Read in EM values
  em.pars<-SS_parlines(ctlfile=EM.ctl.file)
  #First, ensure the initial value in the EM control file is set to the OM true value
  #If an OM is passed
  if(nchar(OM.ctl.file)>0){
    #Read in OM true value
    om.pars<-SS_parlines(ctlfile=OM.ctl.file)
    #If they are not equal, set the EM initial value to the OM true value
    if(om.pars[1:16,"INIT"]!=em.pars[1:16,"INIT"]){
      em.pars[1:16,"INIT"]<-om.pars[1:16,"INIT"]
    }
  }

  
  #Second, use the input data frame to set the LO and HI values of the EM control file
  #To a fixed % of the init value as provided in the user input
  
  #Read in parameters from EM ctl file
  em.pars<-SS_parlines(ctlfile=EM.ctl.file)
  
  #Check input parameter names are valid 
  #Do these match the data frame first column?
  if(any(!percent.df[,"label"] %in% em.pars[,"Label"])){
    print(paste("Element",which(!percent.df[,"label"] %in% em.pars[,"Label"]),"does not have a valid parameter label."))
  }else{
    #Get indices of parameters to standardize; first column is in the data frame and second is in the EM read values
    indices.to.standardize<-matrix(ncol=2,nrow=nrow(percent.df))
    indices.to.standardize[,1]<-which(percent.df[,1] %in% em.pars[,"Label"])
    indices.to.standardize[,2]<-which(em.pars[,"Label"] %in% percent.df[,1])
    
    #Change lo and hi's
    newlos<-percent.df[indices.to.standardize[,1],"lo"]*em.pars[indices.to.standardize[,2],"INIT"]
    newhis<-percent.df[indices.to.standardize[,1],"hi"]*em.pars[indices.to.standardize[,2],"INIT"]
    change_lo_hi(ctlfile=EM.ctl.file,newctlfile="C:\\Users\\Christine Stawitz\\Documents\\GitHub\\estgrowth\\models\\coa-em\\codEMTEST.ctl",
                 strings=as.character(percent.df[indices.to.standardize[,1],1]),newlos=newlos,newhis=newhis)
  }
}

#' @description This is a modified version of r4SS::SS_changepars which modifies the lo and hi bounds in the 
#' control file instead of the initial value. newhis and newlos must be equivalent lengths and both refer to
#' the parameters in strings in the same order. Also put the directory into the filenames to match the structure of the above function.
change_lo_hi<-function (ctlfile = "C:/myfiles/mymodels/myrun/control.ss_new", 
          newctlfile = "C:/myfiles/mymodels/myrun/control_modified.ss", linenums = NULL, strings = NULL, 
          newlos = NULL, newhis=NULL, estimate = FALSE, verbose = TRUE) 
{
  ctl = readLines(ctlfile)
  if (is.null(linenums) & !is.null(strings) & class(strings) == 
        "character") {
    ctltable <- SS_parlines(ctlfile = ctlfile)
    allnames <- ctltable$Label
    goodnames <- NULL
    browser()
    if (!is.null(strings)) {
      for (i in 1:length(strings)) goodnames <- c(goodnames, 
                                                  allnames[grep(strings[i], allnames,fixed=TRUE)])
      goodnames <- unique(goodnames)
      cat("parameter names in control file matching input vector 'strings' (n=", 
          length(goodnames), "):\n", sep = "")
      print(goodnames)
      if (length(goodnames) == 0) {
        stop("No parameters names match input vector 'strings'")
      }
    }
    nvals <- length(goodnames)
    cat("These are the ctl file lines as they currently exist:\n")
    print(ctltable[ctltable$Label %in% goodnames, ])
    browser()
    for (i in 1:nvals) linenums[i] <- ctltable$Linenum[ctltable$Label == 
                                                         goodnames[i]]
  }
  else {
    if (is.null(linenums)) 
      stop("valid input needed for either 'linenums' or 'strings'")
  }
  ctlsubset <- ctl[linenums]
  cat("line numbers in control file (n=", length(linenums), 
      "):\n", sep = "")
  print(linenums)
  newctlsubset <- NULL
  cmntvec <- NULL
  nvals <- length(linenums)
  oldlos <- oldhis <- oldphase <- newphase <- rep(NA, nvals)
  if (!is.null(newlos) & length(newlos) != nvals) {
    stop("'newlos' and either 'linenums' or 'strings' should have the same number of elements")
  }
  if (!is.null(newhis) & length(newhis) != nvals) {
      stop("'newhis' and either 'linenums' or 'strings' should have the same number of elements")
  }
  if (!(length(estimate) %in% c(1, nvals))) 
    stop("'estimate' should have 1 element or same number as 'newvals'")
  if (length(estimate) == 1) 
    estimate <- rep(estimate, nvals)
  if (is.data.frame(newlos)) 
    newlos <- as.numeric(newlos)
  if (is.data.frame(newhis)) 
    newhis <- as.numeric(newhis)
  if (is.null(newlos)) 
    stop("Nothing input for 'newlos'")
  if (is.null(newhis)) 
    stop("Nothing input for 'newhis'")
  for (i in 1:nvals) {
    splitline <- strsplit(ctlsubset[i], "#")[[1]]
    cmnt <- paste("#", paste(splitline[-1], collapse = "#"), 
                  sep = "")
    cmntvec <- c(cmntvec, cmnt)
    vecstrings <- strsplit(splitline[1], split = "[[:blank:]]+")[[1]]
    vec <- as.numeric(vecstrings[vecstrings != ""])
    if (max(is.na(vec)) == 1) 
      stop("There's a problem with a non-numeric value in line", 
           linenums[i])
    oldlos[i] <- vec[1]
    oldhis[i] <- vec[2]
    if ((!is.null(oldlos))&(!is.null(oldhis))) 
      vec[1] <- newlos[i]
      vec[2] <- newhis[i]
    oldphase[i] <- as.numeric(vec[7])
    if (estimate[i]) {
      vec[7] <- abs(oldphase[i])
    }
    else {
      vec[7] <- -abs(oldphase[i])
    }
    if (vec[1] > vec[3]) 
      cat("!warning: new lower bound ", vec[1], "is above initial value ", 
          vec[3], "for", cmnt, "\n")
    if (vec[1] > vec[2]) 
      cat("!warning: new lower bound ", vec[1], "is above upper bound ", 
          vec[2], "for", cmnt, "\n")
    newphase[i] <- vec[7]
    newline <- paste("", paste(vec, collapse = " "), cmnt)
    newctlsubset <- rbind(newctlsubset, newline)
  }
  newctl <- ctl
  newctl[linenums] <- newctlsubset
  writeLines(newctl, newctlfile)
  if (verbose) 
    cat("\nwrote new file to", newctlfile, "with the following changes:\n")
  results <- data.frame(oldlos, newlos, oldhis, newhis, oldphase, newphase, 
                        comment = cmntvec)
  if (is.null(newlos)) 
    newlos <- NA
  if (is.null(newhis))
    newhis <-NA
  if (verbose) 
    print(results)
  return(invisible(results))
}