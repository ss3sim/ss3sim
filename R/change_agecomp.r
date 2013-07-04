#' Change age comps
#'
#' Take a data.SS_new file, resample the age compositions from the expected values, and 
#' return a new file with the new age comp samples. Samples can have dimensions, bins, 
#' sample sizes, and distributions which are different than those coming from SS.
#' 
#' @author Roberto Licandeo, Felipe Hurtado-Ferro
#' @param infile SS data object from SS_readdat() in the r4ss package. Make sure you select option "section=2"
#' @param outfile Name of the new file to be created. May be global or local. Make sure to give extension .dat to the file name.
#' @param distribution Distribution to be used to sample the length compositions. Options are "multinomial" and "dirichlet"
#' @param Nsamp Number of samples drawn from a multinomial distribution, or precision for the Dirichlet distribution
#' this parameter could be a single value or a vector for each year of length comp data (both fishery and survey)
#' @param minyear starting year for the fleet age comps. Overridden by specifying "years"
#' @param maxyear ending year for the fleet age comps. Overridden by specifying "years"
#' @param N_agebins Number of age bins
#' @param agebin_vector A vector of age bins
#' @param years Vector of years for the fleet age comps
#' @param svyears Vector of years for the survey age comps
#' @param agecomp Matrix of age comps 
#' @param fish_agecomp, TRUE or FALSE; default to TRUE. To use or not to use the fishery age comp data 
#' @param sv_agecomp, TRUE or FALSE; default to TRUE. To use or not to use the survey age comp data
#' 
#' @export


change_agecomp <- function(infile,outfile,distribution="multinomial",Nsamp=NA,
                         minyear=NA,maxyear=NA,years=NA,svyears=NA,fish_agecomp=TRUE,sv_agecomp=TRUE,
                         N_agebins=NA,agebin_vector=NA,agecomp=NA,cpar=2){
  #Load required libraries (now done by package)
  #require(r4ss)
  #require(gtools)
  
  #Read the input file
 dat.file <- infile
  
  if(is.na(N_agebins)==FALSE){
    if(class(N_agebins)!="numeric")
      stop("N_agebins must have a numeric input")
    dat.file$N_agebins <- N_agebins #$N_agebins
  }
  
  if(is.na(agecomp)==FALSE){                       
    if(class(agecomp)!="numeric")
      stop("agecomp must have a numeric input")
    new.agecomp <- agecomp    
  }
  
  if(is.na(sum(Nsamp))==TRUE){
    Nsamp <- dat.file$agecomp[,"Nsamp"]
  }
  
  #Determine the length of the observation series
  if(is.na(sum(years))==TRUE){
    if(is.na(minyear)==FALSE & is.na(maxyear)==FALSE)
      years <- minyear:maxyear
    else
      years <- dat.file$agecomp[dat.file$agecomp[,"FltSvy"]==1,1] 
  }
  
  #Determine the length of the survey series
  if(is.na(sum(svyears))==TRUE){
    svyears <- dat.file$agecomp[dat.file$agecomp[,"FltSvy"]==2,1]
  }
    
  #Save the expected lencomps in another object to be modified (if necessary)
  init.agecomp <- dat.file$agecomp
    
  #Write the age comps
  DF.width <- length(init.agecomp[1,])     #Width for the comp matrix
  NDF.width <- length(dat.file$agebin_vector)+9
  
  if(is.na(agecomp)==TRUE){
    new.agecomp <- array(0,dim=c(length(c(years,svyears)),NDF.width))  ## create age matrix
    new.agecomp[,1] <- c(years,svyears)                                ## fill Yr
    new.agecomp[,2] <- 1                                               ## fill Seas      
    new.agecomp[1:length(years),3] <- 1                                ## fill FltSvy  =1    
    new.agecomp[(length(years)+1):length(c(years,svyears)),3] <- 2     ## fill FltSvy  =2
    new.agecomp[,4] <- 0                                               ## Gender (default = 0)
    new.agecomp[,5] <- 0                                               ## Part (default = 0)
    new.agecomp[,6] <- 1                                               ## Ageerr (default = 0)
    new.agecomp[,7] <- -1                                             ## Lbin_lo (default = -1)
    new.agecomp[,8] <- -1                                             ## Lbin_hi (default = -1)
    if (length(Nsamp) ==1) new.agecomp[,9] <- Nsamp
    flacomp <- subset(init.agecomp,init.agecomp[,3]==1)
    if(is.na(sum(years))==FALSE){
    for(it in 1:length(years)){                                   ## maybe here try matrix instead a for 
                           if (length(Nsamp)>1) new.agecomp[it,9] <- Nsamp[it]
      probs <- flacomp[flacomp[,1]==years[it],10:DF.width]
      probs <- as.numeric(probs)/sum(as.numeric(probs))
     if(distribution=="multinomial")
        new.agecomp[it,10:NDF.width] <- rmultinom(1,new.agecomp[it,9],probs)
      if(distribution=="dirichlet")    
        lambda <- Nsamp/cpar^2-1
        new.agecomp[it,10:NDF.width] <- gtools::rdirichlet(1,as.numeric(probs)*lambda)
    }}
    svagecomp <- subset(init.agecomp,init.agecomp[,3]==2)
    if(is.na(sum(svyears))==FALSE){
    for(it in (length(years)+1):length(c(years,svyears))){
	  if (length(Nsamp)>1) new.agecomp[it,9] <- Nsamp[it]
      probs <- svagecomp[svagecomp[,1]==c(years,svyears)[it],10:DF.width]
      probs <- as.numeric(probs)/sum(as.numeric(probs))
      if(distribution=="multinomial")
        new.agecomp[it,10:NDF.width] <- rmultinom(1,new.agecomp[it,9],probs)
      if(distribution=="dirichlet")
        lambda <- Nsamp/cpar^2-1
        new.agecomp[it,10:NDF.width] <- gtools::rdirichlet(1,as.numeric(probs)*lambda)
    }}
  }
  
  new.agecomp <- as.data.frame(new.agecomp)
  names(new.agecomp) <- c(names(dat.file$agecomp)[1:9],paste("a",dat.file$agebin_vector,sep=""))
  
 #To keep or not to keep the length comp from fishery and survey
  if(fish_agecomp==FALSE)
  {
    new.agecomp = subset(new.agecomp, subset=c(FltSvy!=1))
  }
  if(sv_agecomp==FALSE) 
  {
    new.agecomp = subset(new.agecomp, subset=c(FltSvy!=2))
  }

  dat.file$agecomp <- new.agecomp

  if(dim(new.agecomp)[1]==0) dat.file$agecomp = data.frame("#")
    
  #To calculate the final sum of years 
 
  dat.file$N_agecomp <- length(new.agecomp[,1])
 
  
  #Write the modified file
  r4ss::SS_writedat(datlist=dat.file, outfile=outfile, overwrite=TRUE)
}


