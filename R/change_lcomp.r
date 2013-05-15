#' Change length comps
#'
#' Take a data.SS_new file, resample the length compositions from the expected values, and 
#' return a new file with the new length comp samples. Samples can have dimensions, bins, 
#' sample sizes, and distributions which are different than those coming from SS.
#' 
#' @author Felipe Hurtado-Ferro
#' @param infile SS data object from SS_readdat() in the r4ss package. Make sure you select option "section=2"
#' @param outfile Name of the new file to be created. Path may be global or local. Make sure to give extension .dat to the file name.
#' @param distribution Distribution to be used to sample the length compositions. Options are "multinomial" and "dirichlet"
#' @param Nsamp Number of samples drawn from a multinomial distribution, or precision for the Dirichlet distribution
#' @param minyear, maxyear starting and ending year for the fleet length comps. Overridden by specifying "years"
#' @param years vector of years for the fleet length comps.
#' @param svyears vector of years for the survey lenght comps.
#' @param lbin_method method to generate model length bins. See SS manual for details
#' @param binwidth, minimum_size, maximum_size, N_lbins 
#' @param lbin_vector Vector of length bins for the observations
#' @param lencomp matrix of length comps 
#' 
#' @export


change_lcomp <- function(infile,outfile,distribution="multinomial",Nsamp=NA,
                        minyear=NA,maxyear=NA,years=NA,svyears=NA,
                        lbin_method=NA,binwidth=NA,minimum_size=NA,maximum_size=NA,
                        N_lbins=NA,lbin_vector=NA,lencomp=NA,...){

  #Load required libraries (done via package)
  #require(MCMCpack)
  #require(r4ss)
  
  #Read the input file
  #dat.file <- SS_readdat(file=infile, section=2)
  dat.file <- infile
  
  #Explicit inputs
  if(is.na(lbin_method)==FALSE){
    if(class(lbin_method)!="numeric")
      stop("lbin_method must have a numeric input")
    dat.file$lbin_method <- lbin_method
  }
  if(is.na(binwidth)==FALSE){
    if(class(binwidth)!="numeric")
      stop("binwidth must have a numeric input")
    dat.file$binwidth <- binwidth
  }
  if(is.na(minimum_size)==FALSE){
    if(class(minimum_size)!="numeric")
      stop("minimum_size must have a numeric input")
    dat.file$minimum_size <- minimum_size
  }
  if(is.na(maximum_size)==FALSE){
    if(class(maximum_size)!="numeric")
      stop("maximum_size must have a numeric input")
    dat.file$maximum_size <- maximum_size
  }
  if(is.na(N_lbins)==FALSE){
    if(class(N_lbins)!="numeric")
      stop("N_lbins must have a numeric input")
    dat.file$N_lbins <- N_lbins
  }
  if(is.na(lencomp)==FALSE){
    if(class(lencomp)!="numeric")
      stop("lencomp must have a numeric input")
    new.lencomp <- lencomp
  }
  if(is.na(sum(svyears))==FALSE){  
    if(class(svyears)!="numeric") 
      stop("svyears must have a numeric input")   
  }
  if(is.na(Nsamp)==TRUE){
    Nsamp <- dat.file$lencomp[,6]
  }
  
  #Determine the length of the observation series
  if(is.na(sum(years))==TRUE){
    if(is.na(minyear)==FALSE & is.na(maxyear)==FALSE)
      years <- minyear:maxyear
    else
      years <- dat.file$lencomp[dat.file$lencomp[,"FltSvy"]==1,1]
  }

  #Determine the length of the survey series
  if(is.na(sum(svyears))==TRUE){
    svyears <- dat.file$lencomp[dat.file$lencomp[,"FltSvy"]==2,1]
  }

  #Save the expected lencomps in another object to be modified (if necessary)
  init.lcomp <- dat.file$lencomp
  
  #Check the length of the lbin_vector and adjust if necessary
  if(is.na(sum(lbin_vector))==FALSE){
    if(class(lbin_vector)!="numeric")
      stop("lbin_vector must have a numeric input")
    if(length(dat.file$lbin_vector)!=length(lbin_vector)){
      minobsl <- match(min(lbin_vector),dat.file$lbin_vector)
      maxobsl <- match(max(lbin_vector),dat.file$lbin_vector)
      init.lcomp <- cbind(init.lcomp[,1:6],
                          apply(init.lcomp[,7:(6+minobsl)],1,sum),
                          init.lcomp[,(7+minobsl):(maxobsl+5)],
                          apply(init.lcomp[,(maxobsl+5):length(init.lcomp[1,])],1,sum))
    }
    dat.file$lbin_vector <- lbin_vector
  }
  
  #Write the length comps
  DF.width <- length(init.lcomp[1,])
  NDF.width <- length(dat.file$lbin_vector)+6
  
  if(is.na(lencomp)==TRUE){
    new.lencomp <- array(0,dim=c(length(c(years,svyears)),NDF.width))
    new.lencomp[,1] <- c(years,svyears)
    new.lencomp[,2] <- 1
    new.lencomp[1:length(years),3] <- 1
    new.lencomp[(length(years)+1):length(c(years,svyears)),3] <- 2
    new.lencomp[,4] <- 0
    new.lencomp[,5] <- 0
    new.lencomp[,6] <- Nsamp
    fllcomp <- subset(init.lcomp,init.lcomp[,3]==1)
    for(it in 1:length(years)){
      probs <- fllcomp[fllcomp[,1]==years[it],7:DF.width]
      if(distribution=="multinomial")
        new.lencomp[it,7:NDF.width] <- rmultinom(1,new.lencomp[it,6],probs)
      if(distribution=="dirichlet")
        new.lencomp[it,7:NDF.width] <- MCMCpack::rdirichlet(1,as.numeric(probs)*Nsamp)
    }
    svlcomp <- subset(init.lcomp,init.lcomp[,3]==2)
    for(it in (length(years)+1):length(c(years,svyears))){
      probs <- svlcomp[svlcomp[,1]==c(years,svyears)[it],7:DF.width]
      if(distribution=="multinomial")
        new.lencomp[it,7:NDF.width] <- rmultinom(1,new.lencomp[it,6],probs)
      if(distribution=="dirichlet")
        new.lencomp[it,7:NDF.width] <- MCMCpack::rdirichlet(1,as.numeric(probs)*Nsamp)
    }
  }
  
  new.lencomp <- as.data.frame(new.lencomp)
  names(new.lencomp) <- c(names(dat.file$lencomp)[1:6],paste("l",dat.file$lbin_vector,sep=""))
  
  dat.file$lencomp <- new.lencomp
  
  dat.file$N_lencomp <- length(new.lencomp[,1])
  
  
  #Write the modified file
  r4ss::SS_writedat(datlist=dat.file, outfile=outfile, overwrite=TRUE)
}

