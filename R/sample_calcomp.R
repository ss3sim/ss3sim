#' @title Sample conditional age-at-length (CAL) data and write to file for use by the
#' EM.
#' 
#' @description Take a \code{data.SS_new} file containing expected values and sample
#'   from true lengths, using length comp sample sizes, to get realistic sample
#'   sizes for age bins given a length. Only the multinomial distribution is
#'   currently implemented. xIf no fish are sampled then that row is discarded.
#'   A value of NULL for fleets indicates to delete the data so the EM If used
#'   with \code{\link{run_ss3sim}} the case file should be named \code{calcomp}.
#'   
#' @note This function is only reliable when using multinomial length 
#'   compositions for the matching fleet. The real-valued length compositions 
#'   resulting from the Dirichlet distribution cause difficulties in the 
#'   sampling code. See the vignette for more. Use the option that samples
#'   lengths in schools to sample ages from fish aggregations.
#'   
#' @author Gwladys Lambert; modified from a version by Cole Monnahan and Kotaro
#'   Ono
#'   
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template sampling-return
#' @template casefile-footnote
#' @param Nsamp Total number of age samples. The default will be to sample a
#'   proportional number of ages per length bin (i.e. proportional to the
#'   sampled number of lengths in that bin). Alternatively, if \code{"method"} 
#'   is NULL and \code{"fixed.number"} is TRUE, then \code{"Nsamp"} is the exact 
#'   number of ages to sample per length bin. If the \code{"method"} is "SRS", 
#'   i.e. Simple Random Sampling, or "length-strat",then the lengths should 
#'   have been sampled using the schooling option. See \code{\link{sample_lcomp}}. 
#'   \code{"Nsamp"} is the exact number of age samples to take from each haul
#'   when using "SRS", or the exact number of age samples to take from each
#'   length bin when using "length-strat".
#' @param Nhauls Number of hauls to sample age data from. This can only be used
#'   if the lengths were sampled using the schooling option and must be used
#'   with \code{"method"} = "SRS" or "length-strat". See
#'   \code{\link{sample_lcomp}}.
#' @param fixed.number Using the default multinomial length sampling option,
#'   should age data be sampled by fixed or proportional allocation? Default is
#'   FALSE for proportional. TRUE is for fixed number of ages at length.
#' @param method list of \code{"SRS"} for simple random sampling and/or
#'   \code{"length-strat"} for length-stratified approach. List along fleets.
#'   Default is NULL. If "SRS" or "length-strat" is used then the lengths have
#'   to have been sampled using the schooling option. See
#'   \code{\link{sample_lcomp}}.
#' @param hauls Data file of sampled lengths per haul, extracted from
#'   \code{\link{sample_lcomp}} This is only required if the age sampling method
#'   is \code{"SRS"} or \code{"length-strat"}.
#' @param rw NULL by default but use "iterative" for iterative re-weighing using
#'   McAllister and Ianelli method. NOTE - This option could easily be re-written 
#'   to give the choice between McA&I or Francis re-weighting.
#' @param fit.on.agecomp Default is FALSE. TRUE means the EM is to fit 
#'   on marginal age composition rather than conditional age-at-length. If using 
#'   this option, marginal composition is simply estimated by multiplying 
#'   conditional age-at-length with length proportions. This option could be 
#'   further developped to include ways to deal with missing age data (i.e. lengths
#'   present in the length sample but not in the age-at-length sample).
#' @param ctl Path to the .ctl file in case of fitting on marginal age
#'   composition, as the weight on both age and length data will have to be
#'   halved in the .ctl file (as information on length used twice).
#' @param ageplus if not NULL, this is the new age-plus group to use for the
#'   sampled (observed) data. This option will be activated at the end of the
#'   script to sum up all age compositions above ageplus into one age group. The
#'   original age data composition will be saved in case of sampling
#'   weight-at-age data in the scenario, as weight-at-age should cover the full
#'   population age range.
#' @param percent_read Number of otoliths collected vs read. This will randomly
#'   subsample a percentage of the total number of ages sampled from the design
#'   described. This option was included to mimic the process of ageing in the
#'   lab when all otoliths collected at sea can not be processed. The default
#'   value is 100, i.e. all otoliths collected are aged.
#' @param max_ss Similar to \code{percent_read} This will randomly
#'   subsample a number of ages equal to \code{max_ss}. This option was included 
#'   to mimic the process of ageing in the lab when all otoliths collected at sea 
#'   can not be processed. The default value is NULL, i.e. all otoliths collected 
#'   are aged.
#' @param ESS Define effective sample size for age data. Has to be a list of the
#'   same length as the number of fleets. Can be a single value or a vector of
#'   length equal to the number of years. Can also be a character, i.e. "hauls",
#'   in which case the number of hauls sampled from will be used as ESS, if
#'   using the option to sample in schools.
#' @param change_ess_now If ESS has to be changed at this stage or by the end of
#'   the sampling process in \code{\link{ss3sim_base}}. Default is NULL, meaning
#'   that it will be changed here (would do the same with TRUE). Set to FALSE to
#'   keep the original sample size until the end of the sampling process in 
#'   \code{\link{ss3sim_base}}.
#' @param random_gen If \code{years} are randomly generated, give the fleet
#'   number(s) for which they are. Default is NULL. THIS WS JUST A TRIAL - 
#'   NOT IMPORTANT, COULD/SHOULD BE REMOVED.
#' @return If the option \code{ageplus} is activated, it returns a list of 2 objects. 
#' The first one is the .dat file described above, the second one is the agecomp 
#' element and the age vector prior to using the age plus group. This is because 
#' the weight at age sampling requires age comps that use the same max age as the 
#' max age of the pop. 
#' 
#' @family sampling functions
#' @importFrom plyr ddply
#' @importFrom dplyr sample_n
#' @importFrom Hmisc %nin%
#' @export
#'   
#' @examples
#'  \dontrun{
#'  # To sample randomly, using a multinomial distribution, use sample_agecomp
#'  # sample_calcomp creates proportional number of ages-at-length by default
#'  
#'  d <- system.file("extdata", package = "ss3sim")
#'  f_in <- paste0(d, "/models/cod-om/codOM.dat")
#'  dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#'  dat_list <- change_fltname(dat_list)
#'  
#'  # Turn the marginal age data of teh example into conditional age data
#'  dat_list_temp <- change_data(dat_list= dat_list, outfile="test1.txt", 
#'                            fleets           = c(1,2),                  
#'                            years            = c(26:100), 
#'                            types            =  c("index","len","cal"),   
#'                            write_file = F)
#' dat_list <- dat_list_temp
#' head(dat_list$agecomp)
#' tail(dat_list$agecomp)
#'                            
#' ## Turn off age comps by specifying fleets=NULL
#' sample_calcomp(dat_list=dat_list, outfile="test1.dat",
#'              fleets=NULL, Nsamp=list(5,5),
#'              years=list(95, 95), write_file=FALSE)
#' ## Generate with a smaller number of fleet taking samples
#' ex1 <- sample_calcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(2),
#'                     Nsamp=list(c(1,5)), years=list(c(26,27)),
#'                     write_file=FALSE)
#' 
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- sample_calcomp(dat_list=dat_list, outfile="test2.dat", fleets=c(1,2),
#'                       Nsamp=list(c(rep(5, 5), rep(2, 5)), 6),
#'                       years=list(seq(26, 44, 2),
#'                                  c(26:100)), write_file=FALSE)
#' tail(ex2$agecomp, 20)
#' 
#'  ## Testing sampling a fixed number of ages-at-length
#'  dat_list$lencomp$Nsamp <- 10000
#'  ex3 <- sample_calcomp(dat_list=dat_list, outfile="test3.dat", fleets=c(1,2),
#'                        Nsamp=list(20, 20), 
#'                        years=list(seq(26, 44, 2),  c(26:100)),
#'                        fixed.number = TRUE,
#'                        write_file=FALSE)
#'  head(ex3$agecomp)
#'  tail(ex3$agecomp)
#'  
#'  
#'  ## Testing sampling either random (SRS) or length-stratified 
#'  ## (fixed number at length) 
#'  ## with length schooling option
#'  
#'  # First generate shooling length data
#'  
#'  library(doParallel)
#'  library(compiler)
#'  cores=7
#'  assign(".lib.loc", .libPaths()[1], envir = environment(.libPaths)) 
#'  cl<-makeCluster(cores)
#'  clusterCall(cl, function(x) .libPaths(x), .libPaths())
#'  registerDoParallel(cl)
#'  
#'  ex4 <- sample_lcomp(dat_list=dat_list, outfile="test4.dat", fleets=c(1,2),
#'                      Nsamp=list(100,50), years=list(seq(94, 100, by=2), 95:100),
#'                      Nhauls= list(100,100), cl_factor = 1,
#'                      vals_at_cutoff = 0.15, plot.schools = FALSE,
#'                      write_file = FALSE, parallel_lgths = TRUE)
#'  dat_list <- ex4[[1]]
#'  
#'  # then apply SRS
#'  ex4a1 <- sample_calcomp(dat_list=dat_list, outfile="test4a.dat", fleets=c(1,2),
#'                          Nsamp=list(10, 10), years=list(seq(94, 100, by=2), 
#'                          95:100), method = list("SRS","SRS"), Nhauls = 
#'                          list(100,100), hauls = ex4[[2]],
#'                          write_file=FALSE, percent_read = c(100,100))
#'  # changing percent read
#'  ex4a2 <- sample_calcomp(dat_list=dat_list, outfile="test4a.dat", fleets=c(1,2),
#'                          Nsamp=list(10, 10), years=list(seq(94, 100, by=2), 
#'                          95:100), method = list("SRS","SRS"), Nhauls = 
#'                          list(100,100), hauls = ex4[[2]],
#'                          write_file=FALSE, percent_read = c(100,50))
#'  # Length stratified
#'  ex4b <- sample_calcomp(dat_list=dat_list, outfile="test4a.dat", fleets=c(1,2),
#'                         Nsamp=list(10, 10), years=list(seq(94, 100, by=2), 
#'                         95:100), method = list("length-strat","length-strat"), 
#'                         Nhauls = list(100,100), hauls = ex4[[2]],
#'                         write_file=FALSE)
#'  # Other options
#'  # Set up file so that ESS will be the number of hauls sampled from
#'  ex4c_LS  <- sample_calcomp(dat_list=dat_list, outfile="test4a.dat", 
#'                             fleets=c(1,2), Nsamp=list(10, 10), 
#'                             years=list(seq(94, 100, by=2), 95:100),
#'                             method = list("length-strat","length-strat"), 
#'                             Nhauls = list(100,100), hauls = ex4[[2]],
#'                             ESS = "hauls",  write_file=FALSE)
#'  ex4c_SRS <- sample_calcomp(dat_list=dat_list, outfile="test4a.dat", 
#'                             fleets=c(1,2), Nsamp=list(10, 10), 
#'                             years=list(seq(94, 100, by=2), 95:100),
#'                             method = list("SRS","SRS"), Nhauls = list(100,100), 
#'                             hauls = ex4[[2]], ESS = list("hauls","hauls"), 
#'                             write_file=FALSE)
#'  # Define an age-plus group - useful if using weight at age as weight are required 
#'  # at the population level
#'  # You will have to use the ageplus option in sample_mla too for weight-at-age
#'  ex4d      <- sample_calcomp(dat_list=dat_list, outfile="test4d.dat", 
#'                              fleets=c(1,2), Nsamp=list(10, 10), 
#'                              years=list(seq(94, 100, by=2), 95:100),
#'                              method = list("SRS","SRS"), Nhauls = list(100,100), 
#'                              hauls = ex4[[2]],  ESS = list("hauls","hauls"), 
#'                              ageplus= 10, write_file=FALSE)
#'  # Set up file so that the fit will be on marginal age data instead of 
#'  # conditional age-at-length
#'  # This requires to change the ctl_file as well, as the variance adjustment 
#'  # will be set at 0.5 
#'  # (i.e. using length and age data twice)
#'  # A warning shows if there is no ctl file attached to the function 
#'  # (there will be a .ctl file when running the full simulation)
#'  d <- system.file("extdata", package = "ss3sim")
#'  #ctl_f_in <- paste0(d, "/models/cod-om/codOM.ctl")
#'  ex4e <- sample_calcomp(dat_list=dat_list, outfile="test4e.dat", fleets=c(1,2),
#'                         Nsamp=list(10, 10), years=list(seq(94, 100, by=2), 
#'                         95:100), method = list("length-strat","length-strat"), 
#'                         Nhauls = list(100,100), hauls = ex4[[2]],
#'                         fit.on.agecomp = TRUE,  write_file=FALSE)
#'                         
#'                         
#'  ## Run cases showing Multinomial, Dirichlet(1) and over-dispersed
#'  ## Dirichlet for different levels of sample sizes
#'  op <- par(mfrow = c(1,3))
#'  for(samplesize in c(45, 90, 180)){
#'    ex5 <- sample_calcomp(dat_list=dat_list, outfile="test5.dat", fleets=c(1,2),
#'                          Nsamp=list(samplesize, samplesize),
#'                          write_file = FALSE,
#'                          years=list(94,95)
#'    )
#'    # decrease sample size for other options as meaning is different
#'    samplesize1 <- samplesize/45 # 45 is the number of length bins
#'    ex6 <- sample_calcomp(dat_list=dat_list, outfile="test6.dat", fleets=c(1,2),
#'                          Nsamp=list(samplesize1, samplesize1),
#'                          write_file = FALSE, fixed.number = TRUE,
#'                          years=list(94,95)
#'    )
#'    samplesize1 <- samplesize/5
#'    ex7 <- sample_calcomp(dat_list=dat_list, outfile="test7.dat", fleets=c(1,2),
#'                          Nsamp=list(samplesize1, samplesize1),
#'                          write_file = FALSE,
#'                          years=list(94,95),
#'                          method = "SRS",
#'                          Nhauls = list(5,5), hauls = ex4[[2]]
#'    )
#'    samplesize1 <- samplesize/45
#'    ex8 <- sample_calcomp(dat_list=dat_list, outfile="test8.dat", fleets=c(1,2),
#'                          Nsamp=list(samplesize1, samplesize1),
#'                          write_file = FALSE,
#'                          years=list(94,95),
#'                          method = list("length-strat","length-strat"),
#'                          Nhauls = list(50,50), hauls = ex4[[2]]
#'    )
#'    # true <- subset(dat_list$agecomp, FltSvy==-1 & Yr == 94)[-(1:9)]
#'    true <- dat_list$agecomp[1, -(1:9)]
#'    true <- true/sum(true)
#'    plot(1:15, subset(ex5[[1]]$agecomp, FltSvy==-1)[1,-(1:9)], type="b", 
#'         ylim=c(0,0.3),
#'         col=1, xlab="Age", ylab="Proportion", main=paste("Sample size=",
#'                                                          samplesize))
#'    legend("topright", legend=c("Proportional at length", "Fixed at length",
#'                                "SRS on schooling data", "Fixed at length on 
#'                                schooling data","Truth"),
#'           lty=1, col=1:5)
#'    lines((1:15), subset(ex6[[1]]$agecomp, FltSvy==-1)[1,-(1:9)], type="b", col=2)
#'    lines((1:15), subset(ex7[[1]]$agecomp, FltSvy==-1)[1,-(1:9)], type="b", col=3)
#'    lines((1:15), subset(ex8[[1]]$agecomp, FltSvy==-1)[1,-(1:9)], type="b", col=4)
#'    lines((1:15), true, col=5, lwd=2)
#'  }
#'  par(op)
#' }
#' 


sample_calcomp <- function(dat_list, outfile, fleets = c(1,2), years, Nsamp, Nhauls=NULL, hauls=NULL, 
                           method=NULL, fixed.number=NULL, ageplus= NULL, percent_read=c(100,100), 
                           max_ss=NULL, fit.on.agecomp=NULL, ESS = NULL, change_ess_now=NULL, rw=NULL, 
                           write_file=TRUE, ctl = NULL, random_gen=NULL){
  
  ## The samples are taken from the expected values, where the
  ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
  ## Lbin_hi, so subset those out, but don't delete the age values since
  ## those are already sampled from, or might be sampled later so need to
  ## leave them there.
  ## Input checks
  
  ###############################
  ### PREPARATION AND CHECKS ###
  ###############################
  
  Nfleets <- NROW(fleets)
  if (Nfleets>0){
    for(i in 1:Nfleets){
      if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
        stop(paste0("Length of Nsamp does not match length of years for",
                    "fleet ",fleets[i]))
    }
  }
  
  check_data(dat_list)
  agecomp.age <- dat_list$agecomp[dat_list$agecomp$Lbin_lo== -1,]
  agecomp.cal <- dat_list$agecomp[dat_list$agecomp$Lbin_lo != -1,]
  lencomp     <- dat_list$lencomp
  lbin_vector <- dat_list$lbin_vector
  newfile     <- dat_list
  
  ## THIS WILL BE USED ONLY IF LENGTHS WERE SAMPLED BY AGGREGATION/SCHOOLING
  if (!is.null(hauls)){
    #prepare haul vector into a wide format dataframe
    #tidy by removing unuseful columns
    hauls$LENGTH <- as.numeric(as.character(hauls$LENGTH))
    hauls[hauls$LENGTH<min(lbin_vector), "LENGTH"] <- min(lbin_vector)
    hauls[hauls$LENGTH>max(lbin_vector), "LENGTH"] <- max(lbin_vector)
    if (dat_list$Ngenders == 1) {
     hauls <- aggregate(Nb ~ Yr+Seas+FltSvy+Gender+Part+Nsamp+LENGTH+Haul, hauls, sum)
    } else {
      hauls <- aggregate(Nb ~ Yr+Seas+FltSvy+Gender+Part+Nsamp+LENGTH+Haul+sex, hauls, sum)    
    }
    hauls$mult <- NULL
    hauls2 <- hauls[order(hauls$LENGTH),]
    if (dat_list$Ngenders == 1) {
      hauls2$ID <- paste(hauls2$Yr,hauls2$FltSvy,hauls2$Gender,hauls2$Haul, sep=".")
      hauls2 <- reshape(hauls2, v.names="Nb", timevar="LENGTH", idvar="ID", direction="wide")
      hauls2[is.na(hauls2)] <- 0
    } else {
      hauls2$Gender<- hauls2$sex
      hauls2$sex <- NULL
      hauls_f <- hauls2[hauls2$Gender==1,]
      hauls_f$Gender =3
      hauls_f$ID <- paste(hauls_f$Yr,hauls_f$FltSvy,hauls_f$Gender,hauls_f$Haul, sep=".")
      hauls_f <- reshape(hauls_f, v.names="Nb", timevar="LENGTH", idvar="ID", direction="wide")
      names(hauls_f)[-c(1:8)] <- paste("f",names(hauls_f)[-c(1:8)], sep=".")
      hauls_m <- hauls2[hauls2$Gender==2,]
      hauls_m$Gender =3
      hauls_m$ID <- paste(hauls_m$Yr,hauls_m$FltSvy,hauls_m$Gender,hauls_m$Haul, sep=".")
      hauls_m <- reshape(hauls_m, v.names="Nb", timevar="LENGTH", idvar="ID", direction="wide")
      names(hauls_m)[-c(1:8)] <- paste("m",names(hauls_m)[-c(1:8)], sep=".")
      hauls2 <- merge(hauls_f,hauls_m, all=T)
      hauls2[is.na(hauls2)] <- 0
    }
    for (idx_temp in unique(hauls2$FltSvy)){
      nhauls_lgth <- unique(aggregate(Haul ~ Yr + FltSvy + Seas, hauls, function(x) length(unique(x)))[,c(2,4)])$Haul
      idx_temp2 <- which(fleets==idx_temp)
      if(length(idx_temp2)!= 0){
        Nhauls[[which(fleets==idx_temp)]] = ifelse(Nhauls[[which(fleets==idx_temp)]] <= nhauls_lgth[which(fleets==idx_temp)], Nhauls[[which(fleets==idx_temp)]], nhauls_lgth[which(fleets==idx_temp)])
        nyrs_lgth <- unique(cbind.data.frame(Yr=hauls$Yr, FltSvy=hauls$FltSvy))
        yrs <- which( years[[which(fleets==idx_temp)]] %nin% nyrs_lgth$Yr[nyrs_lgth$FltSvy==idx_temp])
        if (length(yrs) != 0) years[[which(fleets==idx_temp)]] = years[[which(fleets==idx_temp)]] [-yrs]
      }
    }
  }
  
  ## A value of NULL for fleets indicates not to sample and strip out the data from the file.
  if(is.null(fleets)){
    newfile$agecomp <- agecomp.age
    newfile$N_agecomp <- nrow(agecomp.age)
    if(write_file)
      SS_writedat(datlist = newfile, outfile = outfile,
                  overwrite = TRUE, verbose=FALSE)
    return(invisible(newfile))
  }
  ## If not, do argument checks
  if(nrow(agecomp.cal)==0)
    stop("No conditional age-at-length data found")
  ## if(nrow(agecomp.age)==0)
  ##     stop("No agecomp data found -- something is wrong with sampling inputs")
  #Nfleets <- length(fleets)
  ## changed this from .cal to .age
  if(any(!fleets %in% unique(abs(agecomp.cal$FltSvy))))
    stop(paste0("The specified fleet number: ",fleets, " does not match input file"))
  if(class(years) != "list" | length(years) != Nfleets)
    stop("years needs to be a list of same length as fleets")
  ## End input checks
  
  ##################################
  ### BEGIN LOOP OF AGE SAMPLING ###
  ##################################
  
  ## The general approach here is to loop through each fl/yr combination
  ## (for which there will be a row for each length bin) and then
  ## recombine them later.
  newcomp.list     <- list() # temp storage for the new rows
  newcomp.len.list <- list() # temp storage for the new rows
  k <- 1                     # each k is a new row of data, to be rbind'ed later
  
  ## This is for statified length sampling with a proportional number per length bin
  
  ## Loop through each fleet
  for(i in 1:length(fleets)){
    fl <- fleets[i]
    if (length(Nsamp[[i]]) == 1) {
      Nsamp[[i]] <- rep(Nsamp[[i]], length(years[[i]]))
    }
    agecomp.cal.fl <- agecomp.cal[abs(agecomp.cal$FltSvy) == fl & agecomp.cal$Yr %in% years[[i]], ]
    
    # define newcomp for length so ESS can be changed later
    # lencomp.fl <- lencomp[abs(lencomp$FltSvy) == fl & lencomp$Yr %in% years[[i]], ]
    
    if(length(years[[i]]) != length(unique((agecomp.cal.fl$Yr))))
      stop(paste("A year specified in years was not found in the","input file for fleet", fl))
    
    ## Only loop through the subset of years for this fleet
    
    for(yr in years[[i]]) {
      
      newcomp <- agecomp.cal.fl[agecomp.cal.fl$Yr==yr, ]
      
      if(nrow(newcomp) != length(lbin_vector))
        stop(paste("number of length bins does not match calcomp data: fleet", fl, ", year", yr))
      if(NROW(newcomp) == 0) stop("no age data found")
      
      ## Get the sample sizes of the length comps.
      Nsamp.len <- lencomp$Nsamp[lencomp$Yr==yr & abs(lencomp$FltSvy)==fl]
      
      
      #############################################################################
      ### EXTRACT THE PROBABILITY AT LENGTH FOR THIS YEAR AND FLEET (/PER HAUL) ###
      #############################################################################
      
      ## Probability distribution for length comps
      ##  prob.len can be defined otherwise if on a per haul basis 
      if (!is.null(method) && method[[i]] %in% c("SRS","length-strat")){ #### PROBLEM HERE - LENGTH STRAT SHOULD NOT LIMIT THE NUMBER OF TOWS DATA COME FROM
        prob.len <- hauls2[hauls2$Yr==yr & abs(hauls2$FltSvy)==fl, -(1:8)]
        # keep just the number of tows necessary for age sampling
        prob.len <- prob.len[sample(1:nrow(prob.len), size=Nhauls[[i]], replace =F),]
      } else {
        prob.len <- as.numeric(lencomp[lencomp$Yr==yr & abs(lencomp$FltSvy)==fl, -(1:6)])
      }
      # Here prob.len will be a single vector if the length sampling was not done using the schooling option
      # If the schooling option was used and "hauls" is defined, then there will be a length distrib per haul
      
      if(any(is.na(prob.len))) stop("Invalid length probs in sample_calcomp -- likely due to missing length data")
      
      ## From observed length distribution, sample which fish to age.
      yr.ind <- which(years[[i]]==yr)
      if(Nsamp[[i]][yr.ind] > Nsamp.len)
        stop("More age samples specified than fish collected for calcomps")
      
      
      ############################################################################
      ###  SELECT AND AGE FISH WHEN NO SCHOOLING AND LENGTH STRATIFIED DESIGN ###
      ############################################################################
      
      ## Here if we did not use schooling there will be just one vector of length distribution
      if (is.null(dim(prob.len)))
      {        
        if(any(prob.len>1)){ #################################################################### MAKE SURE SAMPLE_LCOMP MULTINOMIAL STAYED THE SAME, IE NUMBERS AND NOT PROBS
          ## case of multinomial. This code creates a vector of
          ## empirical samples of length, such that each length bin
          ## is repeated equal to the number of observed fish in that bin
          prob.len.ints <- unlist(sapply(1:length(prob.len), function(i) rep(i, prob.len[i])))
          ## Now resample from it, garaunteeing that the sample size
          ## doesn't exceed
          ## !!! this is for limiting numbers in PROPORTIONAL allocation!!!
          
          #################################################
          ###  SS3SIM DEFAULT - PROPORTIONAL ALLOCATION ###
          #################################################
          
          if (is.null(fixed.number)){
            temp <- sample(x=prob.len.ints, size=Nsamp[[i]][yr.ind], replace=FALSE) #
            Nsamp.ages.per.lbin <- sapply(1:length(prob.len), function(i) sum(temp==i))
          }
          ## !!! this is for limiting numbers in FIXED allocation !!!
          
          ###############################
          ###  NEW - FIXED ALLOCATION ###
          ###############################
          
          if (!is.null(fixed.number)){
            Nsamp.ages.per.lbin <- ifelse(prob.len < Nsamp[[i]][yr.ind], prob.len, Nsamp[[i]][yr.ind]) #[yr.ind]
          }
        } else {  #################################################################### MAKE SURE SAMPLE_LCOMP MULTINOMIAL STAYED THE SAME, IE NUMBERS AND NOT PROBS
          ## case of overdispersed:
          Nsamp.ages.per.lbin <- rmultinom(n=1, size=Nsamp[[i]][yr.ind] , prob=prob.len) #[yr.ind]
          
          ###############################
          ### NEW - FIXED ALLOCATION  ###
          ###############################
          
          if (!is.null(fixed.number)){
            Nsamp.lengths.per.lbin <- rmultinom(n=1, size=Nsamp.len, prob=prob.len)
            Nsamp.ages.per.lbin <- ifelse(Nsamp.lengths.per.lbin < Nsamp[[i]][yr.ind], Nsamp.lengths.per.lbin, Nsamp[[i]][yr.ind]) 
          }
        }
        
        if(any(is.na(Nsamp.ages.per.lbin)))
          stop("Invalid age sample size for a length bin in calcomp")
        ## This is where the actual sampling takes place
        ## Loop through each length bin and sample fish in
        ## each age bin, given expected conditional age-at-length
        newcomp$Nsamp <- Nsamp.ages.per.lbin ############################################# CHANGE Nsamp
        for(ll in 1:nrow(newcomp)){
          N.temp <- newcomp$Nsamp[ll]
          if(N.temp>0){
            cal.temp <- rmultinom(n=1, size=Nsamp.ages.per.lbin[ll],prob=as.numeric(newcomp[ll,-(1:9)]))
          } else {
            cal.temp <- -1
          }
          ## Write the samples back, leaving the other columns
          newcomp[ll,-(1:9)] <- cal.temp
        }
        ## Drpo the -1 value which were temp placeholders
        newcomp <- newcomp[newcomp$Nsamp>0,]
        newcomp.list[[k]] <- newcomp
        
        ## Replacing ESS in length comp data if needed
        #if (!is.null(ESS.len)) newcomp.len$Nsamp <-  ESS.len[[i]][which(years[[i]]==yr)]
        # newcomp.len.list[[k]] <- newcomp.len
        
        k <- k+1
        
      } else {
        
        ###################################################
        ###  SELECT AND AGE FISH WHEN SCHOOLING AND SRS ###
        ###################################################
        
        if (!is.null(method[[i]]) && method[[i]]=="SRS"){ 
          #   if (!is.null(dim(prob.len))){ 
          if(is.null(dim(prob.len)))
            stop("The length data have to be sampled using the schooling option for SRS to run")
          newcomp.long.sample <- NULL
          
          # kk is the index for haul number
          # this runs haul by haul - sampling a defined number of ages in each
          
          for (kk in 1:nrow(prob.len)) {
            
            fem <- NULL
            mal <- NULL
            Nsamp.ages.per.lbin <- NULL
            newcomp1<- NULL
            cal.temp<- NULL
            agebin_vec<- NULL
            newcomp.long <- NULL
            max.nb <- NULL
            Nsamp.ages <- NULL
            newcomp.long.sample_temp <- NULL
            
            #### First age all the fish that was lengthed
            
            newcomp1 <- newcomp
            # take the full number of ages - one age per length
            
            # if sex gender = 3, need to separate fem from mal
            if (newcomp1$Gender[1]==3)
            {
              fem <- newcomp1[,1:c(9+c(ncol(newcomp1)-9)/2)]
              mal <- newcomp1[,c(1:9,c(ncol(fem)+1):ncol(newcomp1))]
              fem$Gender <- 1
              mal$Gender <- 2
              names(mal) <- names(fem)
              newcomp1 <- rbind(fem,mal)
            } 
            
            Nsamp.ages.per.lbin <- unlist(prob.len[kk,])
            
            sizes <- do.call(rbind, strsplit(names(prob.len[kk,]),"[.]"))
            sizes <- as.numeric(sizes[,ncol(sizes)])
            gen <- as.numeric(as.factor(do.call(rbind, strsplit(names(prob.len[kk,]),"[.]"))[,1]))
            if (length(unique(gen))==1) gen <- 0
            sizes_samp <- cbind.data.frame(Lbin_lo=sizes, Nsamp.ages.per.lbin, Gender= gen)
            
            # If length bins do not match bins in the conditonal age at length
            # need to change that here
            # Caution: I don't know if it is allowed in SS anyway that the 
            # conditional age at length is not the same size bins as the length comp...
            # either way that will not affect the simulations but maybe step that should not be needed...
            # ADDED IN AT THE LAST MINUTE SO IF IT FAILS IN MAIN SIMULATIONS THAT MIGHT BE THE PROBLEM

            gender_save <- sizes_samp$Gender[1]
            temp_vec <- sort(unique(newcomp1$Lbin_lo))
            sizes_samp$Lbin_lo   <- cut(sizes_samp$Lbin_lo, c(0,temp_vec, c(max(temp_vec)+1)), include.lowest = F, right=F)
            levels(sizes_samp$Lbin_lo) <- c(temp_vec[1],temp_vec)
            sizes_samp$Lbin_lo       <- as.numeric(as.character(sizes_samp$Lbin_lo))
            sizes_samp               <- aggregate(Nsamp.ages.per.lbin ~ Lbin_lo, sizes_samp, sum)
            sizes_samp$Gender        <- gender_save
            temp_Nsamp.ages.per.lbin <- sizes_samp$Nsamp.ages.per.lbin
            names(temp_Nsamp.ages.per.lbin) <- paste(unique(gsub('[[:digit:]]+', '', names(Nsamp.ages.per.lbin))),sizes_samp$Lbin_lo, sep="")
            Nsamp.ages.per.lbin <- temp_Nsamp.ages.per.lbin
            
            new_samp <- sizes_samp$Nsamp.ages.per.lbin[match(paste(newcomp1$Gender,newcomp1$Lbin_lo, sep="."), 
                                                             paste(sizes_samp$Gender,sizes_samp$Lbin_lo, sep="."))]
            new_samp[is.na(new_samp)] <- 0
            
            newcomp1$Nsamp <- new_samp     ################################################ Change Nsamp     
            
            for(ll in 1:nrow(newcomp1)){
              N.temp <- newcomp1$Nsamp[ll]
              if(N.temp>0){
                cal.temp <- rmultinom(n=1, size=Nsamp.ages.per.lbin[ll],
                                      prob=as.numeric(newcomp1[ll,-(1:9)]))
              } else {
                cal.temp <- -1
              }
              ## Write the samples back, leaving the other columns
              newcomp1[ll,-(1:9)] <- cal.temp
            }
            ## Drop the -1 value which were temp placeholders
            newcomp1 <- newcomp1[-apply(newcomp1[,-c(1:9)],1,mean)<0,]
            
            # Put back in gender = 3 format
            if (dat_list$Ngenders==2){
              fem <- newcomp1[newcomp1$Gender==1,]
              mal <- newcomp1[newcomp1$Gender==2,]
              if (dim(fem)[1]!=0) fem$Gender = 3
              if (dim(mal)[1]!=0) mal$Gender = 3
              names(mal)[-c(1:9)] <- paste("m",names(mal)[-c(1:9)], sep=".")
              if (dim(mal)[1]!=0) mal$Nsamp <- 0
              if (dim(fem)[1]!=0) fem$Nsamp <- 0
              newcomp1 <- merge(fem, mal, all=T)
              newcomp1[is.na(newcomp1)] <- 0
              newcomp1$Nsamp <- apply(newcomp1[,-c(1:9)],1,sum)
            }
            
            #### Then pick a random sample
            
            #create a length - age expanded table and pick a random sample
            agebin_vec               <- dat_list$agebin_vector
            max_age_save <- max(agebin_vec)
            max_age <- max(agebin_vec)
            if (min(agebin_vec)==0) max_age <- max_age+1
            if (dat_list$Ngenders == 2) agebin_vec <- c(agebin_vec,max_age+agebin_vec)
            
            names(newcomp1)[-c(1:9)] <- paste("age",names(newcomp1)[-c(1:9)], sep=".")
            newcomp.long <- reshape(newcomp1,varying=list(names(newcomp1)[-c(1:9)]), 
                                    v.names="Number", idvar=names(newcomp1)[c(1:9)], 
                                    timevar="Age", times=agebin_vec, direction="long") 
            newcomp.long <- newcomp.long[newcomp.long$Number!=0,]
            max.nb <- sum(newcomp.long$Number) # total number of length samples in the haul
            
            # Expand dataframe to sample ages
            newcomp.long.expanded <- newcomp.long[rep(seq(nrow(newcomp.long)), newcomp.long$Number),]
            Nsamp.ages <- ifelse(max.nb < Nsamp[[i]][yr.ind],max.nb, Nsamp[[i]][yr.ind]) #[yr.ind]
            newcomp.long.sample_temp <- sample_n(newcomp.long.expanded, Nsamp.ages, replace=F)
            newcomp.long.sample_temp <- newcomp.long.sample_temp[order(newcomp.long.sample_temp$Lbin_lo),]
            
            if (dat_list$Ngenders == 2) {
              newcomp.long.sample_temp[newcomp.long.sample_temp$Age <= max_age_save, "Gender"] <- 1
              newcomp.long.sample_temp[newcomp.long.sample_temp$Age > max_age_save, "Gender"] <- 2
              newcomp.long.sample_temp[newcomp.long.sample_temp$Gender ==2, "Age"] <-  newcomp.long.sample_temp[newcomp.long.sample_temp$Gender ==2, "Age"] -max_age
              newcomp.long.sample_temp$ones <- 1
            } 
            
            #### Add some fake lines where ages are missing with 0 data so that it can be reshaped correctly
            
            if (dat_list$Ngenders == 1) {
              
              ages <- dat_list$agebin_vector
              missing_age <- ages[which(ages %nin% newcomp.long.sample_temp$Age)]
              newcomp.long.sample_temp$ones <- 1
              
              if (length(missing_age) >=1) {
                jj=1
                while (jj <= length(missing_age)) {
                  fake_line <- newcomp.long.sample_temp[1,]
                  fake_line$Age <- missing_age[jj]
                  fake_line$ones <- 0
                  newcomp.long.sample_temp <- rbind(newcomp.long.sample_temp, fake_line)
                  jj <- jj+1
                }
              }
              newcomp.long.sample <- rbind(newcomp.long.sample, newcomp.long.sample_temp)
              
              
            } else {
              
              newcomp.long.sample_ftemp <- newcomp.long.sample_temp[newcomp.long.sample_temp$Gender==1,]
              newcomp.long.sample_mtemp <- newcomp.long.sample_temp[newcomp.long.sample_temp$Gender==2,]
              
              if(dim(newcomp.long.sample_ftemp)[1]!=0){
                ages <- dat_list$agebin_vector
                missing_age <- ages[which(ages %nin% newcomp.long.sample_ftemp$Age)]
                newcomp.long.sample_ftemp$ones <- 1
                if (length(missing_age) >=1) {
                  jj=1
                  while (jj <= length(missing_age)) {
                    fake_line <- newcomp.long.sample_ftemp[1,]
                    fake_line$Age <- missing_age[jj]
                    fake_line$ones <- 0
                    newcomp.long.sample_ftemp <- rbind(newcomp.long.sample_ftemp, fake_line)
                    jj <- jj+1
                  }
                }
                
              }
              
              if(dim(newcomp.long.sample_mtemp)[1]!=0){
                
                missing_age <- ages[which(ages %nin% newcomp.long.sample_mtemp$Age)]
                newcomp.long.sample_mtemp$ones <- 1
                if (length(missing_age) >=1) {
                  jj=1
                  while (jj <= length(missing_age)) {
                    fake_line <- newcomp.long.sample_mtemp[1,]
                    fake_line$Age <- missing_age[jj]
                    fake_line$ones <- 0
                    newcomp.long.sample_mtemp <- rbind(newcomp.long.sample_mtemp, fake_line)
                    jj <- jj+1
                  }
                }
              }
              
              newcomp.long.sample_temp <- rbind(newcomp.long.sample_ftemp, newcomp.long.sample_mtemp)
              newcomp.long.sample <- rbind(newcomp.long.sample, newcomp.long.sample_temp)
            }
          }
          
          #summarise all hauls for that year
          temp <- aggregate(ones ~ Yr+ Seas+ FltSvy+ Gender+ Part+ Ageerr+ Lbin_lo+ Lbin_hi+ Age, newcomp.long.sample, sum)
          # turn into wide format and format with Nsamp
          temp$Nsamp <-rownames(temp)
          temp2 <- reshape(temp, v.names="ones", timevar="Age", idvar="Nsamp", direction="wide")
          temp2[is.na(temp2)] <- 0
          temp2$Nsamp <- 0
          newcomp1 <- ddply(temp2, c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi"), function(x) colSums(x[-c(1:8)]))
          
          if (dat_list$Ngenders==2) {
            newcomp1f <- newcomp1[newcomp1$Gender==1,]
            newcomp1m <- newcomp1[newcomp1$Gender==2,]
            newcomp1f$NsampF <- newcomp1f$Nsamp
            newcomp1m$NsampM <- newcomp1m$Nsamp
            newcomp1f$Nsamp <- 0
            newcomp1m$Nsamp <- 0
            if(dim(newcomp1f)[1]!=0) newcomp1f$Gender = 3
            if(dim(newcomp1m)[1]!=0) newcomp1m$Gender = 3
            names(newcomp1f)[-c(1:9)] <- paste("f", names(newcomp1f)[-c(1:9)], sep=".")
            names(newcomp1m)[-c(1:9)] <- paste("m", names(newcomp1m)[-c(1:9)], sep=".")
            newcomp1 <- merge(newcomp1f, newcomp1m, all=T)
            newcomp1[is.na(newcomp1)] <- 0
            newcomp1$Nsamp <- newcomp1$f.NsampF + newcomp1$m.NsampM
            newcomp1$f.NsampF <- newcomp1$m.NsampM <- NULL
          }
          
          newcomp1$Nsamp <- apply(newcomp1[,-c(1:9)],1,sum)
          newcomp.list[[k]] <- newcomp1
          
        } # END OF SRS LOOP
        
        
        #################################################################
        ###  SELECT AND AGE FISH WHEN SCHOOLING AND LENGTH-STRATIFIED ###
        #################################################################
        
        if (!is.null(method) && method[[i]]=="length-strat"){ 
          #   if (!is.null(dim(prob.len))){ 
          if(is.null(dim(prob.len)))
            stop("The length data have to be sampled using the clustering option for length stratified method to run")
          newcomp.long.sample <- NULL
          newcomp.long <- NULL
          
          #### First age all the fish that was lengthed across every haul
          
          # kk is the haul index
          # SO the outcome of this loop will be the age of every fish that has been lengthed for yr yr and fleet fl
          
          for (kk in 1:nrow(prob.len)) {
            newcomp1 <- newcomp
            # take the full number of ages - one age per length
            
            # if sex gender = 3, need to separate fem from mal
            if (newcomp1$Gender[1]==3)
            {
              fem <- newcomp1[,1:c(9+c(ncol(newcomp1)-9)/2)]
              mal <- newcomp1[,c(1:9,c(ncol(fem)+1):ncol(newcomp1))]
              fem$Gender <- 1
              mal$Gender <- 2
              names(mal) <- names(fem)
              newcomp1 <- rbind(fem,mal)
            } 
            
            Nsamp.ages.per.lbin <- unlist(prob.len[kk,])
            sizes <- do.call(rbind, strsplit(names(prob.len[kk,]),"[.]"))
            sizes <- as.numeric(sizes[,ncol(sizes)])
            gen <- as.numeric(as.factor(do.call(rbind, strsplit(names(prob.len[kk,]),"[.]"))[,1]))
            if (length(unique(gen))==1) gen[seq_along(gen)] <- 0
            sizes_samp <- cbind.data.frame(Lbin_lo=sizes, Nsamp.ages.per.lbin, Gender= gen)
            
            # If length bins do not match bins in the conditonal age at length
            # need to change that here
            # Caution: I don't know if it is allowed in SS anyway that the 
            # conditional age at length is not the same size bins as the length comp...
            # either way that will not affect the simulations but maybe step that should not be needed...
            # ADDED IN AT THE LAST MINUTE SO IF IT FAILS IN MAIN SIMULATIONS THAT MIGHT BE THE PROBLEM
            
            gender_save <- sizes_samp$Gender[1]
            temp_vec <- sort(unique(newcomp1$Lbin_lo))
            sizes_samp$Lbin_lo   <- cut(sizes_samp$Lbin_lo, c(0,temp_vec, c(max(temp_vec)+1)), include.lowest = F, right=F)
            levels(sizes_samp$Lbin_lo) <- c(temp_vec[1],temp_vec)
            sizes_samp$Lbin_lo       <- as.numeric(as.character(sizes_samp$Lbin_lo))
            sizes_samp               <- aggregate(Nsamp.ages.per.lbin ~ Lbin_lo, sizes_samp, sum)
            sizes_samp$Gender        <- gender_save
            temp_Nsamp.ages.per.lbin <- sizes_samp$Nsamp.ages.per.lbin
            names(temp_Nsamp.ages.per.lbin) <- paste(unique(gsub('[[:digit:]]+', '', names(Nsamp.ages.per.lbin))),sizes_samp$Lbin_lo, sep="")
            Nsamp.ages.per.lbin <- temp_Nsamp.ages.per.lbin
            
            
            new_samp <- sizes_samp$Nsamp.ages.per.lbin[match(paste(newcomp1$Gender,newcomp1$Lbin_lo, sep="."), 
                                                             paste(sizes_samp$Gender,sizes_samp$Lbin_lo, sep="."))]
            new_samp[is.na(new_samp)] <- 0
            
            newcomp1$Nsamp <- new_samp    ################################################ Change Nsamp     
            
            # ll is the length class index 
            
            for(ll in 1:nrow(newcomp1)){
              N.temp <- newcomp1$Nsamp[ll]
              if(N.temp>0){
                cal.temp <- rmultinom(n=1, size=Nsamp.ages.per.lbin[ll],
                                      prob=as.numeric(newcomp1[ll,-(1:9)]))
              } else {
                cal.temp <- -1
              }
              ## Write the samples back, leaving the other columns
              newcomp1[ll,-(1:9)] <- cal.temp
            }
            ## Drop the -1 value which were temp placeholders
            newcomp1 <- newcomp1[-apply(newcomp1[,-c(1:9)],1,mean)<0,]
            
            ### Same as SRS down to here - now need to rbind all tows and pick the Nsamp randomly from all tows
            names(newcomp1)[-c(1:9)] <- paste("age",names(newcomp1)[-c(1:9)], sep=".")
            
            agebin_vector <- c(dat_list$agebin_vector)
            
            # Put back in gender = 3 format
            if (dat_list$Ngenders==2){
              fem <- newcomp1[newcomp1$Gender==1,]
              mal <- newcomp1[newcomp1$Gender==2,]
              if (dim(fem)[1]!=0) fem$Gender = 3
              if (dim(mal)[1]!=0) mal$Gender = 3
              names(mal)[-c(1:9)] <- paste("m",names(mal)[-c(1:9)], sep=".")
              if (dim(mal)[1]!=0) mal$Nsamp <- 0
              if (dim(fem)[1]!=0) fem$Nsamp <- 0
              newcomp1 <- merge(fem, mal, all=T)
              newcomp1[is.na(newcomp1)] <- 0
              newcomp1$Nsamp <- apply(newcomp1[,-c(1:9)],1,sum)
            }
            
            agebin_vec               <- dat_list$agebin_vector
            max_age_save <- max(agebin_vec)
            max_age <- max(agebin_vec)
            if (min(agebin_vec)==0) max_age <- max_age+1
            if (dat_list$Ngenders == 2) agebin_vec <- c(agebin_vec,max_age+agebin_vec)
            
            sub <- reshape(newcomp1,varying=list(names(newcomp1)[-c(1:9)]), 
                           v.names="Number", idvar=names(newcomp1)[c(1:9)], 
                           timevar="Age", times=agebin_vec, direction="long") 
            sub <- sub[sub$Number!=0,]
            newcomp.long <- rbind(newcomp.long,sub)
          }
          
          #### Then pick a length-strtified sample (regardless of which haul they came from)
          
          # Separate data per length class and apply the cumsum sampling method in each length class
          for (ll in unique(newcomp.long$Lbin_lo)) {
            sub1 <- newcomp.long[newcomp.long$Lbin_lo==ll,]
            max.nb <- sum(sub1$Number) # total number of samples in that length
            
            if (dat_list$Ngender==2){
              max.nb.fem <- sum(sub1$Number[sub1$Age<=max_age_save])
              max.nb.mal<- sum(sub1$Number[sub1$Age>max_age_save])
            }
            
            # Expand dataframe to sample ages
            sub1.expanded <- sub1[rep(seq(nrow(sub1)), sub1$Number),]
            
            if (dat_list$Ngender==1){
              Nsamp.ages <- ifelse(max.nb < Nsamp[[i]][yr.ind],max.nb, Nsamp[[i]][yr.ind]) #[yr.ind]
              newcomp.long.sample_temp <- sample_n(sub1.expanded, Nsamp.ages, replace=F)
            }
            
            if (dat_list$Ngender==2){
              Nsamp.ages.fem <- ifelse(max.nb.fem < Nsamp[[i]][yr.ind],max.nb.fem, Nsamp[[i]][yr.ind]) #[yr.ind]
              Nsamp.ages.mal<- ifelse(max.nb.mal < Nsamp[[i]][yr.ind],max.nb.mal, Nsamp[[i]][yr.ind]) #[yr.ind]
              newcomp.long.sample_temp.f <- sample_n(sub1.expanded[sub1.expanded$Age<=max_age_save,], Nsamp.ages.fem, replace=F)
              newcomp.long.sample_temp.m <- sample_n(sub1.expanded[sub1.expanded$Age>max_age_save,], Nsamp.ages.mal, replace=F)
              newcomp.long.sample_temp  <- rbind(newcomp.long.sample_temp.f,newcomp.long.sample_temp.m)
            }
            
            newcomp.long.sample_temp <- newcomp.long.sample_temp[order(newcomp.long.sample_temp$Lbin_lo),]
            
            if (dat_list$Ngenders == 2) {
              newcomp.long.sample_temp[newcomp.long.sample_temp$Age <= max_age_save, "Gender"] <- 1
              newcomp.long.sample_temp[newcomp.long.sample_temp$Age > max_age_save, "Gender"] <- 2
              newcomp.long.sample_temp[newcomp.long.sample_temp$Gender ==2, "Age"] <-  newcomp.long.sample_temp[newcomp.long.sample_temp$Gender ==2, "Age"] -max_age
              newcomp.long.sample_temp$ones <- 1
            } 
            
            if (dat_list$Ngenders == 1) {
              
              ages <- dat_list$agebin_vector
              missing_age <- which(ages %nin% newcomp.long.sample_temp$Age)
              newcomp.long.sample_temp$ones <- 1
              if (length(missing_age) >=1) {
                jj=1
                while (jj <= length(missing_age)) {
                  fake_line <- newcomp.long.sample_temp[1,]
                  fake_line$Age <- missing_age[jj]
                  fake_line$ones <- 0
                  newcomp.long.sample_temp <- rbind(newcomp.long.sample_temp, fake_line)
                  jj <- jj+1
                }
              } 
              newcomp.long.sample <- rbind(newcomp.long.sample, newcomp.long.sample_temp)
            } else {
              
              newcomp.long.sample_ftemp <- newcomp.long.sample_temp[newcomp.long.sample_temp$Gender==1,]
              newcomp.long.sample_mtemp <- newcomp.long.sample_temp[newcomp.long.sample_temp$Gender==2,]
              
              if(dim(newcomp.long.sample_ftemp)[1]!=0){
                ages <- dat_list$agebin_vector
                missing_age <- ages[which(ages %nin% newcomp.long.sample_ftemp$Age)]
                newcomp.long.sample_ftemp$ones <- 1
                if (length(missing_age) >=1) {
                  jj=1
                  while (jj <= length(missing_age)) {
                    fake_line <- newcomp.long.sample_ftemp[1,]
                    fake_line$Age <- missing_age[jj]
                    fake_line$ones <- 0
                    newcomp.long.sample_ftemp <- rbind(newcomp.long.sample_ftemp, fake_line)
                    jj <- jj+1
                  }
                }
                
              }
              
              if(dim(newcomp.long.sample_mtemp)[1]!=0){
                
                missing_age <- ages[which(ages %nin% newcomp.long.sample_mtemp$Age)]
                newcomp.long.sample_mtemp$ones <- 1
                if (length(missing_age) >=1) {
                  jj=1
                  while (jj <= length(missing_age)) {
                    fake_line <- newcomp.long.sample_mtemp[1,]
                    fake_line$Age <- missing_age[jj]
                    fake_line$ones <- 0
                    newcomp.long.sample_mtemp <- rbind(newcomp.long.sample_mtemp, fake_line)
                    jj <- jj+1
                  }
                }
              }
              
              newcomp.long.sample_temp <- rbind(newcomp.long.sample_ftemp, newcomp.long.sample_mtemp)
              newcomp.long.sample <- rbind(newcomp.long.sample, newcomp.long.sample_temp)
            }
            
            #summarise all hauls for that year
            temp <- aggregate(ones ~ Yr+ Seas+ FltSvy+ Gender+ Part+ Ageerr+ Lbin_lo+ Lbin_hi+ Age, newcomp.long.sample, sum)
            # turn into wide format and format with Nsamp
            temp$Nsamp <-rownames(temp)
            temp2 <- reshape(temp, v.names="ones", timevar="Age", idvar="Nsamp", direction="wide")
            temp2[is.na(temp2)] <- 0
            temp2$Nsamp <- apply(temp2[,-c(1:9)],1,sum)
            # newcomp  <- ddply(temp2, c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi"), function(x) colSums(x[-c(1:8)]))
            newcomp1 <- ddply(temp2, c("Yr","Seas","FltSvy","Gender","Part","Ageerr","Lbin_lo","Lbin_hi"), function(x) colSums(x[-c(1:8)]))
            
            if (dat_list$Ngenders==2) {
              newcomp1f <- newcomp1[newcomp1$Gender==1,]
              newcomp1m <- newcomp1[newcomp1$Gender==2,]
              newcomp1f$NsampF <- newcomp1f$Nsamp
              newcomp1m$NsampM <- newcomp1m$Nsamp
              if(dim(newcomp1f)[1]!=0) {newcomp1f$Gender = 3; newcomp1f$Nsamp <- 0}
              if(dim(newcomp1m)[1]!=0) {newcomp1m$Gender = 3;   newcomp1m$Nsamp <- 0}
              names(newcomp1f)[-c(1:9)] <- paste("f", names(newcomp1f)[-c(1:9)], sep=".")
              names(newcomp1m)[-c(1:9)] <- paste("m", names(newcomp1m)[-c(1:9)], sep=".")
              newcomp1 <- merge(newcomp1f, newcomp1m, all=T)
              newcomp1[is.na(newcomp1)] <- 0
              newcomp1$Nsamp <- newcomp1$f.NsampF + newcomp1$m.NsampM
              newcomp1$f.NsampF <- newcomp1$m.NsampM <- NULL
            }
            
            newcomp1$Nsamp <- apply(newcomp1[,-c(1:9)],1,sum)
            
            newcomp.list[[k]] <- newcomp1
            
          } # END OF LOOP PER LENGTH BIN
          
          
        } # END OF LENGTH STRAT LOOP
        
        
        k <- k+1     
        
      } # END OF SRS and LENGTH STRAT LOOP
      
    } # END OF YEAR LOOP
    
  } # END OF FLEET LOOP
  
  
  ## End of loops doing the sampling.
  
  # Choose if less than 100% must be read
  
  if (any(percent_read != 100)) {
    # first randomly select how many per length class will be read then subsample ages?
    for (idx_yr in 1:length(newcomp.list)){
      sub <- newcomp.list[[idx_yr]]
      flt_idx <- sub$FltSvy[1]
      if(which(fleets==flt_idx) != 0) {
        if (percent_read[[which(fleets==flt_idx)]] != 100) {
          tot_samp <- sum(sub$Nsamp)     
          sub$Nsamp <- rmultinom(1, tot_samp*percent_read[[which(fleets==flt_idx)]]/100, sub$Nsamp)
          for (idx_ll in 1:nrow(sub)){
            sub[idx_ll,10:ncol(sub)] <- rmultinom(1, sub[idx_ll,"Nsamp"], sub[idx_ll,10:ncol(sub)])
          }
        }
      }
      sub <- sub[sub$Nsamp!=0,]
      newcomp.list[[idx_yr]] <- sub
    }
  }
  
  if (!is.null(max_ss)) {
    # first randomly select how many per length class will be read then subsample ages?
    for (idx_yr in 1:length(newcomp.list)){
      sub <- newcomp.list[[idx_yr]]
      flt_idx <- sub$FltSvy[1]
      if(which(fleets==flt_idx) != 0) {
        tot_samp <- sum(sub$Nsamp)     
        if (max_ss[[which(fleets==flt_idx)]] < tot_samp) {
          sub$Nsamp <- rmultinom(1, max_ss[[which(fleets==flt_idx)]] , sub$Nsamp)
          for (idx_ll in 1:nrow(sub)){
            sub[idx_ll,10:ncol(sub)] <- rmultinom(1, sub[idx_ll,"Nsamp"], sub[idx_ll,10:ncol(sub)])
          }
        }
      }
        sub <- sub[sub$Nsamp!=0,]
        newcomp.list[[idx_yr]] <- sub
    }
  }
  
  
  ## Combine back together into final data frame with the different data types
  newcomp.final <- do.call(rbind, newcomp.list)
  
  ####### CHANGE ESS
  
  if(is.null(ESS)) {
    #     ESS <- newcomp$Nsamp
    useESS <- FALSE
  } else {
    if (is.null(change_ess_now)) {
      useESS <- TRUE
    } else if (!change_ess_now) {
      useESS <- FALSE
    }
  }
  
  
  if(useESS) {
    for (idx_flt in 1:length(fleets)){
      if (ESS[[idx_flt]] != "hauls") {
        newcomp.final$Nsamp <- ESS[[idx_flt]][which(years[[idx_flt]]==yr)]
      } else {
        #  newcomp1$Nsamp <- round(newcomp1$Nsamp/sum(newcomp1$Nsamp)*Nhauls[[i]],1)
        sub <- newcomp.final[newcomp.final$FltSvy == fleets[idx_flt],]
        for (idx_yrs in unique(sub$Yr)){
          sub1 <- sub[sub$Yr == idx_yrs,]
          if (length(sub1$Nsamp[sub1$Lbin_lo!=-1]) >0 ){
            newcomp.final$Nsamp[newcomp.final$FltSvy == fleets[idx_flt] & newcomp.final$Yr == idx_yrs & newcomp.final$Lbin_lo!=-1] <- 
              round(sub1$Nsamp/sum(sub1$Nsamp)*Nhauls[[idx_flt]],1)
          } 
          if (length(sub1$Nsamp[sub1$Lbin_lo==-1]) >0 ){
            newcomp.final$Nsamp[newcomp.final$FltSvy == fleets[idx_flt] & newcomp.final$Yr == idx_yrs & newcomp.final$Lbin_l==-1] <- 
              Nhauls[[idx_flt]]
          }
        }
      }
    }
  }
  
  # Combine length
  # newcomp.len.final <- do.call(rbind, newcomp.len.list)
  # newfile$lencomp <- newcomp.len.final
  
  ## Cases for which data types are available. Need to be very careful
  ## here, as need to keep what's there.
  ### TODO: check this logic and simplify it. Probably don't need to check
  ### for agecomp.cal existing
  
  ## If wtatage is used the number of ages in the population matters, as opposed to simply in the samples 
  ## There will be a problem if age is truncated - plusgroup options below solves this problem
  
  
  if(NROW(agecomp.age)>0){
    
    names(newcomp.final) <- names(agecomp.age)
    
    # replace agecomp.age values of Nsamp and samples
    agecomp.age_temp <- NULL
    # Multiply age_comp by length_comp # ignore missing values for now...? ###########################################
    for (ifl in unique(newcomp.final$FltSvy)){
      sub <- newcomp.final[newcomp.final$FltSvy==ifl,] 
      sub.len <- lencomp[lencomp$FltSvy==ifl,] 
      for (iyr in unique(sub$Yr)){
        sub1               <- sub[sub$Yr==iyr,]
        sub1$Nsamp         <- sum(sub1$Nsamp)
        sub1.len           <- sub.len[sub.len$Yr==iyr,c(7:ncol(sub.len))]
        if (sub1$Gender[1]!=3){
          names(sub1.len)    <-  gsub("[^[:digit:]]", "", names(sub1.len)) #sapply(names(sub1.len), function(x) strsplit(x, "f")[[1]][2])#
          sub1$len           <- as.vector(sub1.len[names(sub1.len) %in% sub1$Lbin_lo][1,], mode='numeric')
          sub1$len           <- sub1$len/sum(sub1$len)
          sub1[,c(10:c(ncol(sub1)-1))] <- sub1[,c(10:c(ncol(sub1)-1))]/ apply(sub1[,c(10:c(ncol(sub1)-1))],1,sum)
          sub1[,c(10:c(ncol(sub1)-1))] <- sub1[,c(10:c(ncol(sub1)-1))]*sub1$len 
          sub1  <- na.omit(sub1)           #[is.na(sub1[,c(10:c(ncol(sub1)-1))])] <- 0
          sub2 <- apply(sub1[,c(10:c(ncol(sub1)-1))],2,sum)
          sub3 <- cbind.data.frame(sub1[1,c(1:9)], matrix(sub2, nrow=1, ncol=length(sub2)))
        } else {
          names(sub1.len)    <- c(na.omit(sapply(names(sub1.len), function(x) strsplit(x, "f")[[1]][2])), na.omit(sapply(names(sub1.len), function(x) strsplit(x, "m")[[1]][2])))#
          sub1.len_f <- sub1.len[1:c(ncol(sub1.len)/2)]
          sub1.len_m <- sub1.len[c(c(ncol(sub1.len)/2)+1):ncol(sub1.len)]         
          sub1$len_f           <- as.vector(sub1.len_f[names(sub1.len_f) %in% sub1$Lbin_lo][1,], mode='numeric')
          sub1$len_m           <- as.vector(sub1.len_m[names(sub1.len_m) %in% sub1$Lbin_lo][1,], mode='numeric')
          sub1$len_f           <- sub1$len_f/sum(sub1$len_f)
          sub1$len_m           <- sub1$len_m/sum(sub1$len_m)
          sub1[,c(10:c(10+c(ncol(sub1)-11)/2-1))] <- sub1[,c(10:c(10+c(ncol(sub1)-12)/2))] / apply(sub1[,c(10:c(10+c(ncol(sub1)-12)/2))] ,1,sum)
          sub1[,c(10:c(10+c(ncol(sub1)-11)/2-1))]  <- sub1[,c(10:c(10+c(ncol(sub1)-12)/2))] * sub1$len_f
          sub1[is.na(sub1)] <- 0
          sub2_f <- apply(sub1[,c(10:c(10+c(ncol(sub1)-12)/2))] ,2,sum)
          max_col <- c(ncol(sub1)-2)
          sub1[,c(10+c(ncol(sub1)-11)/2):max_col] <-  sub1[,c(10+c(ncol(sub1)-11)/2):max_col]  / apply( sub1[,c(10+c(ncol(sub1)-11)/2):max_col] ,1,sum)
          sub1[,c(10+c(ncol(sub1)-11)/2):max_col] <-  sub1[,c(10+c(ncol(sub1)-11)/2):max_col] *sub1$len_f
          sub1[is.na(sub1)] <- 0
          sub2_m <- apply( sub1[,c(10+c(ncol(sub1)-11)/2):max_col] ,2,sum)
          sub3 <- cbind.data.frame(sub1[1,c(1:9)], matrix(c(sub2_f,sub2_m), nrow=1, ncol=length(c(sub2_f,sub2_m))))
        }
        sub3$Lbin_hi <- -1
        sub3$Lbin_lo <- -1
        agecomp.age_temp <-  rbind(agecomp.age_temp, sub3)
      }
    }
    # Then add up for marginal comp AND turn FltSvy to -1 so it is not included in the loglikelihood ################################################################# CHOOSE TO FIT ON MARGINAL COMP IN WHICH CASE ADD A .5 VARIANCE ADJ IN .ctl
    agecomp.age_temp$FltSvy <- -abs(agecomp.age_temp$FltSvy)
    agecomp.age             <- agecomp.age_temp
    
    ####################################################
    ###  CHOOSE TO FIT ON MARGINAL AGE COMPOSITION  ###
    ####################################################
    
    if (!is.null(fit.on.agecomp)){
      agecomp.age$FltSvy <- abs(agecomp.age$FltSvy)
      newcomp.final$FltSvy <- -abs(newcomp.final$FltSvy)
      
      if (is.null(ctl)) {
        warning("Fitting on marignal age composition should be combined with dividing the weight on age and length by 2. 
                This requires providing a path for the control file to be changed. Ignore this if wtatage was called :)") 
      } else {
        
        # read in ctl file and change lambda to divide by 2... for now changing the varadjust, need to check how to change lambda instead..??? 
        new_table <- as.data.frame(matrix(0, ncol=length(dat_list$fleetnames), nrow=6))
        names(new_table) <- dat_list$fleetnames
        new_table[5,] <- 0.5
        new_table[4,] <- 0.5
        new_table[6,] <- 1
        
        SS_varadjust2(dir = NULL, ctlfile = ctl,
                      newctlfile = ctl, keyword = "Variance_adjustments",
                      newtable = new_table, newrow = NULL, rownumber = NULL, maxcols = 1000,
                      overwrite = T, verbose = TRUE)
      }
    }
    
    names(agecomp.age) <- names(newcomp.final)
    newcomp.final <- rbind(agecomp.age, newcomp.final)
    
    newfile$agecomp <- newcomp.final
    newfile$N_agecomp <- NROW(newfile$agecomp)
    
    
    ###################################################################
    ###  AGE PLUS GROUP CUMULATOR - OPTION ADDED MOSTLY FOR WTATAGE ###
    ###################################################################
    
    # Only if the OM is set up with maximum age in the sample = maximum age in the population will wtatage sampling be possible.
    # Then it is still possible to have an age-plus group in the age comp.
    
    save_for_wtatage <- list(newfile$agecomp, newfile$agebin_vector) 
    
    if (!is.null(ageplus)){      
      
      if (ageplus != newfile$N_agebins) {
        
        save_for_wtatage  <- list(newfile$agecomp, newfile$agebin_vector) 
        
        # then cumulate in the ageplus group
        newfile$N_agebins     <- ageplus
        newfile$agebin_vector <- seq(min(newfile$agebin_vector), ageplus, 1)
        
        if (newfile$agecomp$Gender[1]==3){
          
          agecomp_temp      <- save_for_wtatage[[1]][,c(10:dim(save_for_wtatage[[1]])[2])]
          agecomp_temp_fem  <- agecomp_temp[,1:c(ncol(agecomp_temp)/2)]
          agecomp_temp_mal  <- agecomp_temp[,c(c(ncol(agecomp_temp)/2+1):dim(agecomp_temp)[2])]
          plusgroup_idx     <- which(newfile$agebin_vector==ageplus)
          plusses_fem       <- agecomp_temp_fem[,c(plusgroup_idx:ncol(agecomp_temp_fem))]
          plusses_mal       <- agecomp_temp_mal[,c(plusgroup_idx:ncol(agecomp_temp_mal))]
          plusgroup_fem     <- apply(plusses_fem, 1, sum)
          plusgroup_mal     <- apply(plusses_mal, 1, sum)
          idx_temp          <- ncol(newfile$agecomp[,c(1:c(9+plusgroup_idx-1))]) + ncol(agecomp_temp_fem)
          newfile$agecomp   <- cbind(newfile$agecomp[,c(1:c(9+plusgroup_idx-1))],plusgroup_fem,newfile$agecomp[,c(idx_temp:c(idx_temp+plusgroup_idx-2))],plusgroup_mal)
          names(newfile$agecomp) <- c(names(newfile$agecomp)[1:9], paste("a",newfile$agebin_vector, sep=""))
          
        } else {
          plusgroup_idx  <- 9 + which(newfile$agebin_vector==ageplus)
          plusses        <- newfile$agecomp[,c(plusgroup_idx:ncol(newfile$agecomp))]
          plusgroup      <- apply(plusses, 1, sum)
          newfile$agecomp <- cbind(newfile$agecomp[,c(1:c(plusgroup_idx-1))],plusgroup)
          names(newfile$agecomp) <- c(names(newfile$agecomp)[1:9], paste("a",newfile$agebin_vector, sep=""))
        }
      }  
    } else { save_for_wtatage <- NULL }
  } 
  
  if (!is.null(fit.on.agecomp)) {
    newfile$agecomp <-  newfile$agecomp[newfile$agecomp$Lbin_lo == -1,] 
    newfile$N_agecomp <- NROW(newfile$agecomp)
  }
  
  ## Write the modified file
  if(write_file)
    r4ss::SS_writedat(datlist = newfile, outfile = outfile,
                      overwrite = TRUE, verbose=FALSE)
  if (!is.null(save_for_wtatage)) return(invisible(list(newfile,save_for_wtatage)))
  if (is.null(save_for_wtatage)) return(invisible(newfile))
}



