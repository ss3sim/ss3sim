#' Take an SS data object, manipulate the values, and return a
#' modified version
#' 
#' @param Yr_start Starting year
#' @param Yr_end Ending year
#' @param Nfleet Number of fleets
#' @param Nsurveys Number of surveys
#' @param N_areas Number of areas
#' @param fleetnames Fleet names
#' @param surveytiming Sample timing 
#' @param Nareas Number of areas
#' @param Ncatch Number of catch records
#' @param Ncpue Number of CPUE records
#' @param CPUE_index CPUE index values
#' @param survey_index Survey index values
#' @param se_log_CPUE Standard error of log(CPUE)
#' @param se_log_survey Standard error of log(survey index)
#' @param N_lencomp Number of length-composition observations
#' @param lengthcompdata Length-composition data
#' @param N_agecomp Number of age-composition observations
#' @param agecompdata Age composition data

change_SS_dat <- function(SS_dat, 
  Yr_start=1913,
  Yr_end=2012,
  Nfleet=1,
  Nsurveys=1,
  N_areas=1,
  fleetnames="Fishery%Survey",
  surveytiming=c(-1, 0.5),
  Nareas=1,
  Ncatch=100,
  Ncpue=200,
  CPUE_index=rep(1,100),
  survey_index=rep(1,100),
  se_log_CPUE=rep(0.1,100),
  se_log_survey=rep(0.1,100),
  N_lencomp=1,
  lengthcompdata=1,
  N_agecomp=1, 
  agecompdata=1
  ) {

  Years <- Yr_start:Yr_end
  # change the start year = 1
  SS_dat$styr <- Yr_start
  # change the end year = 1
  SS_dat$endyr <- Yr_end
  # change the fleet = 1
  SS_dat$Nfleet <- Nfleet
  # change the survey = 1
  SS_dat$Nsurveys <- Nsurveys
  # change the area = 1
  SS_dat$N_areas <- N_areas
  # change the fleetname - only Fishery and Survey
  SS_dat$fleetnames  <- fleetnames
  # change the survey timing
  SS_dat$surveytiming  <- surveytiming
  # change area size = 1
  SS_dat$areas <- rep(Nareas, Nfleet+Nsurveys)
  # change fleet info 
  infofleet <- matrix(c(surveytiming, rep(Nareas,
        length(surveytiming))),ncol=length(surveytiming),2,byrow=TRUE)
  SS_dat$fleetinfo1 <- data.frame(infofleet, input=c("#_surveytiming","#_areas")); 
  # change N catch $ change catch
  if(SS_dat$N_catch!=Ncatch)
  {  
    SS_dat$N_catch = Ncatch
    catch <- data.frame(Fishery=rep(1:Nfleet,each=Ncatch),
      year=rep(Years, Nfleet), seas=rep(1:SS_dat$nseas,each=Ncatch))
    SS_dat$catch <- catch
  }
  # change N CPUE
  SS_dat$N_cpue = Ncpue
  # change CPUE error type
  SS_dat$CPUEinfo = data.frame(matrix(c(1:Nfleet, Nfleet+(1:Nsurveys),
        rep(0,(Nfleet+Nsurveys)*2)), ncol=3, byrow=FALSE))
  # change CPUE 
  CPUE <- data.frame(year=rep(Years,(Nfleet+Nsurveys)) ,
    seas=rep(1:SS_dat$nseas,each=Ncatch*(Nfleet+Nsurveys)),
    index=rep(1:(Nfleet+Nsurveys), each=Ncatch), obs=c(CPUE_index,
      survey_index), se_log=c(se_log_CPUE, se_log_survey)) 
  SS_dat$CPUE <- CPUE
  # Mean body weight (NOT USED in this simulation analysis)
  SS_dat$N_meanbodywt = 0
  SS_dat$DF_for_meanbodywt = 0
  # Length binning method (for the pop dyn)
  SS_dat$lbin_method = 1
  # Length binning (for the pop dyn)
  SS_dat$binwidth =1
  # Length binning for the observed length comp
  SS_dat$N_lbins  =  SS_dat$maximum_size - SS_dat$minimum_size +1 
  # Length binning for the observed length comp
  SS_dat$lbin_vector = seq(SS_dat$minimum_size, SS_dat$maximum_size, by=1)
  # N lengthcomp data
  if(length(N_lencomp)==0) SS_dat$N_lencomp = 0
  if(N_lencomp==1) SS_dat$N_lencomp = SS_dat$N_lencomp
  if(N_lencomp>1) SS_dat$N_lencomp = N_lencomp
  # lengthcomp data
  if(length(lengthcompdata)==0) SS_dat$lencomp = data.frame("#")
  if(length(lengthcompdata)==1) SS_dat$lencomp = SS_dat$lengthcompdata
  if(length(lengthcompdata)>1) SS_dat$lencomp = lengthcompdata
  # N agecomp data
  if(N_agecomp<=1) SS_dat$N_agecomp = 0
  if(N_agecomp>1) SS_dat$N_agecomp = N_agecomp
  # agecomp data
  if(length(agecompdata)==0) SS_dat$agecomp = data.frame("#")     
  if(length(agecompdata)==1) SS_dat$agecomp = SS_dat$agecompdata      
  if(length(agecompdata)>1) SS_dat$agecomp = agecompdata      
  # N ageing error 
  SS_dat$N_ageerror_definitions =0
  # agein error 
  SS_dat$ageerror = data.frame("#")
  # meansize at age obs
  SS_dat$N_MeanSize_at_Age_obs = 0
  SS_dat$MeanSize_at_Age_obs = data.frame("#")

  return(SS_dat)
}

