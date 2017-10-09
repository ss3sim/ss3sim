#' [BETA VERSION] Sample mean length (size-)-at-age data and write to file for use by the EM.
#'
#' @details **This function is in beta and untested. Use with caution.**
#' Take a \code{data.SS_new} file, read in by \pkg{r4ss} function
#'   \code{\link[r4ss]{SS_readdat}} containing observed values, and
#'   sample from the observed ages to get realistic proportions for the number
#'   of fish in each age bin, then use the mean size-at-age and CV for growth to
#'   generate random samples of size, which are then averaged to get mean
#'   length-at-age values. These values are then written to file for the
#'   EM.
#' @author Cole Monnahan, Kelli Johnson
#'
#' @template lcomp-agecomp-index
#' @template Nsamp
#' @template dat_list
#' @param ctl_file_in A path to the control file, outputed from an OM, containing
#'   the OM parameters for growth. These values are used to determine the
#'   uncertainty about size for fish sampled in each age bin.
#' @param mean_outfile A path to write length and age data for external
#' estimation of parametric growth. If \code{NULL} no file will be written.
#' This file is used by \code{change_e} to externally estimate growth
#' parameters. Filename must contain "vbgf" to be used by \code{change_e}.
#' Also, if "remove" is included in the filename, the mean length at age data
#' will be removed from the \code{.dat} file and not be available to the EM.
#' @param ageplus XXXXXXXXXXXXXXXXXXXXXXXXX
#' @param pop_agecomp XXXXXXXXXXXXXXXXXXXXXXXXX
#' @param verbose Logical value whether or not diagnostic information from
#'   \pkg{r4ss} functions should be printed to the screen. Default is FALSE.
#' @template sampling-return
#' @template casefile-footnote
#' @family sampling functions
#' @importFrom r4ss SS_writedat SS_parlines
#' @export
#'

sample_mlacomp <- function(dat_list, outfile, ctl_file_in, fleets = 1, Nsamp,
                           years, write_file=TRUE, mean_outfile = NULL, ageplus = NULL, pop_agecomp= NULL, # Added age plus group for use with wtatage
                           verbose = TRUE){

  ## If fleets==NULL, quit here and delete the data so the EM doesn't use it.
  if (is.null(fleets)) {
    dat_list$MeanSize_at_Age_obs <- data.frame("#")
    dat_list$N_MeanSize_at_Age_obs <- 0
    if (write_file)
      SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE,
                  verbose = verbose)
    return(invisible(dat_list))
  }

  check_data(dat_list)
  # Users can specify either Lbin_lo or Lbin_hi < 1 for agecomp data
  agecomp <- dat_list$agecomp
  if (!is.null(pop_agecomp)) agecomp <- pop_agecomp[[1]]
  
  agecomp <- agecomp[agecomp$Lbin_lo == -1 | agecomp$Lbin_hi == -1, ]
  agecomp$Yr <- abs(agecomp$Yr)
  agecomp$FltSvy <- abs(agecomp$FltSvy)
  
  if (NROW(agecomp) == 0) {
    stop(paste0("No age data exist in the dat_list."))
  }
  mwacomp <- dat_list$MeanSize_at_Age_obs[dat_list$MeanSize_at_Age_obs$AgeErr < 0, ]
  mlacomp <- dat_list$MeanSize_at_Age_obs[dat_list$MeanSize_at_Age_obs$AgeErr > 0, ]
  agebin_vector <- dat_list$agebin_vector
  if (!is.null(pop_agecomp)) agebin_vector <- pop_agecomp[[2]]
  
  ## Read in the control file
  ctl <- SS_parlines(ctl_file_in)
    CV.growth <- ctl[ctl$Label == "CV_young_Fem_GP_1", "INIT"]
    CV.growth.old <- ctl[ctl$Label == "CV_old_Fem_GP_1", "INIT"]
    if (CV.growth != CV.growth.old) {
      warning(paste0("sample_mlacomp does not support different values for the",
                  "CV's of young and old fish. Please the check ", ctl_file_in,
                  "and make sure CV_young_Fem_GP_1 (", CV.growth, ") is",
                  " equal to CV_old_Fem_GP_1 (", CV.growth.old, "). The 2 CVs were averaged"))
      CV.growth <- c(CV.growth + CV.growth.old)/2
    #  warning(paste0("The old CV was replace by the average of the 2 cvs, i.e. ", CV.growth))
      
    }

  ## Check inputs for errors
  if (!is.null(outfile) & write_file){
    if (substr_r(outfile,4) != ".dat") {
      stop(paste0("outfile ", outfile, " needs to end in .dat"))
    }
  }

  Nfleets <- length(fleets)
  if (length(years) != Nfleets) {
    stop("years needs to be a list of same length as fleets")
  }
  if (class(years) != "list" & length(years) > 1 & Nfleets == 1) {
    stop("years needs to be a list unless it only includes one fleet and one year")
  }
  if (is.null(mlacomp)) {
    stop("mean length-at-age compositions do not exist")
  }
  # The following subset routine will only work for a single sex model
  if (any(grepl("f", names(mlacomp)))) {
    stop(paste("mlacomp data contains two sexes, which is not currently",
               "supported by ss3sim, please reconfigure your model."))
  }
  # Check for a sample size for every year
  if (length(Nsamp) != Nfleets){ ############################ ERROR IN SS3SIM
    stop(paste("Nsamp was not included for all fleets in fleet.",
               "User must include a sample size for each fleet that can be",
               "repeated for each year for that fleet or specified as a list of",
               "vectors, with the same dimensions as years."))
  }
  if (any(sapply(Nsamp, length) == 1)) {
    if (!is.list(Nsamp)) Nsamp <- as.list(Nsamp)
    repNsamp <- which(sapply(Nsamp, length) == 1)
    for (i in repNsamp){
      Nsamp[[i]] <- rep_len(Nsamp[[i]], length(years[[i]]))
    }
  }
  if (any(sapply(Nsamp, length) != sapply(years, length))) {
    stop(paste("Number of samples were not specified for every year.\n",
               "Length of years and Nsamp did not match for fleet(s)",
               fleets[which(sapply(Nsamp, length) != sapply(years, length))]))
  }
  
  # Make sure the years have got agecomp data
  years_age <-unique(cbind.data.frame(Yr=abs(dat_list$agecomp$Yr),FltSvy=abs(dat_list$agecomp$FltSvy)))
  for (idx_flt in 1:length(unique(years_age$FltSvy))){
    years[[idx_flt]] <- years[[idx_flt]][years[[idx_flt]] %in% years_age[years_age$FltSvy== fleets[idx_flt],"Yr"]]
  }
  
  ## End input checks

  mlacomp.new.list <- list() # temp storage for the new rows
  save_for_wtatage.list <- list() # temp storage for the new rows
  forexport <- list()
  k <- 1                 # each k is a new row of data, to be rbind'ed later
  # Loop through mla data for this fleet, given the years specified
  for (fl in 1:length(fleets)) {
    fl.temp <- fleets[fl]
    mlacomp.fl <- mlacomp[mlacomp$FltSvy == fleets[fl] &
                          mlacomp$Yr %in% years[[fl]], ]
    if (length(years[[fl]]) != length(unique(mlacomp.fl$Yr))) {
      stop(paste("A year specified in years for fleet", fl.temp, "was not",
                 "found in the input dat_list for fleet", fl.temp))
    }
    for (j in 1:NROW(mlacomp.fl)) {
      yr.temp <- mlacomp.fl$Yr[j]
      # Loop through mla data for this fleet / year combo
      mlacomp.new <- mlacomp.fl[j, ]
      if (NROW(mlacomp.new) == 0) {
        stop(paste("No mla comp data found for fleet", fl.temp,
                   "in year", yr.temp))
      }
      # Get the columns that pertain to the actual mla data and not metadata
      mla.means <- as.numeric(mlacomp.new[paste0("a", agebin_vector)])
    
      # For each age, given year and fleet, get the expected length and CV
      # around length, then sample from it using lognormal (below)
      # length for a given age is lognormal with expected value = E[mla]
      # and CV equal to CV from ctl file
      sds <- mla.means * CV.growth
      # \mu = log(\frac{M}{sqrt{1 + \frac{V}{m^2})}})
      # log(\frac{m}{(1 + \frac{v}{m^2})^(1/2)}) =
      # log(\frac{m^2}{(m^2 + v)^(1/2)})
      means.log <- log(mla.means^2 / sqrt(sds^2 + mla.means^2))
      # sigma^2 = ln(1 + var[X] / (E[X])^2)
      # symmetric distribution: Var on real scale, $\mu$ on log scale
      sds.log <- sqrt(log(1 + sds^2 / mla.means^2))
      # Get the true age distributions, probability of being a fish of age x
      agecomp.temp <- agecomp[agecomp$Yr == yr.temp &
                              agecomp$FltSvy == fl.temp, ]
      # remove the 9 columns of metadata
      age.means <- as.numeric(agecomp.temp[-(1:9)])
      age.means  <- age.means/sum(age.means)
      # Get user input sample size, theoretically this cannot be bigger than age n
      age.Nsamp <- as.numeric(Nsamp[[fl]][j])
      if (age.Nsamp > sum(agecomp$Nsamp)) {
        stop(paste("Cannot sample more fish for mean length-at-age than what",
                   "exists in the observed ages. ", agecomp$Nsamp, "were",
                   "observed but user specified a sample size of ", age.Nsamp))
      }
      # Draw samples to get # of fish in each age bin
      if (any(is.na(age.means))) {
        stop("Invalid age comp probabilities in sample_mlacomp.")
      }
      # If TRUE, assume a multinomial was used for agecomp sampling, thus
      # resample agecomp N empirically to avoid too many samples in a given bin
      if (any(age.means > 1)) {
        # Create a vector of empirical samples of ages, such that each age bin
        # is repeated equal to the number of observed fish in that bin.
        prob.age.ints <- unlist(sapply(1:length(age.means), function(x) {
          rep(x, age.means[x])
          }))
        # Resample to guarantee the sample size does not exceed the observed
        temp <- sample(x = prob.age.ints, size = age.Nsamp, replace = FALSE)
        age.samples <- sapply(1:length(age.means), function(x) sum(temp == x))
      } else {
        # in the case of overdispersed age comp data
        age.samples <- rmultinom(n = 1, size = as.integer(age.Nsamp),
                                 prob = age.means)
      }
      if (any(is.na(age.samples))) {
        stop("Invalid length at age sample size in mlacomp")
      }

      # apply sampling across columns (ages) to get sample of lengths
      lengths.list <-
        lapply(1:length(means.log), function(kk) {
          exp(rnorm(n = age.samples[kk], mean = means.log[kk],
          sd = sds.log[kk]))
          })

      # prepare mla data for export to the von B growth function
      names(lengths.list) <- as.numeric(gsub("a", "",
                                        colnames(agecomp.temp)[-(1:9)]))
      temp <- lapply(seq_along(lengths.list), function(x) {
         cbind(age = names(lengths.list)[x], length = lengths.list[[x]],
               mean = exp(means.log[x]), fleet = fl.temp, year = yr.temp)
        })
      forexport[[k]] <- do.call("rbind", temp[lapply(temp, length) > 4])
      # Take mean length of each age bin mean and place in mla comp data frame
      mlacomp.new.means <- do.call(c, lapply(lengths.list, mean))
      # Sometimes you draw 0 fish from an age class, resulting in NaN
      # For now, replace with filler values
      # TODO: Fix the placeholder values for missing age bins
       mlacomp.new.means[is.nan(mlacomp.new.means)] <- -1
      # Change the missing value for an average of the adjacent ones - -1 does not work when using it for wtatage after...
      missing <- which(mlacomp.new.means==-1)
      values <- seq_along(mlacomp.new.means)[-missing]
      for (fix_mla in missing){
        dif <- c(values - fix_mla)
        if (any(dif <0)) neg <- max(dif[dif<0])
        if (any(dif >0)) pos <- min(dif[dif>0])
        if (!any(dif <0)) neg <- pos
        if (!any(dif >0)) pos <- neg
        below <- mlacomp.new.means[values[which(dif == neg)]]
        above <- mlacomp.new.means[values[which(dif == pos)]]
        mlacomp.new.means[fix_mla] <- (below + above)/2
      }
      
      ## mla data needs the sample sizes, so concatenate those on
      mlacomp.new[-(1:7)] <- c(mlacomp.new.means, age.samples)
      
      
      save_for_wtatage <- list(mlacomp.new) ##################################################################### NEW
      
      if (!is.null(ageplus)){      
        
        if (ageplus != max(pop_agecomp[[2]])) {
          
          save_for_wtatage  <- list(mlacomp.new) #newfile$MeanSize_at_Age_obs,
            
          # then cumulate in the ageplus group
          dat_list$N_agebins     <- ageplus
          #dat_list$agebin_vector <- seq(min(dat_list$agebin_vector), ageplus, 1)
          
          if (mlacomp.new$Gender[1]==3){
            meansize_cols      <- save_for_wtatage[[1]][,c(8:c((8+dim(save_for_wtatage[[1]])[2])/2))]#save_for_wtatage[[2]]
            meansize_cols_fem  <- meansize_cols[,1:c(ncol(meansize_cols)/2)]
            meansize_cols_mal  <- meansize_cols[,c(ncol(meansize_cols)/2+1):ncol(meansize_cols)]
            other              <- save_for_wtatage[[1]][,c(c((9+dim(save_for_wtatage[[1]])[2])/2):dim(save_for_wtatage[[1]])[2])]
            other_fem          <- other[,1:c(ncol(other)/2)]
            other_mal          <- other[,c(ncol(other)/2+1):ncol(other)]
            plusgroup_idx      <- which(dat_list$agebin_vector==ageplus)
            plusses_fem        <- meansize_cols_fem[,c(plusgroup_idx:ncol(meansize_cols_fem))]
            plusses_mal        <- meansize_cols_mal[,c(plusgroup_idx:ncol(meansize_cols_mal))]
            plusgroup_fem      <-  mean(plusses_fem[plusses_fem != -1])
            plusgroup_mal      <-  mean(plusses_mal[plusses_mal != -1])#apply(plusses_mal[plusses_fem != -1], 1, mean)
            idx_temp           <- ncol(mlacomp.new[,c(1:c(7+plusgroup_idx-1))]) + ncol(meansize_cols_fem)
            mlacomp.new        <- cbind(mlacomp.new[,c(1:c(7+plusgroup_idx-1))],plusgroup_fem,mlacomp.new[,c(idx_temp:c(idx_temp+plusgroup_idx-2))],plusgroup_mal)
            other_fem          <- other_fem[,1:plusgroup_idx]
            other_mal          <- other_mal[,1:plusgroup_idx]
            mlacomp.new       <- cbind(mlacomp.new, other_fem, other_mal) 
          } else{
            meansize_cols <- save_for_wtatage[[1]][,c(8:c((8+dim(save_for_wtatage[[1]])[2])/2))]
            other <- save_for_wtatage[[1]][,c(c((9+dim(save_for_wtatage[[1]])[2])/2):dim(save_for_wtatage[[1]])[2])]
            plusgroup_idx <- which(dat_list$agebin_vector==ageplus)
            plusses <- meansize_cols[,c(plusgroup_idx:ncol(meansize_cols))]
            plusgroup <- mean(plusses[plusses != -1])
            mlacomp.new <- cbind(mlacomp.new[,c(1:c(7+plusgroup_idx-1))],plusgroup)
            names(mlacomp.new)[ncol(mlacomp.new)] <- paste("a",ageplus, sep="")
            other <- other[,1:plusgroup_idx]
            mlacomp.new <- cbind(mlacomp.new, other)
            
          } 
        }
      }
        
        mlacomp.new.list[[k]] <- mlacomp.new
        save_for_wtatage.list[[k]] <- save_for_wtatage[[1]]
        
      k <- k + 1
    }
  } # end sampling
  if (!is.null(mean_outfile)) {
    write.csv(do.call("rbind", forexport), mean_outfile, row.names = FALSE)
  }
  ## Combine new rows together into one data.frame
  mlacomp.new <- do.call(rbind, mlacomp.new.list)
  save_for_wtatage.new <- do.call(rbind, save_for_wtatage.list)
  dat_list$MeanSize_at_Age_obs <- rbind(mlacomp.new, mwacomp)
  dat_list$N_MeanSize_at_Age_obs <- NROW(mlacomp.new)
  ## Write the modified file
  if(write_file) SS_writedat(datlist = dat_list, outfile = outfile,
                             overwrite = TRUE, verbose = verbose)
  return(invisible(list(dat_list,save_for_wtatage.new)))
}
