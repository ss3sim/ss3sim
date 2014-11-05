#' Sample mean length (size-)-at-age data and write to file for use by the EM.
#'
#' @details Take a \code{data.SS_new} file containing expected values and
#' sample from true ages to get realistic proportions for the number of
#' fish in each age bin, then use the mean size-at-age and CV for growth to
#' generate random samples of size, which are then averaged to get mean
#' weight-at-age values. These values are then written to file for the
#' EM.
#' @author Cole Monnahan
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param datfile A path to the data file, outputed from an OM, containing
#' the true age distributions (population bins). This file is read in and
#' then used to determine how many fish of each age bin are to be sampled.
#' @param ctlfile A path to the control file, outputed from an OM, containing
#' the OM parameters for growth and weight/length relationship. These
#' values are used to determine the uncertainty about weight for fish
#' sampled in each age bin.
#' @param mean_outfile A path to write length and age data for external
#' estimation of parametric growth. If NULL no file will be written.
#' This file is used by \code{change_e} to externally estimate growth
#' parameters.
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}}
#' @export
#' @importFrom r4ss SS_readdat SS_writedat SS_parlines

sample_mlacomp <- function(datfile, outfile, ctlfile, fleets = 1, Nsamp,
                           years, write_file=TRUE, mean_outfile = NULL){
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function.
    ##

    ## Read in datfile, need this for true age distributions and Nsamp
    datfile <- SS_readdat(file=datfile, verbose=FALSE)
    ## If fleets==NULL, quit here and delete the data so the EM doesn't use it.
    if(is.null(fleets)){
        datfile$MeanSize_at_Age_obs <- NULL
        datfile$N_MeanSize_at_Age_obs <- 0
        SS_writedat(datlist=datfile, outfile=outfile, overwrite=TRUE,
                          verbose=FALSE)
        return(NULL)
    }
    agecomp <- datfile$agecomp
    mlacomp <- datfile$MeanSize_at_Age_obs
    agebin_vector <- datfile$agebin_vector
    ## Read in the control file
    ctl <- SS_parlines(ctlfile)
    ## Check inputs for errors
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- length(fleets)
    if(length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    if(class(years) != "list" & length(years) > 1 & Nfleets == 1)
        stop("years needs to be a list unless it only includes one fleet and one year")
    if(is.null(mlacomp)){
            stop("mean length-at-age compositions do not exist")
        } else {
            if(nrow(mlacomp[mlacomp$AgeErr > 0, ]) < 1)
                stop("mean length-at-age compositions do not exist")
        }
    ## End input checks

    ## Resample from the length-at-age data. The general approach here is
    ## to loop through each row and sample based on the true age
    ## distribution. Note, true age distribution is known, as is but there
    ## is uncertainty in the age->length relationship. This uncertainty
    ## defines the distribution from which we sample. It is also based on
    ## the # of age samples taken, to mimic reality better.
    mlacomp.new.list <- list() # temp storage for the new rows
    forexport <- list()
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet, if fleets=NULL then skip sampling and
    ## return nothing (subtract out this type from the data file)
    for(fl in 1:length(fleets)){
        fl.temp <- fleets[fl]
        mlacomp.fl <- mlacomp[mlacomp$Fleet == fleets[fl] & mlacomp$Yr %in% years[[fl]],]
        for(j in 1:NROW(mlacomp.fl)){
            yr.temp <- mlacomp.fl$Yr[j]
            mlacomp.new <- mlacomp.fl[j,]
            mla.means <- as.numeric(mlacomp.new[paste0("a",agebin_vector)])
            ## For each age, given year and fleet, get the expected length
            ## and CV around that length, then sample from it using
            ## lognormal (below)
            CV.growth <- ctl[ctl$Label=="CV_young_Fem_GP_1", "INIT"]
            sds <- mla.means*CV.growth
            ## These are the moments on the natural scale, so
            ## convert to log scale and generate data
            means.log <- log(mla.means^2/sqrt(sds^2+mla.means^2))
            sds.log <- sqrt(log(1 + sds^2/mla.means^2))
            ## Get the true age distributions
            agecomp.temp <- agecomp[agecomp$Yr==yr.temp & agecomp$FltSvy==fl.temp &
              agecomp$Lbin_lo < 0,]
            ## Get the true age distributions
            age.means <- as.numeric(agecomp.temp[-(1:9)])
            age.Nsamp <- as.numeric(Nsamp[[fl]][j])
            ## Draw samples to get # of fish in each age bin
            age.samples <- rmultinom(n=1, size=as.integer(age.Nsamp), prob=age.means)
            ## apply sampling across the columns (ages) to get
            ## sample of lengths
            lengths.list <-
                lapply(1:length(means.log), function(kk)
                       exp(rnorm(n=age.samples[kk], mean=means.log[kk], sd=sds.log[kk])))

            names(lengths.list) <- as.numeric(gsub("a", "", colnames(agecomp.temp)[-(1:9)]))
            temp <- lapply(seq_along(lengths.list), function(x) {
               cbind(names(lengths.list)[x], lengths.list[[x]], exp(means.log[x]))
              })
            forexport[[k]] <- do.call("rbind", temp[lapply(temp, length) > 2])
            colnames(forexport[[k]]) <- c("age", "length", "mean")
            ## Take means and combine into vector to put back
            ## into the data frame.
            mlacomp.new.means <- do.call(c, lapply(lengths.list, mean))
            ## Sometimes you draw 0 fish from an age class,
            ## resulting in NaN for the mean mlacomp. For now,
            ## replace with filler values
### TODO: Fix the placeholder values for missing age bins
            mlacomp.new.means[is.nan(mlacomp.new.means)] <- -1
            ## mla data needs the sample sizes, so concatenate those on
            mlacomp.new[-(1:7)] <- c(mlacomp.new.means, age.samples)
            mlacomp.new.list[[k]] <- mlacomp.new
            k <- k+1
        }
    } # end sampling
    if(!is.null(mean_outfile)) {
        write.csv(do.call("rbind", forexport), mean_outfile)
    }
    ## Combine new rows together into one data.frame
    mlacomp.new <- do.call(rbind, mlacomp.new.list)
    datfile$MeanSize_at_Age_obs <- mlacomp.new
    datfile$N_MeanSize_at_Age_obs <- nrow(mlacomp.new)
    ## Write the modified file
    if(write_file) SS_writedat(datlist=datfile, outfile=outfile,
                                     overwrite=TRUE, verbose=TRUE)
    return(invisible(mlacomp.new))
}
