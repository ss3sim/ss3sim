#' Sample conditional age-at-length (CAL) data and write to file for use by
#' the EM.
#'
#' @details Take a \code{data.SS_new} file containing expected values and
#' sample from true lengths, using length comp sample sizes, to get
#' realistic sample sizes for age bins given a length. If no fish are
#' sampled then that row is discarded. A value of NULL for fleets indicates
#' to delete the data so the EM
#' @author Cole Monnahan
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param datfile A path to the data file, outputed from an OM, containing
#' the expected conditional age-at-length, age comps and length comps. This
#' file is read in and then used to determine how many fish of each age bin
#' are to be sampled.
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}}
#' @export

sample_calcomp <- function(datfile, outfile, fleets = c(1,2), years,
                           write_file=TRUE){
    ## The samples are taken from the expected values, where the
    ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
    ## Lbin_hi, so subset those out, but don't delete the age values since
    ## those are already sampled from, or might be sampled later so need to
    ## leave them there.
    is_ssdat_file(datfile)
    agecomp.age <- datfile$agecomp[datfile$agecomp$Lbin_lo== -1,]
    agecomp.cal <- datfile$agecomp[datfile$agecomp$Lbin_lo != -1,]
    lencomp <- datfile$lencomp
    lbin_vector <- datfile$lbin_vector
    newfile <- datfile
    ## A value of NULL for fleets indicates not to sample and strip out the
    ## data from the file.
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
    if(nrow(agecomp.age)==0)
        stop("No agecomp data found -- something is wrong with sampling inputs")
    Nfleets <- length(fleets)
    if(any(!fleets %in% unique(agecomp.cal$FltSvy)))
        stop(paste0("The specified fleet number: ",fleets, " does not match input file"))
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    ## End input checks

    ## The general approach here is to loop through each fl/yr combination
    ## (for which there will be a row for each length bin) and then
    ## recombine them later.
    newcomp.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet
        for(i in 1:length(fleets)){
            fl <- fleets[i]
            agecomp.age.fl <- agecomp.age[agecomp.age$FltSvy == fl &
                                      agecomp.age$Yr %in% years[[i]], ]
            agecomp.cal.fl <- agecomp.cal[agecomp.cal$FltSvy == fl &
                                      agecomp.cal$Yr %in% years[[i]], ]
            if(length(years[[i]]) != length(unique((agecomp.age.fl$Yr))))
                stop(paste("A year specified in years was not found in the",
                           "input file for fleet", fl))
            ## Only loop through the subset of years for this fleet
            for(yr in years[[i]]) {
                newcomp <- agecomp.cal.fl[agecomp.cal.fl$Yr==yr, ]
                if(nrow(newcomp) != length(lbin_vector))
                    stop(paste("number of length bins does not match calcomp data: fleet", fl, ", year", yr))
                ## Get the sample sizes of the length and age comps.
                Nsamp.len <- lencomp$Nsamp[lencomp$Yr==yr & lencomp$FltSvy==fl]
                Nsamp.age <- agecomp.age$Nsamp[agecomp.age$Yr==yr & agecomp.age$FltSvy==fl]
                ## Probability distribution for length comps
                prob.len <- as.numeric(lencomp[lencomp$Yr==yr & lencomp$FltSvy==fl, -(1:6)])
                if(any(is.na(prob.len))) stop("Invalid length probs in sample_calcomp -- likely due to missing length data")
                ## Sample to get # fish in each length bin
                N1 <- rmultinom(n=1, size=Nsamp.len, prob=prob.len)
                ## Convert that to proportions
                p1 <- N1/sum(N1)
                N2 <- rmultinom(n=1, size=Nsamp.age, prob=p1)
                ## This is where the actual sampling takes place
                ## Loop through each length bin and sample # fish in
                ## each age bin, given expected conditional age-at-length
                newcomp$Nsamp <- N2
                for(ll in 1:nrow(newcomp)){
                    N.temp <- newcomp$Nsamp[ll]
                    if(N.temp>0){
                        cal.temp <- rmultinom(n=1, size=N2[ll], prob=as.numeric(newcomp[ll,-(1:9)]))
                    } else {
                        cal.temp <- -1
                    }
                    ## Write the samples back, leaving the other columns
                    newcomp[ll,-(1:9)] <- cal.temp
                }
                ## Drpo the -1 value which were temp placeholders
                newcomp <- newcomp[newcomp$Nsamp>0,]
                newcomp.list[[k]] <- newcomp
                k <- k+1
            }
        }
    ## End of loops doing the sampling.

    ## Combine back together into final data frame with the different data
    ## types
    newcomp.final <- do.call(rbind, newcomp.list)
    ## Cases for which data types are available. Need to be very careful
    ## here, as need to keep what's there.
### TODO: check this logic and simplify it. Probably don't need to check
### for agecomp.cal existing
    if(nrow(agecomp.age)>0){
        if(nrow(agecomp.cal)>0){
            ## age and cal
            newcomp.final <- rbind(agecomp.age, newcomp.final)
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)
        } else {
            ## age but not cal
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)
        }
    } else {
        ## only cal
        if(nrow(agecomp.cal)>0){
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)
        } else {
            ## no age nor cal data
            newfile$agecomp <- NULL
            newfile$N_agecomp <- 0
        }
    }
        ## Write the modified file
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile,
                          overwrite = TRUE, verbose=FALSE)
    return(invisible(newfile))
}


