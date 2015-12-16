#' Sample conditional age-at-length (CAL) data and write to file for use by
#' the EM.
#'
#' @details Take a \code{data.SS_new} file containing expected values and
#' sample from true lengths, using length comp sample sizes, to get
#' realistic sample sizes for age bins given a length. Only the multinomial
#' distribution is currently implemented. xIf no fish are sampled then that
#' row is discarded. A value of NULL for fleets indicates to delete the
#' data so the EM If used with \code{\link{run_ss3sim}} the case file
#' should be named \code{calcomp}.
#'
#' @note This function is only reliable when using multinomial length
#' compositions for the matching fleet. The real-valued length compositions
#' resulting from the Dirichlet distribution cause difficulties in the
#' sampling code. See the vignette for more.
#'
#' @author Cole Monnahan, Kotaro Ono
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template Nsamp
#' @template sampling-return
#' @template casefile-footnote
#' @family sampling functions
#' @export

sample_calcomp <- function(dat_list, outfile, fleets = c(1,2), years,
                           write_file=TRUE, Nsamp){
    ## The samples are taken from the expected values, where the
    ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
    ## Lbin_hi, so subset those out, but don't delete the age values since
    ## those are already sampled from, or might be sampled later so need to
    ## leave them there.
    ## Input checks
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
    lencomp <- dat_list$lencomp
    lbin_vector <- dat_list$lbin_vector
    newfile <- dat_list
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
    ## if(nrow(agecomp.age)==0)
    ##     stop("No agecomp data found -- something is wrong with sampling inputs")
    Nfleets <- length(fleets)
    ## changed this from .cal to .age
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
        if (length(Nsamp[[i]]) == 1) {
            Nsamp[[i]] <- rep(Nsamp[[i]], length(years[[i]]))
        }
        ## agecomp.age.fl <- agecomp.age[agecomp.age$FltSvy == fl &
        ##                               agecomp.age$Yr %in% years[[i]], ]
        agecomp.cal.fl <- agecomp.cal[agecomp.cal$FltSvy == fl &
                                      agecomp.cal$Yr %in% years[[i]], ]
        if(length(years[[i]]) != length(unique((agecomp.cal.fl$Yr))))
            stop(paste("A year specified in years was not found in the",
                       "input file for fleet", fl))
        ## Only loop through the subset of years for this fleet
        for(yr in years[[i]]) {
            newcomp <- agecomp.cal.fl[agecomp.cal.fl$Yr==yr, ]
            if(nrow(newcomp) != length(lbin_vector))
                stop(paste("number of length bins does not match calcomp data: fleet", fl, ", year", yr))
            if(NROW(newcomp) == 0) stop("no age data found")
            ## Get the sample sizes of the length and age comps.
            Nsamp.len <- lencomp$Nsamp[lencomp$Yr==yr & lencomp$FltSvy==fl]
            ## Nsamp.age <- agecomp.age$Nsamp[agecomp.age$Yr==yr & agecomp.age$FltSvy==fl]
            ## Probability distribution for length comps
            prob.len <- as.numeric(lencomp[lencomp$Yr==yr & lencomp$FltSvy==fl, -(1:6)])
            if(any(is.na(prob.len))) stop("Invalid length probs in sample_calcomp -- likely due to missing length data")
            ## From observed length distribution, sample which fish to age.
            yr.ind <- which(years[[i]]==yr)
            if(Nsamp[[i]][yr.ind] > Nsamp.len)
                stop("More age samples specified than fish collected for calcomps")
            ## The Dirichlet case is annoying since the values of prob.len
            ## will be <1 and not whole fish, and we can't multiply by
            ## sample size to get them b/c they are real. Thus two cases:
            ## (1) If using multinomial for length resample empirically.
            if(any(prob.len>1)){
                ## This code creates a vector of empirical samples of
                ## length, such that each length bin is repeated equal to
                ## the number of observed fish in that bin
                prob.len.ints <- unlist(sapply(1:length(prob.len), function(i) rep(i, prob.len[i])))
                ## Now resample from it, garuanteeing that the sample size
                ## doesn't exceed
                temp <- sample(x=prob.len.ints, size=Nsamp[[i]][yr.ind], replace=FALSE)
                Nsamp.ages.per.lbin <- sapply(1:length(prob.len), function(i) sum(temp==i))
                ## Note: If you're ageing all fish this isn't needed, but holds.
            } else {
                ## (2) case of Dirichlet. No way to verify more fish are
                ## not aged than were lengthed.
                Nsamp.ages.per.lbin <- rmultinom(n=1, size=Nsamp[[i]][yr.ind] , prob=prob.len)
            }
            ## Nsamp.ages.per.lbin is the column of sample sizes in the
            ## CAAL matrix, which gives the sample size to draw CAAL
            ## samples below.
        if(any(is.na(Nsamp.ages.per.lbin)))
            stop("Invalid age sample size for a length bin in calcomp")
        ## This is where the actual sampling takes place. Loop through each
        ## length bin and sample # fish in each age bin, given expected
        ## conditional age-at-length
        newcomp$Nsamp <- Nsamp.ages.per.lbin
        for(ll in 1:nrow(newcomp)){
            N.temp <- newcomp$Nsamp[ll]
            if(N.temp>0){
                cal.temp <-
                    rmultinom(n=1, size=Nsamp.ages.per.lbin[ll],
                              prob=as.numeric(newcomp[ll,-(1:9)]))
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
    if(NROW(agecomp.age)>0){
        if(NROW(agecomp.cal)>0){
            ## age and cal
            newcomp.final <- rbind(agecomp.age, newcomp.final)
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- NROW(newcomp.final)
        } else {
            ## age but not cal
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- NROW(newcomp.final)
        }
    } else {
        ## only cal
        if(NROW(agecomp.cal)>0){
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- NROW(newcomp.final)
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


