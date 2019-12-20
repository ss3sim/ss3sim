#' Sample conditional age-at-length (CAL) data
#'
#' Samples conditional age-at-length (CAL) data from expected values of length
#' proportions and expected values of age proportions (conditional on length)
#' from the operating model (OM) and writes the samples to file for use by the
#' estimation model (EM).
#'
#' @details This function takes a \code{data.SS_new} file from an OM containing
#'  expected values of length proportions and age proportions, conditional on
#'  length. First, sample from true length proportions, using the length comp
#'  sample sizes, to get realistic sample sizes for age bins given their lengths.
#'  Note that the overall total sample size for all CAL bins is specified by
#'  the user for the given fleet and year in \code{Nsamp}. Next,
#'  use these sample sizes and the expected values of age proportions
#'  (conditional on length) to sample for realistic age proportions. If no fish
#'  are sampled, for a row of age proportions in conditional age at length data,
#'  then that row is discarded. A value of NULL for fleets indicates to delete
#'  the conditional age at length data data (but not the marginal age data). If
#'  the EM is used with \code{\link{run_ss3sim}}, the case file should be named
#'   \code{calcomp}. Only the multinomial distribution is currently implemented,
#'  so this function cannot be used with the dirichlet distribution.
#'  Note that this function cannot handle all types of conditional age at length
#'  sampling. This function requires that there be a row of conditional age at
#'  length data for each length data bin (for each year and fleet that sampling
#'  is specified to be performed), where Lbin_lo and Lbin_hi are the
#'  same value.
#'  Note also that this sampling procedure represents simple random sampling for
#'  conditional age at length, where 1) lengths are sampled randomly, 2) fish
#'  are lengthed and placed into bins, and 3) a subset of lengthed fish are
#'  aged, where a constant proportion from each length bin are selected for
#'  aging. This does NOT represent length stratified sampling where a subset of
#'  lengthed fish are aged, and a constant number from each length bin is
#'  selected for aging, although these data could also be put into a Stock
#'  Synthesis model as "Conditional Age at Length."
#' @note This function is only reliable when using multinomial length
#'  compositions for the fleet(s) with conditional age at length sampling. The
#'  real-valued length compositions resulting from the Dirichlet distribution
#'  cause difficulties in the sampling code. See the vignette for more
#'  information.
#' @author Cole Monnahan, Kotaro Ono
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param Nsamp *A numeric list of the same length as fleets. Either single
#'  values or vectors of the same length as the number of years can be passed
#'  through. Single values are repeated for all years. If no fleet collected
#'  samples, specify \code{Nsamp = NULL}. Specifically, for
#'  \code{sample_calcomp}, \code{Nsamp} denotes the total number of conditional
#'   age at length samples for a given year and fleet across all length bins.
#' @template sampling-return
#' @template casefile-footnote
#' @family sampling functions
#' @export

sample_calcomp <- function(dat_list, outfile = NULL, fleets, years,
                           Nsamp){
    ## The samples are taken from the expected values, where the
    ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
    ## Lbin_hi, so subset those out, but don't delete the age values since
    ## those are already sampled from, or might be sampled later so need to
    ## leave them there.
    ## Input checks
    Nfleets <- NROW(fleets)
    if (Nfleets>0){
        for(i in seq_len(Nfleets)) {
            if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
                stop(paste0("Length of Nsamp does not match length of years for",
                  "fleet ",fleets[i]))
        }
    }
    check_data(dat_list)
    # Get necessary values
    ss_version <- get_ss_ver_dl(dat_list) # to use later in SS_writedat calls
    # Divide up age comp into marginal and CAL; we will only be sampling from
    # cal in this function, but want to retain the marginal age comps.
    agecomp.age <- dat_list$agecomp[dat_list$agecomp$Lbin_lo== -1,] # marginal
    agecomp.cal <- dat_list$agecomp[dat_list$agecomp$Lbin_lo != -1,] # CAL
    lencomp <- dat_list$lencomp
    lbin_vector <- dat_list$lbin_vector # This is the vector of data length bins
    newfile <- dat_list
    ## A value of NULL for fleets indicates not to sample and strip out the
    ## CAL data from the file.
    if(is.null(fleets)){
        newfile$agecomp <- agecomp.age # only leave in maraginal age comps
        # newfile$N_agecomp <- nrow(agecomp.age) # not needed in SS 3.30
        if(!is.null(outfile)){
          SS_writedat(datlist = newfile, outfile = outfile, version = ss_version,
                      overwrite = TRUE, verbose=FALSE)
        }
        return(invisible(newfile))
    }
    ## If not, do additional argument checks
    if(nrow(agecomp.cal)==0) { #TODO: maybe turn this into a warning instead?
        stop("No conditional age-at-length data found")
    }
    Nfleets <- length(fleets)
    ## changed this from .cal to .age
    if(any(!fleets %in% unique(agecomp.cal$FltSvy)))
        stop(paste0("The specified fleet number: ",fleets, " does not match input file"))
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    # check that Lbin_lo and Lbin_hi are the same for all CAL data. This is
    # a requirement of how the function is currently written
    if(!all(agecomp.cal$Lbin_lo == agecomp.cal$Lbin_hi)) {
      stop("In order to use sample_calcomp, for each row of conditional age at",
           " length data, Lbin_lo must equal Lbin_hi. Currently, this is not ",
           "the case for all conditional age at length data within dat_list.")
    }
    ## End input checks

    ## The general approach here is to loop through each fl/yr combination
    ## (for which there will be a row for each length bin) and then
    ## recombine them later.
    newcomp.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet
    for(i in seq_along(fleets)){
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
            #Note that the below check may be redundant...
            if(nrow(newcomp) != length(lbin_vector)) {
                stop("The number of conditional age at length data rows for ",
                "fleet ", fl, "and year ", yr, " is not the same as the number",
                " of length bins. For each fleet and year, please make sure ",
                "there is row where Lbin_lo and Lbin_hi are equal to each of ",
                "the values in lbin_vector: ",
                paste0(lbin_vector, collapse = ", "), ". Note that Lbin_lo and",
                "Lbin_hi if conditional age at length data should always have",
                "the same values in order for sample_calcomp() to work.")
            }
            if(NROW(newcomp) == 0) {
              stop("no age data found for fleet ", fl, "and yr ", yr)
            }
            ## Get the sample sizes of the length and age comps.
            Nsamp.len <- lencomp$Nsamp[lencomp$Yr==yr & lencomp$FltSvy==fl]
            ## Nsamp.age <- agecomp.age$Nsamp[agecomp.age$Yr==yr & agecomp.age$FltSvy==fl]
            ## Probability distribution for length comps
            prob.len <- as.numeric(lencomp[lencomp$Yr==yr & lencomp$FltSvy==fl, -(1:6)])
            if(any(is.na(prob.len))) stop("Invalid length probs in sample_calcomp -- likely due to missing length data")
            ## From observed length distribution, sample which fish to age.
            yr.ind <- which(years[[i]]==yr)
            if(Nsamp[[i]][yr.ind] > Nsamp.len) {
                stop("More age samples specified than fish collected for ",
                 "calcomps for fleet ", fl, " and year ", yr, ". Please ",
                 "adjust the Nsamp specified via sample_calcomp function input",
                 " for the given fleet in year so that it is less than the ",
                 "Nsamp in the length composition data for the same fleet and ",
                 "year.")
            }
            ## The Dirichlet case is annoying since the values of prob.len
            ## will be <1 and not whole fish, and we can't multiply by
            ## sample size to get them b/c they are real. Thus two cases:
            ## (1) If using multinomial for length resample empirically.
            if(any(prob.len>1)){
                ## This code creates a vector of empirical samples of
                ## length, such that each length bin is repeated equal to
                ## the number of observed fish in that bin
                prob.len.ints <- unlist(sapply(seq_along(prob.len), function(i) rep(i, prob.len[i])))
                ## Now resample from it, garuanteeing that the sample size
                ## doesn't exceed
                temp <- sample(x=prob.len.ints, size=Nsamp[[i]][yr.ind], replace=FALSE)
                Nsamp.ages.per.lbin <- sapply(seq_along(prob.len), function(i) sum(temp==i))
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
        for(ll in seq_len(nrow(newcomp))) {
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
            #newfile$N_agecomp <- NROW(newcomp.final) # Not needed in SS 3.30
        } else {
            ## age but not cal
            newfile$agecomp <- newcomp.final
            #newfile$N_agecomp <- NROW(newcomp.final) # Not needed in SS 3.30
        }
    } else {
        ## only cal
        if(NROW(agecomp.cal)>0){
            newfile$agecomp <- newcomp.final
            #newfile$N_agecomp <- NROW(newcomp.final) # Not needed in SS 3.30
        } else {
            ## no age nor cal data
            newfile$agecomp <- NULL
            #newfile$N_agecomp <- 0 # Not needed in SS 3.30
        }
    }

    ## Write the modified file
    if (!is.null(outfile)){
      r4ss::SS_writedat(datlist = newfile, outfile = outfile,
                        version = ss_version, overwrite = TRUE, verbose=FALSE)
    }
    return(invisible(newfile))
}


