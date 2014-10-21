#' Sample conditional age-at-length compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed length-at-age compositions which are then written to
#' file for use by the estimation model.
#'
#' @author Cole Monnahan
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param agebin_vector Depreciated argument. Does nothing and will be
#'   removed in a future major version update. Instead, see
#'   \code{change_bin}.
#' @param cv A vector of coefficient of variation to use for each fleet.
#'
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}}
#' @export


sample_calcomp <- function(infile, outfile, fleets = c(1,2), Nsamp,
                           years, cv, write_file=TRUE){
    ## Check inputs for errors

    ## The samples are taken from the expected values, where the
    ## age-at-length data is in the age matrix but has a -1 for Lbin_lo and
    ## Lbin_hi, so subset those out, but don't delete the age values since
    ## those are already sampled from, or might be sampled later so need to
    ## leave them there.
    agecomp <- infile$agecomp
    agecomp.age <- agecomp[agecomp$Lbin_lo== -1,]
    agecomp.cal <- agecomp[agecomp$Lbin_lo != -1,]
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- ifelse(is.null(fleets), 0, length(fleets))
    if(Nfleets >0 & FALSE %in% (fleets %in% unique(agecomp.cal$FltSvy)))
        stop(paste0("The specified fleet number does not match input file"))
    if(Nfleets!= 0 & (class(Nsamp) != "list" | length(Nsamp) != Nfleets))
        stop("Nsamp needs to be a list of same length as fleets")
    if(Nfleets!= 0 & (class(years) != "list" | length(years) != Nfleets))
        stop("years needs to be a list of same length as fleets")
    ## If no fleets are specified then skip these
    if (Nfleets>0){
        for(i in 1:Nfleets){
            if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
                stop(paste0("Length of Nsamp does not match length of years for",
                            "fleet ",fleets[i]))
        }
    }
    ## End input checks

    ## Resample from the length-at-age data
    ## The general approach here is to loop through each row to keep
    ## (depends on years input) and resample depending on Nsamp and
    ## cvar. All these rows are then combined back together to form
    ## the final ccomp.
    newcomp.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet, if fleets=NULL then skip sampling and
    ## return nothing (subtract out this type from the data file)
    if (Nfleets>0){
        for(i in 1:length(fleets)){
            fl <- fleets[i]
            agecomp.fl <- agecomp.cal[agecomp.cal$FltSvy == fl &
                                      agecomp.cal$Yr %in% years[[i]], ]
            if(length(years[[i]]) != length(unique((agecomp.fl$Yr))))
                stop(paste("A year specified in years was not found in the",
                           "input file for fleet", fl))
            agecomp.fl$Nsamp <- Nsamp[[i]]
            ## Only loop through the subset of years for this fleet
            for(yr in years[[i]]) {
                newcomp <- agecomp.fl[agecomp.fl$Yr==yr, ]
                ## Now loop through each row and sample for each age bin
                ## (column)
                for(j in 1:NROW(newcomp)){
                    newcomp.lbin <- newcomp[j,]
                    ## Replace expected values with sampled values
                    ## First 1-9 cols aren't data so skip them
                    means <- as.numeric(newcomp.lbin[-(1:9)])
                    sds <- means*cv[i]
                    ## These are the moments on the natural scale, so
                    ## convert to log scale and generate data
                    means.log <- log(means^2/sqrt(sds^2+means^2))
                    sds.log <- sqrt(log(1 + sds^2/means^2))
                    samples.list <- lapply(1:length(means), function(kk)
                        exp(rnorm(n=newcomp.lbin$Nsamp, mean=means.log[kk], sd=sds.log[kk])))
                    ## Take means and combine into vector to put back
                    ## into the data frame
                    newcomp.lbin[-(1:9)] <- do.call(c, lapply(samples.list, mean))
                    newcomp.list[[k]] <- newcomp.lbin
                    k <- k+1
                }
            }
        }
    }

    ## Combine back together into final data frame with the different data
    ## types
    newfile <- infile
    if(Nfleets>0){
        newcomp.final <- do.call(rbind, newcomp.list)
        ## Case with both types
        if(nrow(agecomp.age)>0){
            newcomp.final <- rbind(newcomp.final, agecomp.age)
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)

        } else { ## case with only conditional data
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)
             }
    } else {
        ## Case with only age data
        if(nrow(agecomp.age)>0){
            newcomp.final <- agecomp.age
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)

        } else {
            ## case with no data of either type
            newcomp.final <- data.frame("#")
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- 0
        }
    }

    ## Write the modified file
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile,
                          overwrite = TRUE, verbose=FALSE)
    return(invisible(newcomp.final))
}
