#' Sample empirial weight-at-age data and write to file for use by the EM.
#'
#' Take a \code{wtatage.SS_new} file containing expected values and sample
#' to create observed data which are then written to file for use by the
#' estimation model.
#'
#' @author Cole Monnahan
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @param cv A list of coefficient of variation to use for each
#' fleet. Single values are repeated for given number of \code{years}, else
#' the length must be the same as the \code{years}.
#'
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}}
#' @export

sample_wtatage <- function(infile, outfile, fleets = 1, Nsamp,
                           years, cv, write_file=TRUE){
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function.
    if(is.null(fleets)) return(NULL)
    ## Read in the file and grab the expected values
    infile <- readLines(infile)
    ## Remove double spaces, which SS3 writes in the 7th column
    infile <- gsub("  ", replace=" ", x=infile)
    xx <- grep(x=infile, "#yr seas gender growpattern birthseas fleet")
    if(length(xx)!=1) stop("Failed to read in wtatage file")
    header <- unlist(strsplit(infile[xx], " "))
    wtatage <- infile[(xx+1):length(infile)]
    wtatage <-  as.data.frame(matrix(as.numeric(unlist(strsplit(wtatage, split=" "))),
                                     nrow=length(wtatage), byrow=TRUE))
    names(wtatage) <- gsub("#", replace="", x=header)
    ## Drop fleets that arent used
    wtatage <- wtatage[wtatage$fleet %in% fleets,]
    ## Check inputs for errors
    if(NROW(wtatage)==0) stop("Specified fleets not found in file")
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- length(fleets)
    if(class(Nsamp) != "list" | length(Nsamp) != Nfleets)
        stop("Nsamp needs to be a list of same length as fleets")
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    ## Test Nsamp and fill out the input if single values given
    for(i in 1:Nfleets){
        if(length(Nsamp[[i]])>1){
            if(length(Nsamp[[i]]) != length(years[[i]])){
                stop(paste0("Length of Nsamp does not match length of years for",
                            "fleet ",fleets[i]))
            }
        } else { ## replicate the 1 value for all years
            Nsamp[[i]] <- rep(Nsamp[[i]], times=length(years[[i]]))
        }
    }
    ## Test cv and fill out the input if single values given
    for(i in 1:Nfleets){
        if(length(cv[[i]])>1){
            if(length(cv[[i]]) != length(years[[i]])){
                stop(paste0("Length of cv does not match length of years for",
                            "fleet ",fleets[i]))
            }
        } else { ## replicate the 1 value for all years
            cv[[i]] <- rep(cv[[i]], times=length(years[[i]]))
        }
    }
    ## End input checks

    ## Resample from the length-at-age data The general approach here is to
    ## loop through each row and resample for the given CV and sample
    ## size. All these rows are then combined back together to form the
    ## final data which is then written to file.
    wtatage.new.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet, if fleets=NULL then skip sampling and
    ## return nothing (subtract out this type from the data file)
    for(i in 1:length(fleets)){
        wtatage.fl <- wtatage[wtatage$fleet == fleets[i],]
        if(wtatage.fl$yr[1] != years[[i]][1])
            stop("First year to be sampled must match first year of expected values")
        for(j in 1:NROW(wtatage.fl)){
            wtatage.new <- wtatage.fl[j,]
            ## Each row is a year, so check that this row was passed,
            ## if it is then sample from it, if not use the previous
            ## row of sampled data.
            if(wtatage.new$yr %in% years[[i]]){
                ## Need to be careful about getting the sample size and cv
                ## since the j loop is not over years specified, but what
                ## is in the file.
                n.temp <- Nsamp[[i]][which(wtatage.new$yr==years[[i]])]
                cv.temp <- cv[[i]][which(wtatage.new$yr==years[[i]])]
                ## Replace expected values with sampled values
                ## First 1-6 cols aren't data so skip them
                means <- as.numeric(wtatage.new[-(1:6)])
                sds <- means*cv.temp
                ## These are the moments on the natural scale, so
                ## convert to log scale and generate data
                means.log <- log(means^2/sqrt(sds^2+means^2))
                sds.log <- sqrt(log(1 + sds^2/means^2))
                ## apply sampling across the columns
                samples.list <-
                    lapply(1:length(means.log), function(kk)
                         exp(rnorm(n=n.temp, mean=means.log[kk], sd=sds.log[kk])))
                if(any(is.na(samples.list))) print(c(j, wtatage.new$yr, n.temp))
                ## Take means and combine into vector to put back
                ## into the data frame
                wtatage.new[-(1:6)] <- do.call(c, lapply(samples.list, mean))
                wtatage.new.list[[k]] <- wtatage.new
            } else {
                wtatage.new.list[[k]] <- wtatage.new.list[[k-1]]
                wtatage.new.list[[k]]$yr <- wtatage.new$yr
            }
            k <- k+1
        }
    }

    ## Combine new rows together into one data.frame
    wtatage.matrix <- do.call(rbind, wtatage.new.list)

    ## Make changes to the wtatage file. First, put the number of rows of
    ## new data at the top
    infile[1] <- paste(NROW(wtatage.matrix), "#number_of_rows_determined_by_function:sample_wtatage")
    ## Collapse the matrix back to character rows and combine with first part
    wtatage.rows <- apply(wtatage.matrix, MARGIN=1, FUN=function(x) paste(x, collapse=" "))
    wtatage.final <- c(infile[1:xx], wtatage.rows)
    ## Write the modified file
    if(write_file) writeLines(wtatage.final, con=outfile)
    return(invisible(wtatage.matrix))
}
