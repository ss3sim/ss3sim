#' Sample empirial weight-at-age data and write to file for use by the EM.
#'
#' @details Take a \code{data.SS_new} file containing expected values and sample
#' from true ages to get realistic proportions for the number of fish in
#' each age bin, then use the mean size-at-age and CV for growth to
#' generate random samples of size, which are then converted to weight and
#' averaged to get mean weight-at-age values. These values are then written
#' to file for the EM. By calling this function, \code{ss3sim} will turn on
#' the empirical weight-at-age function (set maturity option to 5)
#' automatically. See \link{\code{ss3sim_base}} for more details on how
#' that is implemented.
#'
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
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}}
#' @export

sample_wtatage <- function(infile, outfile, datfile, ctlfile, fleets = 1,
                           years, write_file=TRUE){
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function.
    ##
    if(is.null(fleets)) return(NULL)
### ***TODO PETER**** Why are the years negative? Here I'm turning them
### negative but might want to change this??
    years <- lapply(years, function(xx) -xx)
    ## Read in datfile, need this for true age distributions and Nsamp
    datfile <- r4ss::SS_readdat(file=datfile, verbose=FALSE)
    agecomp <- datfile$agecomp
    agebin_vector <- datfile$agebin_vector
    mlacomp <- datfile$MeanSize_at_Age_obs
    if(is.null(mlacomp)) stop("No mean length-at-age data found in datfile")
    ## Read in the control file
    ctl <- r4ss::SS_parlines(ctlfile)
    ## Read in the file and grab the expected values
    infile <- readLines(infile)
    ## Remove double spaces, which SS3 writes in the 7th column
    infile <- gsub("  ", replace=" ", x=infile)
    xx <- grep(x=infile, "#yr seas gender growpattern birthseas fleet")
    if(length(xx)!=1) stop("Failed to read in wtatage file")
    header <- unlist(strsplit(infile[xx], " "))
    ## It appears the first three lines need to be there for some
    ## reason. ****TODO Peter****: fix this if need be??
    wtatage <- infile[(xx+4):length(infile)]
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
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    ## End input checks

    ## Resample from the length-at-age data The general approach here is to
    ## loop through each row and sample based on the true age distirbution
    ## and age-weight relationship distribution. Note, true age
    ## distribution is known, as is length->weight relationship, but there
    ## is uncertainty in the age->length relationship. This uncertainty
    ## defines the distribution from which we sample. It is also based on
    ## the # of age samples taken, to mimic reality better.
    wtatage.new.list <- list() # temp storage for the new rows
    k <- 1                 # each k is a new row of data, to be rbind'ed later
    ## Loop through each fleet, if fleets=NULL then skip sampling and
    ## return nothing (subtract out this type from the data file)
    for(fl in 1:length(fleets)){
        fl.temp <- fleets[fl]
        wtatage.fl <- wtatage[wtatage$fleet == fleets[fl],]
        for(j in 1:NROW(wtatage.fl)){
            yr.temp <- -wtatage.fl$yr[j]
            wtatage.new <- wtatage.fl[j,]
            mla.means <- as.numeric(mlacomp[mlacomp$Yr==yr.temp &
                                            mlacomp$Fleet==fl.temp, paste0("a",
                                            agebin_vector)])
            ## For each age, given year and fleet, get the expected length
            ## and CV around that length, then sample from it using
            ## lognormal (below)
            CV.growth <- ctl[ctl$Label=="CV_young_Fem_GP_1", "INIT"]
            ## These two params convert length to weight
            Wtlen1 <- ctl[ctl$Label=="Wtlen_1_Fem", "INIT"]
            Wtlen2 <- ctl[ctl$Label=="Wtlen_2_Fem", "INIT"]
            sds <- mla.means*CV.growth
            ## These are the moments on the natural scale, so
            ## convert to log scale and generate data
            means.log <- log(mla.means^2/sqrt(sds^2+mla.means^2))
            sds.log <- sqrt(log(1 + sds^2/mla.means^2))
            ## Each row is a year, so check that this row was specified by
            ## user, if it is then sample from it, if not use the previous
            ## row of sampled data.
            if(-yr.temp %in% years[[fl]]){
                ## First step, draw from the true age distributions
                agecomp.temp <- agecomp[agecomp$Yr==yr.temp & agecomp$FltSvy==fl.temp,]
                ## If this row is not output in the .dat file (nrow==0),
                ## skip the sampling and fill later
                if(nrow(agecomp.temp)==1){
                    ## Get the true age distributions
                    age.means <- as.numeric(agecomp.temp[-(1:9)])
                    age.Nsamp <- as.numeric(agecomp.temp$Nsamp)
                    ## Draw samples to get # of fish in each age bin
                    age.samples <- rmultinom(n=1, size=age.Nsamp, prob=age.means)
                    ## apply sampling across the columns (ages) to get
                    ## sample of lengths
                    lengths.list <-
                        lapply(1:length(means.log), function(kk)
                               exp(rnorm(n=age.samples[kk], mean=means.log[kk], sd=sds.log[kk])))
                    ## Convert lengths into weights
                    weights.list <- lapply(lengths.list, function(kk) Wtlen1*kk^Wtlen2)
                    ## Take means and combine into vector to put back
                    ## into the data frame.
                    wtatage.new.means <- do.call(c, lapply(weights.list, mean))
                    ## Sometimes you draw 0 fish from an age class,
                    ## resulting in NaN for the mean wtatage. For now,
                    ## replace with filler values
### TODO PETER: Fix this so it uses the same age bin from the last
### year. But need to be careful to check that that one also exists
                    wtatage.new.means[is.nan(wtatage.new.means)] <- -1
### ***TODO PETER***: for some reason we're missing age 0 in the .dat file, but
### the wtatage file needs it. So I can't sample from it, so for now just
### using the true expected value (cheating!!) (should be -(1:6))
                    wtatage.new[-(1:7)] <- wtatage.new.means
                    wtatage.new.list[[k]] <- wtatage.new
                } else {## no data to sample from, so fill with NA now and come
                    ## back to this later
                    wtatage.new[-(1:6)] <- NA
                    wtatage.new[1] <- -yr.temp
                    wtatage.new.list[[k]] <- wtatage.new
                }
            } else {
                wtatage.new.list[[k]] <- wtatage.new.list[[k-1]]
                wtatage.new.list[[k]]$yr <- wtatage.new$yr
            }
            k <- k+1
        }
    }
    ## Combine new rows together into one data.frame
    wtatage.matrix <- do.call(rbind, wtatage.new.list)
    ## Need to fix the first chunk of missing years. For now, loop through
    ## in reverse and paste them backward if full of NA, which we used
    ## above if that year was missing
### *** TODO PETER *** what happens if the last year is missing? This will
### break
    for(ii in rev(1:nrow(wtatage.matrix))){
        if(sum(is.na(wtatage.matrix[ii,]))>1){
            if(ii==nrow(wtatage.matrix)) stop("Last year in wtatage missing")
            wtatage.matrix[ii,-1] <- wtatage.matrix[ii+1,-1]
        }
    }

    ## prepare new data to be written back to file
    wtatage.rows <- apply(wtatage.matrix, MARGIN=1, FUN=function(x) paste(x, collapse=" "))
    wtatage.final <- c(infile[1:(xx+3)], wtatage.rows)
### *** TODO PETER *** is it always 3 lines? safe to hard code this??
    wtatage.final[1] <- paste(length(wtatage.matrix)+3, "#number_of_rows_determined_by_function:sample_wtatage")
    ## Write the modified file
    if(write_file) writeLines(wtatage.final, con=outfile)
    return(invisible(wtatage.final))
}
