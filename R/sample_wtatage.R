#' Sample empirial weight-at-age data and write to file for use by the EM
#'
#' Take a \code{data.SS_new} file containing expected values and sample from
#' true ages to get realistic proportions for the number of fish in each age
#' bin, then use the mean size-at-age and CV for growth to generate random
#' samples of size, which are then converted to weight and averaged to get mean
#' weight-at-age values. Missing ages and years are filled according to a
#' specified function. These matrices are then written to file for the EM. By
#' calling this function, \pkg{ss3sim} will turn on the empirical weight-at-age
#' function (set maturity option to 5) automatically. See
#' \code{\link{ss3sim_base}} for more details on how that is implemented.
#'
#' @author Cole Monnahan, Allan Hicks, Peter Kuriyama
#'
#' @param infile The file to read weight-at-age from. Specifically to get the
#'   age-0 weight-at-age. This is typically \code{wtatage.ss_new}.
#' @param outfile The file to write the created weight-at-age matrices to be
#'   read in by the estimation model. Commonly \code{wtatage.ss}.
#' @param datfile A path to the data file, outputed from an OM, containing the
#'   true age distributions (population bins). This file is read in and then
#'   used to determine how many fish of each age bin are to be sampled. Commonly
#'   \code{data.ss_new}
#' @param ctlfile A path to the control file, outputed from an OM, containing
#'   the OM parameters for growth and weight/length relationship. These values
#'   are used to determine the uncertainty about weight for fish sampled in each
#'   age bin. Commonly \code{control.ss_new}
#' @param years *A list of vectors for each fleet indicating the years that are
#'   sampled for weight-at-age. There must be a corresponding age composition
#'   for that year in \code{datfile}.
#' @param fill_fnc *A function to fill in missing values (ages and years). The
#'   resulting weight-at-age file will have values for all years and ages.One
#'   function is \code{fill_across}.
#' @param write_file Logical to determine if \code{outfile} will be written.
#' @template sampling-return
#' @template casefile-footnote
#' @seealso \code{\link{sample_lcomp}, \link{sample_agecomp}},
#'   \code{\link{fill_across}}
#' @export

sample_wtatage <- function(infile, outfile, datfile, ctlfile,
                           years, fill_fnc, write_file=TRUE){
    ##fill_type: specify type of fill, fill zeroes with first row? annual interpolation?
        ## Age Interpolation?
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function.
    ##
    #if(is.null(fleets)) return(NULL)
### ***TODO PETER**** Why are the years negative? Here I'm turning them
### negative but might want to change this??
### I think this one's OK

### ACH: Because you always have to have year 100, you may want to check for duplicates
    years <- years[!duplicated(years)]

    #years <- lapply(years, function(xx) -xx)
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
    header[-(1:6)] <- paste("age",header[-(1:6)],sep="")
    ## It appears the first three lines need to be there for some
    ## reason. ****TODO Peter****: fix this if need be??

    wtatage <- infile[(xx+1):length(infile)]
    wtatage <-  as.data.frame(matrix(as.numeric(unlist(strsplit(wtatage, split=" "))),
                                     nrow=length(wtatage), byrow=TRUE))
    names(wtatage) <- gsub("#", replace="", x=header)
    wtatage$yr <- abs(wtatage$yr)
    age0 <- wtatage[!duplicated(wtatage$fleet),c("fleet","age0")]

    ## Drop fleets that arent used
    ##ACH: no need to do this, since you loop over fleets
    ##ACH:  by keepign everything, you can make sure to include everything
    #wtatage <- wtatage[wtatage$fleet %in% fleets,]

    ## Check inputs for errors
    ## ACH: This next line isn't very robust. If you are working with two fleets and one is in the matrix, it won't warn you
    ## ACH: I put a check in the loops below
    #if(NROW(wtatage)==0) stop("Specified fleets not found in file")

    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    fleets <- 1:length(datfile$fleetnames)
    #if(min(fleets) != 1) stop("Fleets must from 1 to the total number of fleets \n")
    #if(Nfleets != length(datfile$fleetnames)) stop("You must specify all fleets when smapling wtatage data\n")
    #Not sure why years has to be same length as nfleets
    #ACH: We should determine if we want the option of a single input year vector
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
    wtatage.new.list <- vector(length=length(fleets),mode="list") # temp storage for the new rows
    ## Loop through each fleet, if fleets=NULL then skip sampling and
    ## return nothing (subtract out this type from the data file)

 #####################################################################################
    ##ToDo: what if a fleet hasno age data, like fleet 3 in cod model?
    ##maybe if a single value is entered in year list, that is fleet to copy wtatage from
####################################################################################

    for(fl in fleets) { #fleets must be 1:Nfleets
        #set up wtatage matrix of sampled years
        wtatage.new.list[[fl]] <- as.data.frame(matrix(NA,nrow=length(years[[fl]]),ncol=ncol(wtatage)))
        names(wtatage.new.list[[fl]]) <- names(wtatage)
        row.names(wtatage.new.list[[fl]]) <- as.character(years[[fl]])

        #===============Loop over Years Sampled
        if(length(years[[fl]]) == 1) { #copy wtatage matrix from designated fleet
            if(fl <= years[[fl]]) stop("You must designate an earlier fleet to copy from.\n")
            wtatage.new.list[[fl]] <- wtatage.new.list[[years[[fl]]]]
            wtatage.new.list[[fl]]$fleet <- fl
        } else {
            for(yr in years[[fl]]) {
                cat('fl=',fl,'yr=', yr, '\n')

                mla.means <- as.numeric(mlacomp[mlacomp$Yr==yr & mlacomp$Fleet==fl,
                                                paste0("a", agebin_vector)])
                ## For each age, given year and fleet, get the expected length
                ## and CV around that length, then sample from it using
                ## lognormal (below)

                #######******************************************************************
                ## WE MAY WANT TO SAMPLE USING THE DISTRIBUTION ASSUMED IN THE MODEL
                ######*******************************************************************

                CV.growth <- ctl[ctl$Label=="CV_young_Fem_GP_1", "INIT"]
                ## These two params convert length to weight
                Wtlen1 <- ctl[ctl$Label=="Wtlen_1_Fem", "INIT"]
                Wtlen2 <- ctl[ctl$Label=="Wtlen_2_Fem", "INIT"]
                sds <- mla.means*CV.growth

                ## These are the moments on the natural scale, so
                ## convert to log scale and generate data
                ##***************************************************************
                #ACH CHECK THIS
                # I typicall use the cv to caluclate sd.log, then caluclate mu.log as log(mla.means)-1/2(sd.log^2)
                ##****************************************************************

                #means.log <- log(mla.means^2/sqrt(sds^2+mla.means^2))
                sds.log <- sqrt(log(1 + sds^2/mla.means^2))
                means.log <- log(mla.means)-0.5*sds.log^2

                ## Each row is a sampled year

                cat('fleet=', fl, 'year=', yr, 'We sampling', '\n')
                ## First step, draw from the true age distributions
                agecomp.temp <- agecomp[agecomp$Yr==yr & agecomp$FltSvy==fl,]
                ## If this row is not output in the .dat file (nrow==0), then no sampling
                #ACH: I'm going with the motto of think about it. Why enter a year for wtatage when you do not have data?
                if(nrow(agecomp.temp)==0) {stop("No age comp observations for year",yr,"and fleet",fl,"\n")}
                ## Get the true age distributions
                age.means <- as.numeric(agecomp.temp[-(1:9)])
                age.Nsamp <- as.numeric(agecomp.temp$Nsamp)
                ## Draw samples to get # of fish in each age bin
                age.samples <- rmultinom(n=1, size=age.Nsamp, prob=age.means)
                ## apply sampling across the columns (ages) to get sample of lengths

                lengths.list <- lapply(1:length(means.log), function(kk)
                                       exp(rnorm(n=age.samples[kk], mean=means.log[kk], sd=sds.log[kk])))
                ## Convert lengths into weights
                weights.list <- lapply(lengths.list, function(kk) Wtlen1*kk^Wtlen2)
                ## Take means and combine into vector to put back
                ## into the data frame.
                wtatage.new.means <- do.call(c, lapply(weights.list, mean))
                ## Sometimes you draw 0 fish from an age class,
                ## resulting in NaN for the mean wtatage. For now,
                ## replace with filler values

                #ACH: This is the wtatage for that year. Build up this matrix and pass to fill in function
                prefix <- wtatage[wtatage$yr==yr & wtatage$fleet==1,1:5]  #I used fleet=1 because wtatage_new only outputs fleet 1
                tmp <- fl#; names(tmp) <- "fleet"
                wtatage.new.means <- c(unlist(prefix),fl,age0[age0$fleet==1,"age0"],unlist(wtatage.new.means))  #using fleet 1 cuz wtatage.ss only has this

                wtatage.new.list[[fl]][as.character(yr),] <- wtatage.new.means
            }
        }
    }

    #call function to fill in missing values
    wtatage.complete <- lapply(wtatage.new.list,fill_across,minYear=datfile$styr,maxYear=datfile$endyr)

    fltNeg1 <- fltZero <- wtatage.complete[[datfile$Nfleet+1]]  #first survey
    fltNeg1$fleet <- -1
    fltZero$fleet <- 0

    mat.fn <- function(x,age) {
        omega3 <- x[1]
        omega4 <- x[2]
        den <- 1+exp(omega3*(age-omega4))
        return(1/den)
    }
    matAtAge <- mat.fn(c(ctl[ctl$Label=="Mat_slope_Fem","INIT"],ctl[ctl$Label=="Mat50%_Fem","INIT"]),agebin_vector)
    fecund <- matAtAge * wtatage.complete[[datfile$Nfleet+1]][,-(1:6)]
    fecund <- cbind(wtatage.complete[[datfile$Nfleet+1]][,1:6],fecund)
    fecund$fleet <- -2

    Nlines <- nrow(fecund)+nrow(fltNeg1)+nrow(fltZero)
    Nlines <- Nlines + sum(unlist(lapply(wtatage.complete,nrow)))

    ##write wtatage.ss file
    if(write_file)    cat(Nlines,"# Number of lines of weight-at-age input to be read\n",file=outfile)
    if(write_file)    cat(datfile$Nages,"# Maximum Age\n",file=outfile,append=TRUE)

    #loop through the various matrices and build up wtatage.final while doing it
    wtatage.final <- list()

    if(write_file)     cat("#Fleet -2, fecundity\n",file=outfile,append=TRUE)
    wtatage.final[[1]] <- fecund
    wtatage.final[[1]]$yr <- -1 * wtatage.final[[1]]$yr
    if(write_file)    write.table(fecund, file=outfile, append=TRUE, row.names=F, col.names=F)

    if(write_file)    cat("\n#Fleet -1\n",file=outfile,append=TRUE)
    wtatage.final[[2]] <- fltNeg1
    wtatage.final[[2]]$yr <- -1 * wtatage.final[[2]]$yr
    if(write_file)        write.table(fltNeg1, file=outfile, append=TRUE, row.names=F, col.names=F)

    if(write_file)    cat("\n#Fleet 0\n",file=outfile,append=TRUE)
    wtatage.final[[3]] <- fltZero
    wtatage.final[[3]]$yr <- -1 * wtatage.final[[3]]$yr
    if(write_file)    write.table(fltZero, file=outfile, append=TRUE, row.names=F, col.names=F)

    #loop through fleets
    for(i in fleets) {
        if(write_file)     cat("\n#Fleet",i,"\n",file=outfile,append=TRUE)
        wtatage.final[[i+3]] <- wtatage.complete[[i]]
        wtatage.final[[i+3]]$yr <- -1 * wtatage.final[[i+3]]$yr
        if(write_file)    write.table(wtatage.final[[i+3]], file=outfile, append=TRUE, row.names=F, col.names=F)
    }

    return(invisible(wtatage.final))
}
