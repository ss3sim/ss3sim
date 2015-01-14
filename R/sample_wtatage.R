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
# 
# #For Debugging
# setwd('/Users/peterkuriyama/School/Research/capam_growth/sample_wtatage_test/')
# source('fill_across.r')
# 
# infile <- "om/wtatage.ss_new"
# outfile <- "em/wtatage.ss"
# datfile <- "em/ss3.dat"
# ctlfile <- "om/control.ss_new"
# years <- list(seq(2, 100, 1), seq(2, 100, 1))
# fill_fnc <- fill_across
# fleets <- list(1, 2)
# cv.wtatage <- .01
# write_file <- TRUE
# 
# test <- sample_wtatage(infile = infile, outfile = outfile, datfile = datfile, 
#     ctlfile = ctlfile, years = years, fill_fnc = fill_across, 
#     fleets = fleets, cv.wtatage = .01)
# 

sample_wtatage <- function(infile, outfile, datfile, ctlfile,
                           years, fill_fnc = fill_across, write_file=TRUE, fleets,
                           cv.wtatage = NULL){
    ##fill_type: specify type of fill, fill zeroes with first row? annual interpolation?
        ## Age Interpolation?
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function
    if(is.null(fleets)) return(NULL)

    ### ACH: Because you always have to have year 100, you may want to check for duplicates
    # years <- years[!duplicated(years)]

    #----------------------------------------------------------------------------
    ## Read in datfile, need this for true age distributions and Nsamp
    #unchanged from previous versions
    datfile <- r4ss::SS_readdat(file=datfile, verbose=FALSE)
    agecomp <- datfile$agecomp
    agebin_vector <- datfile$agebin_vector
    # agebin_vector <- c(0, agebin_vector)

    mlacomp <- datfile$MeanSize_at_Age_obs
    if(is.null(mlacomp)) stop("No mean length-at-age data found in datfile")
    ## Read in the control file
    ctl <- r4ss::SS_parlines(ctlfile)
    ## Read in the file and grab the expected values
    infile <- readLines(infile)

    ## Remove double spaces, which SS3 writes in the 7th column
    infile <- gsub("  ", replacement=" ", x=infile)
    xx <- grep(x=infile, "#yr seas gender growpattern birthseas fleet")
    if(length(xx)!=1) stop("Failed to read in wtatage file")
    header <- unlist(strsplit(infile[xx], " "))
    header[-(1:6)] <- paste("age",header[-(1:6)],sep="")
    ## It appears the first three lines need to be there for some
    ## reason. ****TODO Peter****: fix this if need be??

    wtatage <- infile[(xx+1):length(infile)]
    wtatage <-  as.data.frame(matrix(as.numeric(unlist(strsplit(wtatage, split=" "))),
                                     nrow=length(wtatage), byrow=TRUE))
    names(wtatage) <- gsub("#", replacement="", x=header)
    wtatage$yr <- abs(wtatage$yr)
    if(2 %in% unique(wtatage$fleet) == FALSE){
        ones <- subset(wtatage, fleet == 1)
        twos <- ones
        twos$fleet <- 2
        wtatage <- rbind(wtatage, twos)
    }

    age0 <- wtatage[!duplicated(wtatage$fleet),c("fleet","age0")]
    wtatage.new.list <- list(1, 2) # temp storage for the new rows

    #----------------------------------------------------------------------------
    #start sampling
    #Pull wtatage for fleets -2, -1, and 0 for now. -2 is age-specific fecundity * maturity
    #-1 is population wt-at-age in middle of season
    #0 is population wt-at-age in beginning of season2
    unsampled.wtatage <- list(1, 2, 3)
    unsampled.wtatage[[1]] <- wtatage[which(wtatage$fleet == -2), ]
    unsampled.wtatage[[2]] <- wtatage[which(wtatage$fleet == -1), ]
    unsampled.wtatage[[3]] <- wtatage[which(wtatage$fleet == 0), ]

    #Change all years to negatives so it will work with ss
    unsampled.wtatage <- lapply(unsampled.wtatage, 
        function(x){
              x$yr <- -x$yr
              return(x)
        })

    #Start Sampling other fleets
    # fl <- 1
    # yr <- 1
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
                
                #----------------------------------------------------------------------------------------------------
                #Step 1, draw from true age distributions
                agecomp.temp <- agecomp[agecomp$Yr==yr & agecomp$FltSvy == fl, ]                
                #ACH: I'm going with the motto of think about it. Why enter a year for wtatage when you do not have data?
                if(nrow(agecomp.temp)==0) {stop("No age comp observations for year",yr,"and fleet",fl,"\n")}
                
                ## Get the true age distributions
                age.means <- as.numeric(agecomp.temp[-(1:9)])
                age.Nsamp <- as.numeric(agecomp.temp$Nsamp)
                
                #----------------------------------------------------------------------------------------------------
                #Step 2, determine # of fish in sample per bin
                #Use a multinomial here but may need to have dirichlet or option to toggle between the two
                age.samples <- rmultinom(n = 1, size = age.Nsamp, prob = age.means)

                #----------------------------------------------------------------------------------------------------
                #Step 3, use mean length-at-age to sample with normal/lognormal and user-specified cv
                #first define mean length-at-age
                mla.means <- as.numeric(mlacomp[mlacomp$Yr==yr & mlacomp$Fleet==fl,
                                                paste0("a", agebin_vector)])

                # CV.growth <- ctl[ctl$Label=="CV_young_Fem_GP_1", "INIT"]
                CV.growth <- cv.wtatage #User-Specified
                
                #Define growth parameters
                Wtlen1 <- ctl[ctl$Label=="Wtlen_1_Fem", "INIT"]
                Wtlen2 <- ctl[ctl$Label=="Wtlen_2_Fem", "INIT"]
                sds <- mla.means*CV.growth

                #Sample: I used a for loop to keep things understandable for me. could use apply also
            
                #create empty list to store lengths and weights
                lengths.list <- as.list(seq(1:nrow(age.samples)))
                weights.list <- lengths.list
                
                #fill in list by sampling from normal distribution
                for(ii in 1:nrow(age.samples))
                {
                  lengths.list[[ii]] <- supressWarnings(rnorm(n = age.samples[ii], mean = mla.means[ii], sd = sds[ii]))
                  
                  #Step 4, convert lengths to weights with no error
                  weights.list[[ii]] <- Wtlen1 * lengths.list[[ii]] ^ Wtlen2 
                }
                
                #step 5, calculate new mean weight at age
                samp.wtatage <- sapply(weights.list, mean)

                #concatenate everything
                prefix <- wtatage[wtatage$yr==yr & wtatage$fleet==1,1:5]  #I used fleet=1 because wtatage_new only outputs fleet 1
                tmp.fl <- fl
                wtatage.new.means <- c(unlist(prefix), tmp.fl, age0[age0$fleet == 1, 'age0'],
                    samp.wtatage)

                #store to wtatage.new.list
                wtatage.new.list[[fl]][as.character(yr), ] <- wtatage.new.means
            }
        }
    }

    #fill in missing values
    wtatage.complete <- lapply(wtatage.new.list,fill_fnc,minYear=datfile$styr,maxYear=datfile$endyr)

    # wtatage.new.list[[1]]
    # temp.wtatage <- wtatage.new.list[[1]]

    # filled.wtatage <- fill_fnc(mat = temp.wtatage, minYear = datfile$styr, maxYear = datfile$endyr)
    
    #Manually check
    # write.csv(temp.wtatage, file = 'wtatage1_gaps.csv')
    # write.csv(filled.wtatage, file = 'wtatage1_filled.csv')
    # save(wtatage.complete, file = '/Users/peterkuriyama/School/Research/capam_growth/Empirical/runs/wtatage.complete.Rdata')
    
    # wtatage.complete[[datfile$Nfleet+1]] <- wtatage.complete[[datfile$Nfleet]]
    # print(datfile$Nfleet+1)

    # fltNeg1 <- fltZero <- wtatage.complete[[datfile$Nfleet+1]]  #first survey
    # fltNeg1 <- fltZero <- wtatage.complete[[datfile$Nfleet+1]]  #first survey
    # fltNeg1$fleet <- -1
    # fltZero$fleet <- 0



    #----------------------------------------------------------------------------------------------------
    #Comment this out because I just copied from the original file
    # mat.fn <- function(x,age) {
    #     omega3 <- x[1]
    #     omega4 <- x[2]
    #     den <- 1+exp(omega3*(age-omega4))
    #     return(1/den)
    # }
    
    # # ctl[ctl$Label=="Mat_slope_Fem","INIT"]
    # # ctl[ctl$Label=="Mat50%_Fem","INIT"]
    # # # print(c(ctl[ctl$Label=="Mat_slope_Fem","INIT"],ctl[ctl$Label=="Mat50%_Fem","INIT"]))

    # matAtAge <- mat.fn(c(ctl[ctl$Label=="Mat_slope_Fem","INIT"],ctl[ctl$Label=="Mat50%_Fem","INIT"]),agebin_vector)
    # matAtAge <- c(0, matAtAge)

    # # fecund <- matAtAge * wtatage.complete[[datfile$Nfleet+1]][,-(1:6)]
    # #Adjusted Matrix Multiplication
    # fecund <- t(apply(wtatage.complete[[datfile$Nfleet+1]][,-(1:6)], 1, FUN = function(x) matAtAge * x))       

    # fecund <- cbind(wtatage.complete[[datfile$Nfleet+1]][,1:6],fecund)
    # fecund$fleet <- -2

    # Nlines <- nrow(fecund)+nrow(fltNeg1)+nrow(fltZero)
    Nlines <- length(unsampled.wtatage)
    Nlines <- Nlines + sum(unlist(lapply(wtatage.complete, nrow)))

    # Nlines <- Nlines + sum(unlist(lapply(wtatage.complete,nrow)))

    ##write wtatage.ss file
    if(write_file)    cat(Nlines,"# Number of lines of weight-at-age input to be read\n",file=outfile)
    if(write_file)    cat(datfile$Nages,"# Maximum Age\n",file=outfile,append=TRUE)

    #loop through the various matrices and build up wtatage.final while doing it
    wtatage.final <- list()

    if(write_file)     cat("#Fleet -2, fecundity\n",file=outfile,append=TRUE)
    wtatage.final[[1]] <- unsampled.wtatage[[1]]

    # wtatage.final[[1]] <- fecund
    # wtatage.final[[1]]$yr <- -1 * wtatage.final[[1]]$yr
    if(write_file)    write.table(unsampled.wtatage[[1]], file=outfile, append=TRUE, row.names=F, col.names=F)

    if(write_file)    cat("\n#Fleet -1\n",file=outfile,append=TRUE)
    # wtatage.final[[2]] <- fltNeg1
    wtatage.final[[2]] <- unsampled.wtatage[[2]]
    # wtatage.final[[2]]$yr <- -1 * wtatage.final[[2]]$yr
    if(write_file)        write.table(unsampled.wtatage[[2]], file=outfile, append=TRUE, row.names=F, col.names=F)

    if(write_file)    cat("\n#Fleet 0\n",file=outfile,append=TRUE)
    # wtatage.final[[3]] <- fltZero
    wtatage.final[[3]] <- unsampled.wtatage[[3]]
    # wtatage.final[[3]]$yr <- -1 * wtatage.final[[3]]$yr
    if(write_file)    write.table(unsampled.wtatage[[3]], file=outfile, append=TRUE, row.names=F, col.names=F)

    #loop through fleets
    for(i in fleets) {
        if(write_file)     cat("\n#Fleet",i,"\n",file=outfile,append=TRUE)
        wtatage.final[[i+3]] <- wtatage.complete[[i]]
        wtatage.final[[i+3]]$yr <- -1 * wtatage.final[[i+3]]$yr
        if(write_file)    write.table(wtatage.final[[i+3]], file=outfile, append=TRUE, row.names=F, col.names=F)
    }

    return(invisible(wtatage.final))
}




