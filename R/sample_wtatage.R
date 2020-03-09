#' Sample empirical weight-at-age data and write to file for use by the EM
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
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{wtatage}.
#'
#' @author Cole Monnahan, Allan Hicks, Peter Kuriyama
#'
#' @param wta_file_in The file to read weight-at-age from. Specifically to get the
#'   age-0 weight-at-age. This is typically \code{wtatage.ss_new}.
#' @param ctl_file_in A path to the control file, output from an OM, containing
#'   the OM parameters for growth and weight/length relationship. These values
#'   are used to determine the uncertainty about weight for fish sampled in each
#'   age bin. Commonly \code{control.ss_new}
#' @param fill_fnc *A function to fill in missing values (ages and years). The
#'   resulting weight-at-age file will have values for all years and ages.One
#'   function is \code{fill_across}.
#' @param cv_wtatage A user specified CV for growth. Default is \code{NULL}.
#' @return A modified \code{.wtatage.ss} file if \code{!is.null(outfile)}. A list
#'   object containing the modified \code{.wtatage.ss} file is returned invisibly.
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @template casefile-footnote
#' @importFrom r4ss SS_parlines
#' @seealso \code{\link{fill_across}}
#' @family sampling functions
#' @export
#
# # #For Debugging
# setwd('/Users/peterkuriyama/School/Research/capam_growth/sample_wtatage_test/')
# # source('fill_across.r')
# #
# wta_file_in <- "om/wtatage.ss_new"
# outfile <- "em/wtatage.ss"
# datfile <- "em/ss3.dat"
# ctl_file_in <- "om/control.ss_new"
# years <- list(seq(2, 100, 1), seq(2, 100, 1))
# fill_fnc <- fill_across
# fleets <- list(1, 2)
# cv_wtatage <- .5
#
# dat_list <- r4ss::SS_readdat(file=datfile, version = NULL, verbose=FALSE)
# test <- sample_wtatage(wta_file_in = wta_file_in, outfile = outfile, dat_list = dat_list,
#     ctl_file_in = ctl_file_in, years = years, fill_fnc = fill_across,
#     fleets = fleets, cv_wtatage = cv_wtatage)
#

sample_wtatage <- function(wta_file_in, outfile, dat_list, ctl_file_in,
                           years, fill_fnc = fill_across, fleets,
                           cv_wtatage = NULL){
  ##fill_type: specify type of fill, fill zeroes with first row? annual interpolation?
        ## Age Interpolation?
    ## A value of NULL for fleets signifies to turn this data off in the
    ## EM. So quit early and in ss3sim_base do NOT turn wtatage on using
    ## the maturity function
    if(is.null(cv_wtatage)) stop('specify cv_wtatage in case file')
    if(is.null(dat_list$MeanSize_at_Age_obs)) stop("The provided list of data",
      " in dat_list doesn't contain mean size at age observations.")
    if(is.null(fleets)) return(NULL)
    if(length(fleets) > 1 & length(years) == 1) {
      years[2:length(fleets)] <- years[1]
    }

    ### ACH: Because you always have to have year 100, you may want to check for duplicates
    # years <- years[!duplicated(years)]
    #----------------------------------------------------------------------------
    # compress conditionals to a marginal number of samples per year
    agecomp <- aggregate(. ~ Yr + FltSvy + Gender, FUN = sum,
      data = dat_list$agecomp[,grep("Yr|Flt|samp|Gend|^a[0-9]", 
        colnames(dat_list$agecomp))])
    agebin_vector <- dat_list$agebin_vector

    mlacomp <- dat_list$MeanSize_at_Age_obs
    ## Read in the control file
    ss_version <- get_ss_ver_file(ctl_file_in)
    ctl <-SS_parlines(ctl_file_in, version = ss_version)
    #Define growth parameters
    Wtlen1 <- ctl[grep("Wtlen_1_Fem", ctl$Label), "INIT"]
    Wtlen2 <- ctl[grep("Wtlen_2_Fem", ctl$Label), "INIT"]
    if(any(grepl("Wtlen_1_M|GP_[2-9]", ctl$Label))) {
      stop("sample_wtatage does not currently accommodate",
        " 2-sex models or multiple growth morphs.")
    }
    ## Read in the file and grab the expected values for fecundity,
    ## population, and beginning of they year population and assign NA
    ## to values for all fleets in model
    wtatage <- r4ss::SS_readwtatage(wta_file_in, verbose = FALSE)
    colnames(wtatage) <- tolower(colnames(wtatage))
    ages <- type.convert(grep("^[0-9]", colnames(wtatage), value = TRUE))
    colnames(wtatage) <- gsub("([0-9]+)", "age\\1", colnames(wtatage))
    wtatage$yr <- abs(wtatage$yr)
    wtatage <- wtatage[wtatage$fleet %in% -1:0, ]
    wtatagefleets <- make_dummy_wtatage(fleets = seq(dat_list$Nfleets),
      years = wtatage[wtatage$fleet == 0, "yr"],
      age_bins = ages)
    wtatage <- rbind(wtatage, wtatagefleets)

    age0 <- wtatage[!duplicated(wtatage$fleet),c("fleet","age0")]

    for(fl in fleets) { #fleets must be 1:Nfleets
        for(yr in years[[fl]]) {

                #Step 1, draw from true age distributions
                agecomp.temp <- agecomp[agecomp$Yr==yr & agecomp$FltSvy == fl, ]
                #ACH: I'm going with the motto of think about it. Why enter a year for wtatage when you do not have data?
                if(nrow(agecomp.temp)==0) {
                  stop("No age comp observations for year",yr,"and fleet",fl,"\n")
                }

                ## Get the true age distributions
                age.means <- agecomp.temp[, grep("^a[0-9]", colnames(agecomp.temp))]
                age.Nsamp <- as.numeric(agecomp.temp$Nsamp)

                #Step 2, determine # of fish in sample per bin
                #Use a multinomial here but may need to have dirichlet or option to toggle between the two
                age.samples <- rmultinom(n = 1, size = age.Nsamp, prob = age.means)

                #----------------------------------------------------------------------------------------------------
                #Step 3, use mean length-at-age to sample with normal/lognormal and user-specified cv
                #first define mean length-at-age
                mla.means <- as.numeric(mlacomp[
                  mlacomp$AgeErr == 1 & mlacomp$Gender == 0 &
                  mlacomp$Yr == yr & mlacomp$FltSvy == fl,
                  paste0("a", agebin_vector)])
                # todo: make cv_wtatage fleet specific
                sds <- mla.means * cv_wtatage

                #step 4, sampling from normal distribution
                lengths.list <- mapply(rnorm, n = age.samples, mean = mla.means, sd = sds)
                weights.list <- lapply(lengths.list, function(x) Wtlen1*x^Wtlen2)

                #step 5, calculate new mean weight at age
                samp.wtatage <- sapply(weights.list, mean)
                names(samp.wtatage) <- gsub("^a", "age", rownames(age.samples))

                #concatenate everything
                # age0[age0$fleet == 1, 'age0']
                wtatage[wtatage$yr == yr & wtatage$fleet == fl, names(samp.wtatage)] <- samp.wtatage
            }
        }

    # todo: something with beginning and middle of the year wtatage
    maturity.pars <- ctl[grep("^Mat", ctl$Label), "INIT"]
    maturityatage <- exp(
      maturity.pars[1]*maturity.pars[2] + 0:20 *-1* maturity.pars[2]) /
      (1+exp(maturity.pars[1]*maturity.pars[2] + 0:20 * -1*maturity.pars[2]))
    population <- fill_wtatage(aggregate(. ~ yr + seas + sex + bio_pattern + birthseas,
      data = wtatage[wtatage$fleet %in% unlist(fleets), ],
      FUN = function(x) mean(x, na.rm = TRUE), na.action = "na.pass"))
    populationpieces <- fill_header(population)
    fecundity <- cbind(
      populationpieces[["prefix"]],
      as.matrix(populationpieces[["data"]]) %*% diag(maturityatage))
    colnames(fecundity) <- populationpieces[["cols"]]
    fecundity$fleet <- -2

    wtatagefleet <- wtatage[wtatage$fleet > 0, ]
    wtatagesampled <- rbind(
      do.call("rbind", mapply(`[<-`, replicate(2, population, simplify = FALSE),
          'fleet', value = 0:-1, SIMPLIFY = FALSE)),
        fecundity,
      do.call("rbind", tapply(seq(NROW(wtatagefleet)),
      list(wtatagefleet$fleet, wtatagefleet$sex),
      simplify = FALSE,
      FUN = function(yy) fill_wtatage(wtatagefleet[yy,]))))

   colnames(wtatagesampled) <- gsub("yr", "Yr", colnames(wtatagesampled))
   colnames(wtatagesampled) <- gsub("seas", "Seas", colnames(wtatagesampled))
   colnames(wtatagesampled) <- gsub("fleet", "Fleet", colnames(wtatagesampled))

    ##write wtatage.ss file
    if(!is.null(outfile)) {
      r4ss::SS_writewtatage(wtatagesampled,
        file = basename(outfile), dir = dirname(outfile), overwrite=TRUE,
        verbose = FALSE, warn = FALSE)
    }

    return(invisible(wtatagesampled))
}
