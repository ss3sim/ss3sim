
## Some prelim tests for development, use these to create unit tests????

## scen <- expand_scenarios(cases=list(D=80, E=0, F=0), species="fla")
## case_files <- list(F = "F",  E="E",  D =
##     c("index", "lcomp", "agecomp"))
## a <- get_caseargs(folder = 'data test cases', scenario = scen[1],
##                   case_files = case_files)
## lcomp_params= a$lcomp
## agecomp_params= a$agecomp
## calcomp_params= a$calcomp
## mlacomp_params= a$mlacomp


## lcomp_params= list(Nsamp=list(12345), fleets=1, years=list(c(1,5)))
## agecomp_params= list(Nsamp=list(12345), fleets=c(1,2), years=list(2,c(15,16)))
## calcomp_params= list(Nsamp=list(1), fleets=c(1), years=98)
## mlacomp_params= NULL
## d <- system.file("extdata", package = "ss3sim")
## f_in <- paste0(d, "/example-om/data.ss_new")
## datfile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
## data_units <- calculate_data_units(lcomp_params=lcomp_params,
##                      agecomp_params=agecomp_params,
##                      calcomp_params=calcomp_params,
##                      mlacomp_params=mlacomp_params)
## dat2 <- with(data_units, change_data(datfile=datfile, fleets=fleets, years=years,
##                              types=types, write_file=FALSE))
## dat2 <- change_data(datfile, fleets=c(1,2), years=c(4,5),
##                     types=c("age","len", "mla", "cal"), write_file=FALSE)
## datfile <- dat2
## dat3 <- clean_datfile(datfile=dat2, lcomp_params=lcomp_params,
##                      agecomp_params=agecomp_params,
##                      calcomp_params=calcomp_params,
##                      mlacomp_params=mlacomp_params,
##                       verbose=TRUE)

#' Given sampling arguments, removed ("clean") all data in a .dat file that
#' is not specified in the arguments.
#'
#' This prepares a .dat file to be used by an EM, whereas before it may
#' have had leftover data from sampling purposes. See examples in
#' \code{\link{change_data}}.
#'
#' @author Cole Monnahan
#' @param index_params, lcomp_params, agecomp_params, calcomp_params,
#' mlacomp_params Named lists containing the arguments for the different
#' sampling functions.
#' @param verbose When \code{TRUE} it will print a message when rows are
#' deleted.
#' @seealso calculate_data_units, change_data
#' @return An invisible cleaned data list as an object.
#' @note This function does not write the result to file.
#' @export
clean_data <- function(datfile, index_params=NULL, lcomp_params=NULL,
                       agecomp_params=NULL, calcomp_params=NULL,
                       mlacomp_params=NULL, verbose=FALSE ){
    ## Should somehow have a check that datfile is valid. None for now.
    ## Note that verbose=TRUE will print how many rows are removed. The
    ## sampling functions should themselves remove data for most cases, but
    ## but not for all cases, such as when extra types are generated for
    ## sampling purposes.

    ## CPUE
    a <- datfile$CPUE
    if(is.null(index_params)){
        stop("Indices are currently mandatory: index_params is NULL")
    } else {
        datfile$CPUE <- do.call(rbind,
         lapply(1:length(index_params$fleets), function(i)
                a[a$index == index_params$fleets[i] &
                  a$year %in% index_params$years[[i]],]))
        datfile$N_cpue <- NROW(datfile$CPUE)
    }
    index.N.removed <- NROW(a)-NROW(datfile$CPUE)
    if(index.N.removed !=0  & verbose)
        message(paste(index.N.removed, "lines of CPUE data removed"))

    ## Length composition data
    a <- datfile$lencomp
    if(is.null(lcomp_params)){
        datfile$lencomp <- NULL
        datfile$N_lencomp <- 0
    } else {
        datfile$lencomp <- do.call(rbind,
         lapply(1:length(lcomp_params$fleets), function(i)
                a[a$Flt == lcomp_params$fleets[i] &
                  a$Yr %in% lcomp_params$years[[i]],]))
        datfile$N_lencomp <- NROW(datfile$lencomp)
    }
    lcomp.N.removed <- NROW(a)-NROW(datfile$lencomp)
    if(lcomp.N.removed !=0  & verbose)
        message(paste(lcomp.N.removed, "lines of length comp data removed"))

    ## Mean length at age data
    a <- datfile$MeanSize_at_Age_obs
    if(is.null(mlacomp_params$fleets)){
        datfile$MeanSize_at_Age_obs <- NULL
        datfile$N_MeanSize_at_Age_obs <- 0
    } else {
        datfile$MeanSize_at_Age_obs <-
            do.call(rbind,
         lapply(1:length(mlacomp_params$fleets), function(i)
                a[a$Flt == mlacomp_params$fleets[i] &
                  a$Yr %in% mlacomp_params$years[[i]],]))
        datfile$N_MeanSize_at_Age_obs <- NROW(datfile$MeanSize_at_Age_obs)
    }
    mlacomp.N.removed <- NROW(a) - NROW(datfile$MeanSize_at_Age_obs)
    if(mlacomp.N.removed !=0 & verbose)
        message(paste(mlacomp.N.removed, "lines of mean length data removed"))

    ## Age comps and conditional age-at-legnth at the same time
    a <- datfile$agecomp
    agecomp <- a[a$Lbin_lo < 0,]
    calcomp <- a[a$Lbin_lo >= 0, ]
    if(is.null(agecomp_params) & is.null(calcomp_params)){
        datfile$agecomp <- NULL
        datfile$N_agecomp <- 0
    } else {
        ## Clean the agecomp and calcomp data
        new.agecomp <- do.call(rbind,
         lapply(1:length(agecomp_params$fleets), function(i)
             agecomp[agecomp$Flt == agecomp_params$fleets[i] &
             agecomp$Yr %in% agecomp_params$years[[i]],]))
        new.calcomp <- do.call(rbind,
         lapply(1:length(calcomp_params$fleets), function(i)
             calcomp[calcomp$Flt == calcomp_params$fleets[i] &
             calcomp$Yr %in% calcomp_params$years[[i]],]))
        datfile$agecomp <- rbind(new.agecomp, new.calcomp)
        datfile$N_agecomp <- NROW(datfile$agecomp)
        calcomp.N.removed <- NROW(calcomp)-NROW(new.calcomp)
    }
    agecomp.N.removed <-
        NROW(agecomp)-NROW(datfile$agecomp[datfile$agecomp$Lbin_lo < 0,])
    calcomp.N.removed <-
        NROW(calcomp)-NROW(datfile$calcomp[datfile$agecomp$Lbin_lo >= 0,])
    if(agecomp.N.removed !=0  & verbose)
            message(paste(agecomp.N.removed, "lines of age data removed"))
    if(calcomp.N.removed !=0  & verbose)
            message(paste(calcomp.N.removed, "lines of CAL data removed"))

    return(invisible(datfile))
}
