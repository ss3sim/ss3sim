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
## dat_list <- r4ss::SS_readdat(f_in, section = 2, version = NULL,
##               verbose = FALSE)
## data_units <- calculate_data_units(lcomp_params=lcomp_params,
##                      agecomp_params=agecomp_params,
##                      calcomp_params=calcomp_params,
##                      mlacomp_params=mlacomp_params)
## dat2 <- with(data_units, change_data(dat_list=dat_list, fleets=fleets, years=years,
##                              types=types))
## dat2 <- change_data(dat_list, fleets=c(1,2), years=c(4,5),
##                     types=c("age","len", "mla", "cal"))
## dat_list <- dat2
## dat3 <- clean_datfile(dat_list=dat2, lcomp_params=lcomp_params,
##                      agecomp_params=agecomp_params,
##                      calcomp_params=calcomp_params,
##                      mlacomp_params=mlacomp_params,
##                       verbose=TRUE)
#' Given sampling arguments remove ("clean") all data in a .dat file that
#' is not specified
#'
#' This prepares a \code{.dat} file to be used by an EM, whereas before it may
#' have had leftover data from sampling purposes. See examples in
#' \code{\link{change_data}}.
#'
#' @author Cole Monnahan
#' @param index_params Named lists containing the arguments for
#'   \code{sample_index}.
#' @param lcomp_params Named lists containing the arguments for
#'   \code{\link{sample_lcomp}}.
#' @param agecomp_params Named lists containing the arguments for
#'   \code{\link{sample_agecomp}}.
#' @param calcomp_params Named lists containing the arguments for
#'   \code{\link{sample_calcomp}}.
#' @param mlacomp_params Named lists containing the arguments for
#'   \code{\link{sample_mlacomp}}.
#' @param verbose When \code{TRUE} it will print a message when rows are
#' deleted.
#' @template dat_list
#' @seealso calculate_data_units, change_data
#' @family sampling functions
#' @return An invisible cleaned data list as an object.
#' @note This function does not write the result to file.
#' @import dplyr
#' @importFrom tidyr complete nesting
clean_data <- function(dat_list, index_params=NULL, lcomp_params=NULL,
                       agecomp_params=NULL, calcomp_params=NULL,
                       mlacomp_params=NULL, verbose=FALSE ){
    ## sampling functions should themselves remove data for most cases, but
    ## but not for all cases, such as when extra types are generated for
    ## sampling purposes.

    if(is.null(dat_list$type)) {
      stop("dat_list must be an r4ss data file read into R using ",
           "r4ss::SSreaddat()")
    }

    if(dat_list$type != "Stock_Synthesis_data_file") {
      stop("dat_list must be an r4ss data file read into R using ",
           "r4ss::SSreaddat()")
    }
    # checks for years and fleets in params. check structure and range so that
    # function does not fail later with uninformative message or pass when it
    # shouldn't.
    all_params <- list(index_params = index_params,
                       lcomp_params = lcomp_params,
                       agecomp_params = agecomp_params,
                       calcomp_params = calcomp_params,
                       mlacomp_parms = mlacomp_params)
    check_data_str_range(all_params, dat_list)
    # check that index_params specified
    if(is.null(index_params$fleets)) {
      stop("Indices are currently mandatory: index_params is NULL")
    }
    #this stop message can be removed once conditional age at length implemented
    if(!is.null(calcomp_params)){
      stop("Conditional age at length (CAL) is not yet implemented, please only ",
           "use models and scenarios without CAL.")
    }
    ## CPUE
    a <- dat_list$CPUE
    dat_list$CPUE <- do.call(rbind,
     lapply(1:length(index_params$fleets), function(i)
            a[a$index == index_params$fleets[i] &
              a$year %in% index_params$years[[i]],]))
    dat_list$N_cpue <- NROW(dat_list$CPUE)
    dat_list$NCPUEObs <- dat_list$CPUE %>%
                          group_by(index) %>%
                          summarize(count = n()) %>%
                          complete(nesting(index = dat_list$CPUEinfo$Fleet),
                                           fill = list(count = 0)) %>%
                          arrange(index)
    dat_list$NCPUEObs <- dat_list$NCPUEObs$count

    index.N.removed <- NROW(a)-NROW(dat_list$CPUE)
    if(index.N.removed !=0  & verbose)
        message(index.N.removed, " lines of CPUE data removed")

    ## Length composition data
    a <- dat_list$lencomp
    if(is.null(lcomp_params$fleets)){
        dat_list$lencomp <- NULL
        dat_list$N_lencomp <- 0
    } else {
        dat_list$lencomp <- do.call(rbind,
         lapply(1:length(lcomp_params$fleets), function(i)
                a[a$FltSvy == lcomp_params$fleets[i] &
                  a$Yr %in% lcomp_params$years[[i]],]))
        dat_list$N_lencomp <- NROW(dat_list$lencomp)
    }
    lcomp.N.removed <- NROW(a)-NROW(dat_list$lencomp)
    if(lcomp.N.removed !=0  & verbose)
        message(lcomp.N.removed, " lines of length comp data removed")

    ## Mean length at age data
    ## Check to see if mean_outfile specifies that mlacomps should be deleted
    if (any(grepl("remove", mlacomp_params$mean_outfile))) {
      mlacomp_params$years <- NULL
    }
    a <- dat_list$MeanSize_at_Age_obs
    if(!is.null(a)) {
      if(a[1,1] == "#") a <- NULL
    }
    if(is.null(mlacomp_params$fleets)){
        dat_list$MeanSize_at_Age_obs <- NULL
        dat_list$N_MeanSize_at_Age_obs <- 0
    } else {
        dat_list$MeanSize_at_Age_obs <-
            do.call(rbind,
         lapply(1:length(mlacomp_params$fleets), function(i)
                a[a$FltSvy == mlacomp_params$fleets[i] &
                  a$Yr %in% mlacomp_params$years[[i]],]))
        dat_list$N_MeanSize_at_Age_obs <- NROW(dat_list$MeanSize_at_Age_obs)
    }
    mlacomp.N.removed <- NROW(a) - NROW(dat_list$MeanSize_at_Age_obs)
    if(mlacomp.N.removed !=0 & verbose)
        message(mlacomp.N.removed, " lines of mean length data removed")

    ## Age comps and conditional age-at-length at the same time
    a <- dat_list$agecomp
    agecomp <- a[a$Lbin_lo < 0,]
    calcomp <- a[a$Lbin_lo >= 0, ]
    ## case with no age or cal data
    if(is.null(agecomp_params$fleets) & is.null(calcomp_params$fleets)){
        new.agecomp <- new.calcomp <- NULL
    } else if(!is.null(agecomp_params$fleets) & is.null(calcomp_params$fleets))
          ## Case with just age comps and no calcomps
      {
          new.agecomp <- do.call(rbind,
         lapply(1:length(agecomp_params$fleets), function(i)
             agecomp[agecomp$FltSvy == agecomp_params$fleets[i] &
                         agecomp$Yr %in% agecomp_params$years[[i]],]))
          new.calcomp <- NULL
      } else if(!is.null(agecomp_params$fleets) & !is.null(calcomp_params$fleets)){
          ## Case with both types
          new.agecomp <- do.call(rbind,
         lapply(1:length(agecomp_params$fleets), function(i)
             agecomp[agecomp$FltSvy == agecomp_params$fleets[i] &
                         agecomp$Yr %in% agecomp_params$years[[i]],]))
          new.calcomp <- do.call(rbind,
         lapply(1:length(calcomp_params$fleets), function(i)
             calcomp[calcomp$FltSvy == calcomp_params$fleets[i] &
                         calcomp$Yr %in% calcomp_params$years[[i]],]))
      } else if(is.null(agecomp_params$fleets) & !is.null(calcomp_params$fleets)){
          ## case with only cal comps
          new.agecomp <- NULL
          new.calcomp <- do.call(rbind,
         lapply(1:length(calcomp_params$fleets), function(i)
             calcomp[calcomp$FltSvy == calcomp_params$fleets[i] &
                         calcomp$Yr %in% calcomp_params$years[[i]],]))
      }
    ## Create clean dat file
    dat_list$agecomp <- rbind(new.agecomp, new.calcomp)
    dat_list$N_agecomp <- NROW(dat_list$agecomp)
    agecomp.N.removed <-
        NROW(agecomp)-NROW(dat_list$agecomp[dat_list$agecomp$Lbin_lo < 0,])
    calcomp.N.removed <-
        NROW(calcomp)-NROW(dat_list$calcomp[dat_list$agecomp$Lbin_lo >= 0,])
    if(agecomp.N.removed !=0  & verbose)
            message(agecomp.N.removed, " lines of age data removed")
    if(calcomp.N.removed !=0  & verbose)
            message(calcomp.N.removed, " lines of CAL data removed")
    # Set data type to NULL in dat_list because if no rows exist
    # "[1]" # will be written in dat_list
    data.names <- c("lencomp", "agecomp", "MeanSize_at_Age_obs")
    for(dname in data.names) {
      if (NROW(dat_list[[dname]]) == 0) dat_list[dname] <- NULL
    }

    return(invisible(dat_list))
}

#' Check input arguments for data
#'
#' Check that the param list inputs have correct structure and range given an
#' associated data file.
#'
#' @param all_params A named list of the parameters containing at a minimum
#'   year and fleet values
#' @param dat_list An SS data list object as read in by \code{\link[r4ss]{SS_readdat}}.
#'
check_data_str_range <- function(all_params, dat_list) {
  str_err <- lapply(all_params, FUN = function(params){
    if(is.null(params)|is.null(params$fleets)|is.null(unlist(params$years))){
      error <- FALSE
      return(error)
    }
    is_vector <- is.atomic(params$fleets)
    is_list <- is.list(params$years)
    test_length <- length(params$fleets) == length(params$years)
    if(is_vector == FALSE | is_list == FALSE | test_length == FALSE) {
      error <- TRUE
    } else {
      error <- FALSE
    }
  })
  if(any(unlist(str_err) == TRUE)) {
    str_err_names <- names(str_err)[which(unlist(str_err == TRUE))]
    stop("The structure of ",
         paste0(str_err_names, collapse = ", "), " is not valid.")
  }
  range_err <-  lapply(all_params, FUN = function(params, dat_list) {
    if(is.null(params)) {
      error <- FALSE
    } else if (is.null(params$fleets)|is.null(unlist(params$years))) {
      error <- FALSE
    } else if(any(!params$fleets %in% 1:dat_list$Nfleets)) {
      error <- TRUE
    } else if(any(unlist(params$years) < dat_list$styr)|
              any(unlist(params$years) > dat_list$endyr)) {
      error <- TRUE
    } else {
      error <- FALSE
    }
  }, dat_list = dat_list)
  if(any(unlist(range_err) == TRUE)) {
    range_err_names <- names(range_err)[which(unlist(range_err == TRUE))]
    stop("Fleets or years specified in ",
         paste0(range_err_names, collapse = ", "), " are not valid values in the ",
         "datafile")
  }
  invisible(all_params)
}
