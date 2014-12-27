
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

clean_data <- function(datfile, index_params=NULL, lcomp_params=NULL,
                       agecomp_params=NULL, calcomp_params=NULL,
                       mlacomp_params=NULL, verbose=FALSE ){
    ## Should somehow have a check that datfile is valid. None for now.

    ## Keep track of how many rows were removed to see if needed
    index.N.removed <- agecomp.N.removed <- lcomp.N.removed <-
        mlacomp.N.removed <- calcomp.N.removed <- 0
    ## CPUE
    ## Length composition data
    if(is.null(index_params)){
        stop("Indices are currently mandatory: index_params is NULL")
    } else {
        a <- datfile$CPUE
        datfile$CPUE <- do.call(rbind,
         lapply(1:length(index_params$fleets), function(i)
                a[a$index == index_params$fleets[i] &
                  a$year %in% index_params$years[[i]],]))
        datfile$N_cpue <- nrow(datfile$CPUE)
        index.N.removed <- nrow(a)-nrow(datfile$CPUE)
        if(index.N.removed>0 & verbose)
            message(paste(index.N.removed,
                      "lines of CPUE data were removed"))
    }

    ## Length composition data
    if(is.null(lcomp_params)){
        datfile$lencomp <- data.frame("#")
        datfile$N_lencomp <- 0
    } else {
        a <- datfile$lencomp
        datfile$lencomp <- do.call(rbind,
         lapply(1:length(lcomp_params$fleets), function(i)
                a[a$Flt == lcomp_params$fleets[i] &
                  a$Yr %in% lcomp_params$years[[i]],]))
        datfile$N_lencomp <- nrow(datfile$lencomp)
        lcomp.N.removed <- nrow(a)-nrow(datfile$lencomp)
        if(lcomp.N.removed>0 & verbose)
            message(paste(lcomp.N.removed,
                      "lines of length comp data were removed"))
    }

    ## Mean length at age data
    if(is.null(mlacomp_params$fleets)){
        datfile$MeanSize_at_Age_obs <- data.frame("#")
        datfile$N_MeanSize_at_Age_obs <- 0
    } else {
        a <- datfile$MeanSize_at_Age_obs
        datfile$MeanSize_at_Age_obs <-
            do.call(rbind,
         lapply(1:length(mlacomp_params$fleets), function(i)
                a[a$Flt == mlacomp_params$fleets[i] &
                  a$Yr %in% mlacomp_params$years[[i]],]))
        datfile$N_MeanSize_at_Age_obs <- nrow(datfile$MeanSize_at_Age_obs)
        mlacomp.N.removed <-
            if(is.null(a))  0 else  nrow(a)-datfile$N_MeanSize_at_Age_obs
        if(mlacomp.N.removed!=0 & verbose)
            message(paste(mlacomp.N.removed,
                      "lines of mean length data were removed"))
    }

    ## Age comps and conditional age-at-legnth at the same time
    if(is.null(agecomp_params) & is.null(calcomp_params)){
        datfile$agecomp <- data.frame("#")
        datfile$N_agecomp <- 0
    } else {
        a <- datfile$agecomp
        ## Clean the agecomp and calcomp data
        agecomp <- a[a$Lbin_lo < 0,]
        calcomp <- a[a$Lbin_lo >= 0, ]
        new.agecomp <- do.call(rbind,
         lapply(1:length(agecomp_params$fleets), function(i)
             agecomp[agecomp$Flt == agecomp_params$fleets[i] &
             agecomp$Yr %in% agecomp_params$years[[i]],]))
        new.calcomp <- do.call(rbind,
         lapply(1:length(calcomp_params$fleets), function(i)
             calcomp[calcomp$Flt == calcomp_params$fleets[i] &
             calcomp$Yr %in% calcomp_params$years[[i]],]))
        datfile$agecomp <- rbind(new.agecomp, new.calcomp)
        datfile$N_agecomp <- nrow(datfile$agecomp)
        agecomp.N.removed <- nrow(a)-nrow(datfile$agecomp)
        if(agecomp.N.removed>0 & verbose)
            message(paste(agecomp.N.removed,
                      "lines of age data were removed"))
    }
    return(invisible(datfile))
}
