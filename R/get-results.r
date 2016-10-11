#' Calculate run time
#'
#' Internal function used by \code{get_results_scenario} to calculate the
#' runtime (in minutes) from a \code{Report.sso} file.
#'
#' @param start_time Vector of characters as read in from the r4ss report file
#' @param end_time Vector of characters as read in from the r4ss report
#' file
#' @return A numeric value representing the number of minutes of total
#' runtime as reported by SS.
#' @details This runtime includes the overhead for reading and writing the
#' file up to the Report.sso time stamp.
#' @author Cole Monnahan
calculate_runtime <- function(start_time, end_time) {
  ## The start_time and end_time strings are complex and need to be cleaned up
  ## before processing into date objects.
  start <- data.frame(do.call(rbind, strsplit(x = as.character(start_time),
    split = " ", fixed = T))[, -(1:2)])
  end <- data.frame(do.call(rbind, strsplit(x = as.character(end_time),
    split = " ", fixed = T))[, -(1:2)])
  start <- as.data.frame(t(start))
  end <- as.data.frame(t(end))
  if(ncol(start) == 4) {
    names(start) <- names(end) <- c("month", "day", "time", "year")
  }
  if(ncol(start) == 5) {
    names(start) <- names(end) <- c("month", "", "day", "time", "year")
  }
  if(ncol(start) %in% c(4, 5)) {
    start.date <- lubridate::ymd_hms(with(start, paste(year,
      month, day, time, sep = "-")))
    end.date <- lubridate::ymd_hms(with(end, paste(year,
      month, day, time, sep = "-")))
    ## run.mins <- as.vector(end.date - start.date)
    run.mins <- as.vector(difftime(end.date, start.date, units='secs'))/60
  } else {
    run.mins <- NA
  }
  return(run.mins)
}

#' Identify ss3sim scenarios within a directory
#'
#' @param directory The directory which contains scenario folders with
#'    results.
#' @return A character vector of folders
#' @author Merrill Rudd
#' @export
id_scenarios <- function(directory){
    ## Get unique scenarios that exist in the folder. Might be other random
    ## stuff in the folder so be careful to extract only scenario folders.
    all.dirs <- list.dirs(path=directory, full.names=FALSE, recursive=FALSE)
    temp.dirs <- sapply(1:length(all.dirs), function(i) {
        x <- unlist(strsplit(all.dirs[i], split="/"))
        return(x[length(x)])
    })
    scens <- temp.dirs[grepl("^([A-Z]{1}[0-9]+-)+[a-z-]+$", temp.dirs)]
    if(length(scens)==0) warning(paste("No scenario folders found in",
             directory))
    else return(scens)
}

#' Extract SS3 simulation output
#'
#' This high level function extracts results from SS3 model runs. Give it a
#' directory which contains directories for different "scenario" runs, within
#' which are replicates and potentially bias adjustment runs. It writes two
#' data.frames to file: one for single scalar values (e.g. MSY) and a second
#' that contains output for each year of the same model (timeseries, e.g.
#' biomass(year)). These can always be joined later.
#'
#' @param directory The directory which contains scenario folders with
#'   results.
#' @param overwrite_files A switch to determine if existing files should be
#'   overwritten, useful for testing purposes or if new replicates are run.
#' @param user_scenarios A character vector of scenarios that should be read
#'   in. Default is \code{NULL}, which indicates find all scenario folders in
#'   \code{directory}.
#' @param parallel Should the function be run on multiple cores? You will
#'   need to set up parallel processing as shown in \code{\link{run_ss3sim}}.
#' @export
#' @return
#' Creates two .csv files in the current working directory:
#' \code{ss3sim_ts.csv} and \code{ss3sim_scalar.csv}.
#' @author Cole Monnahan, Merrill Rudd
#' @family get-results
get_results_all <- function(directory=getwd(), overwrite_files=FALSE,
  user_scenarios=NULL, parallel=FALSE){

    on.exit(setwd(directory))

    if(parallel) {
      cores <- setup_parallel()
      if(cores == 1) parallel <- FALSE
    }

    ## Choose whether to do all scenarios or the vector passed by user
    if(is.null(user_scenarios)) {
        scenarios <- id_scenarios(directory=directory)
    } else {
        temp_scenarios <- id_scenarios(directory=directory)
        scenarios <- user_scenarios[which(user_scenarios %in% temp_scenarios)]
        if(any(user_scenarios %in% temp_scenarios==FALSE)){
            warning(paste(user_scenarios[which(user_scenarios %in%
                temp_scenarios == FALSE)], "not in directory\n"))
        }
    }

    if(length(scenarios)==0)
        stop(paste("Error: No scenarios found in:",directory))
    message(paste("Extracting results from", length(scenarios), "scenarios"))

    if(parallel){
        parallel_scenario <- NULL
        # ts.list <- scalar.list <- list()

        # to satisfy R CMD check in the foreach() call below
        foreach <- NULL
        `%dopar%` <- NULL

        results_all <- foreach(parallel_scenario = scenarios, .verbose = FALSE,
            .export = c("pastef", "get_results_scenario",
            "get_results_scalar", "get_nll_components",
            "get_results_timeseries", "calculate_runtime"), .combine = rbind) %dopar% {
            ## If the files already exist just read them in, otherwise get results
                scalar.file <- paste0(parallel_scenario,"/results_scalar_",parallel_scenario,".csv")
                ts.file <- paste0(parallel_scenario,"/results_ts_",parallel_scenario,".csv")
                ## Delete them if this is flagged on
                if( overwrite_files){
                    if(file.exists(scalar.file)) file.remove(scalar.file)
                    if(file.exists(ts.file)) file.remove(ts.file)
                    get_results_scenario(scenario=parallel_scenario, directory=directory,
                                         overwrite_files=overwrite_files)
                }
                ## Check if still there and skip if already so, otherwise read in
                ## and save to file
                if(!file.exists(scalar.file) |  !file.exists(ts.file)){
                    get_results_scenario(scenario=parallel_scenario, directory=directory,
                                         overwrite_files=overwrite_files)
                }
        }
        ts.list <- scalar.list <- dq.list <- list()
        flag.na <- rep(0, length(scenarios))
        for(i in 1:length(scenarios)){
            scalar.file <- paste0(scenarios[i],"/results_scalar_",scenarios[i],".csv")
            ts.file <- paste0(scenarios[i],"/results_ts_",scenarios[i],".csv")
            dq.file <- paste0(scenarios[i],"/results_dq_",scenarios[i],".csv")
            scalar.list[[i]] <- tryCatch(read.csv(scalar.file, stringsAsFactors=FALSE), error=function(e) NA)
            ts.list[[i]] <- tryCatch(read.csv(ts.file, stringsAsFactors=FALSE), error=function(e) NA)
            dq.list[[i]] <- tryCatch(read.csv(dq.file, stringsAsFactors=FALSE), error=function(e) NA)
            if(all(is.na(scalar.list[[i]]))){flag.na[i] <- 1}
        }
        scalar.list.out <- scalar.list[which(flag.na!=1)]
        ts.list.out <- ts.list[which(flag.na!=1)]
        dq.list.out <- dq.list[which(flag.na!=1)]
        ## Combine all scenarios together and save into big final files
        scalar.all <- do.call(plyr::rbind.fill, scalar.list.out)
        scalar.all$ID <- paste(scalar.all$scenario, scalar.all$replicate, sep = "-")
        ts.all <- do.call(plyr::rbind.fill, ts.list.out)
        ts.all$ID <- paste(ts.all$scenario, ts.all$replicate, sep="-")
        dq.all <- do.call(plyr::rbind.fill, dq.list.out)
        dq.all$ID <- paste(dq.all$scenario, dq.all$replicate, sep="-")
        write.csv(scalar.all, file="ss3sim_scalar.csv")
        write.csv(ts.all, file="ss3sim_ts.csv")
        ## write.csv(dq.all, file="ss3sim_dq.csv")
        message(paste("Final result files written to", directory))
    } else{
    ## Loop through each scenario in folder in serial
    dq.list <- ts.list <- scalar.list <- list()
    for(i in 1:length(scenarios)){
        setwd(directory)
        scen <- scenarios[i]
        ## If the files already exist just read them in, otherwise get results
        scalar.file <- paste0(scen,"/results_scalar_",scen,".csv")
        ts.file <- paste0(scen,"/results_ts_",scen,".csv")
        dq.file <- paste0(scen,"/results_dq_",scen,".csv")
        ## Delete them if this is flagged on
        if( overwrite_files){
            if(file.exists(scalar.file)) file.remove(scalar.file)
            if(file.exists(ts.file)) file.remove(ts.file)
            if(file.exists(dq.file)) file.remove(dq.file)
            get_results_scenario(scenario=scen, directory=directory,
                                 overwrite_files=overwrite_files)
        }
        ## Check if still there and skip if already so, otherwise read in
        ## and save to file
        if(!file.exists(scalar.file) | !file.exists(ts.file) | !file.exists(dq.file)){
            get_results_scenario(scenario=scen, directory=directory,
                                 overwrite_files=overwrite_files)
        }
        scalar.list[[i]] <- tryCatch(read.csv(scalar.file, stringsAsFactors=FALSE), error=function(e) NA)
        ts.list[[i]] <- tryCatch(read.csv(ts.file, stringsAsFactors=FALSE), error=function(e) NA)
        dq.list[[i]] <- tryCatch(read.csv(dq.file, stringsAsFactors=FALSE), error=function(e) NA)
    }
    scalar.list <- scalar.list[which(!is.na(scalar.list))]
    ts.list <- ts.list[which(!is.na(ts.list))]
    dq.list <- dq.list[which(!is.na(dq.list))]
    ## Combine all scenarios together and save into big final files
    scalar.all <- do.call(plyr::rbind.fill, scalar.list)
    scalar.all$ID <- paste(scalar.all$scenario, scalar.all$replicate, sep = "-")
    ts.all <- do.call(plyr::rbind.fill, ts.list)
    ts.all$ID <- paste(ts.all$scenario, ts.all$replicate, sep="-")
    dq.all <- do.call(plyr::rbind.fill, dq.list)
    dq.all$ID <- paste(dq.all$scenario, dq.all$replicate, sep="-")
    write.csv(scalar.all, file="ss3sim_scalar.csv")
    write.csv(ts.all, file="ss3sim_ts.csv")
    ## write.csv(dq.all, file="ss3sim_dq.csv")
    message(paste("Final result files written to", directory))
  }
}

#' Extract SS3 simulation results for one scenario.
#'
#' Function that extracts results from all replicates inside a supplied
#' scenario folder. The function writes 3 .csv files to the scenario
#' folder: (1) scalar metrics with one value per replicate (e.g. \eqn{R_0},
#' \eqn{h}), (2) a timeseries data ('ts') which contains multiple values per
#' replicate (e.g.  \eqn{SSB_y} for a range of years \eqn{y}), and (3) [currently
#' disabled and not tested] residuals on the log scale from the surveys
#' across all replicates. The function \code{get_results_all} loops through
#' these .csv files and combines them together into a single "final"
#' dataframe.
#'
#' @param scenario A single character giving the scenario from which to
#'   extract results.
#' @param directory The directory which contains the scenario folder.
#' @param overwrite_files A boolean (default is \code{FALSE}) for whether to delete
#'   any files previously created with this function. This is intended to be
#'   used if replicates were added since the last time it was called, or any
#'   changes were made to this function.
#' @author Cole Monnahan
#' @importFrom r4ss SS_output
#' @family get-results
#' @export
#' @examples \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' case_folder <- paste0(d, "/eg-cases")
#' om <- paste0(d, "/models/cod-om")
#' em <- paste0(d, "/models/cod-em")
#' run_ss3sim(iterations = 1:2, scenarios =
#'   c("D0-F0-G0-S0-cod"),
#'   case_folder = case_folder, om_dir = om, em_dir = em,
#'   bias_adjust = FALSE)
#' get_results_scenario(c("D0-F0-G0-S0-cod"))
#' }
get_results_scenario <- function(scenario, directory=getwd(),
                                 overwrite_files=FALSE){
    ## This function moves the wd around so make sure to reset on exit,
    ## especially in case of an error
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    if (file.exists(pastef(directory, scenario))) {
        setwd(pastef(directory, scenario))
    } else {
        stop(paste("Scenario", scenario, "does not exist in", directory))
    }
    ## Stop if the files already exist or maybe delete them
    scalar.file <- paste0("results_scalar_",scenario,".csv")
    ts.file <- paste0("results_ts_",scenario,".csv")
    dq.file <- paste0("results_dq_",scenario,".csv")
    resids.file <- paste0("results_resids_",scenario,".csv")
    if(file.exists(scalar.file) | file.exists(ts.file) | file.exists(dq.file)){
        if(overwrite_files) {
            ## Delete them and continue
            message(paste0("Files deleted for ", scenario))
            file.remove(scalar.file, ts.file, dq.file)
        } else {
            ## Stop the progress
            stop(paste0("Files already exist for ", scenario,"
              and overwrite_files=FALSE"))
        }
    }
    ## Check for bias correction for this scenario, grab it if exists otherwise
    ## report NAs
    bias <- c("bias.converged" = NA, "bias.tried" = NA)
    if(length(grep("bias", dir()))==1){
        bias.file <- read.table(file="bias/AdjustBias.DAT", header=FALSE)
        ## The ones with NAs mean it didn't converge
        bias[1] <- NROW(na.omit(bias.file))
        bias[2] <- NROW(bias.file)
    }

    ## Loop through each replicate, not including the bias folders, and get
    ## results from both models
    ## Remove the .csv files and bias folder, they are not reps
    reps.dirs <- list.files(pattern = "[0-9]+$")
    reps.dirs <- sort(as.numeric(reps.dirs))
    if(length(reps.dirs)==0)
        stop(paste("Error:No replicates for scenario", scenario))
    ## Loop through replicates and extract results using r4ss::SS_output
    resids.list <- list()
    message(paste0("Starting ", scenario, " with ", length(reps.dirs), " iterations"))
    ## Get the number of columns for this scenario
    numcol <- read.table(file.path(reps.dirs[1], "om", "Report.sso"),
      col.names = 1:300, fill = TRUE, quote = "",
      colClasses = "character", nrows = -1, comment.char = "")
    numcol <- max(which(apply(numcol, 2, function(x) all(x == "")) == FALSE)) + 1
    for(rep in reps.dirs){
        ## Check that the model finished running and if not skip it but
        ## report that ID
        ID <- paste0(scenario, "-", rep)
        if(!file.exists(paste0(rep,"/em/Report.sso"))){
            message(paste("Missing Report.sso file for:", ID, "; skipping..."))
        } else {
            ## Otherwise read in and write to file
            report.em <-
                SS_output(paste0(rep,"/em/"), covar=FALSE, verbose=FALSE,
                          compfile="none", forecast=TRUE, warn=TRUE,
                          readwt=FALSE, printstats=FALSE, NoCompOK=TRUE,
                          ncols=numcol)
            report.om <-
                SS_output(paste0(rep,"/om/"), covar=FALSE, verbose=FALSE,
                          compfile="none", forecast=FALSE, warn=TRUE,
                          readwt=FALSE, printstats=FALSE, NoCompOK=TRUE,
                          ncols=numcol)
            ## ## Grab the residuals for the indices
            ## resids <- log(report.em$cpue$Obs) - log(report.em$cpue$Exp)
            ## resids.long <- data.frame(report.em$cpue[,c("FleetName", "Yr")], resids)
            ## resids.list[[rep]] <-
            ##     cbind(scenario, rep, reshape2::dcast(resids.long, FleetName~Yr,
            ##                                          value.var="resids"))
            ## Get scalars from the two models
            scalar.om <- get_results_scalar(report.om)
            names(scalar.om) <- paste0(names(scalar.om),"_om")
            scalar.em <- get_results_scalar(report.em)
            names(scalar.em) <- paste0(names(scalar.em),"_em")
            ## Get timeseires from the two
            timeseries.om <- get_results_timeseries(report.om)
            names(timeseries.om) <- paste0(names(timeseries.om),"_om")
            timeseries.em <- get_results_timeseries(report.em)
            names(timeseries.em) <- paste0(names(timeseries.em),"_em")
            ## Get derived quantities information
            derived.om <- get_results_derived(report.om)
            names(derived.om) <- paste0(names(derived.om), "_om")
            derived.em <- get_results_derived(report.em)
            names(derived.em) <- paste0(names(derived.em), "_em")

            ## Combine them together and massage a bit
            scalar <- cbind(scalar.om, scalar.em, t(bias))
            ts <- cbind(timeseries.om, timeseries.em)
            dq <- cbind(derived.om, derived.em)
            scalar$scenario <- ts$scenario <- dq$scenario <- scenario
            scalar$replicate <- ts$replicate <- dq$replicate <- rep

            ## parse the scenarios into columns for plotting later
            scenario.scalar <-
                data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
               as.character(scalar$scenario)), "- ")), stringsAsFactors = FALSE)
            names(scenario.scalar) <-
                c(substr(as.vector(as.character(
                    scenario.scalar[1,-ncol(scenario.scalar)])), 1,1) ,"species")
            scenario.ts <-
                data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
                                                        as.character(ts$scenario)), "- ")),
                           row.names = row.names(ts), stringsAsFactors = FALSE)
            names(scenario.ts) <-
                c(substr(as.vector(as.character(
                    scenario.ts[1,-ncol(scenario.ts)])), 1,1) ,"species")
            scenario.dq <-
                data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
                                                        as.character(dq$scenario)), "- ")),
                           row.names = row.names(dq), stringsAsFactors = FALSE)
            names(scenario.dq) <-
                c(substr(as.vector(as.character(
                    scenario.dq[1,-ncol(scenario.dq)])), 1,1) ,"species")

            scalar <- cbind(scalar, scenario.scalar)
            ts <- cbind(ts, scenario.ts)
            dq <- cbind(dq, scenario.dq)

            ## Other calcs
            ts$year <- ts$Yr_om
            ts$Yr_om <- NULL
            ts$Yr_em <- NULL
            dq$year <- dq$Yr_om
            dq$Yr_om <- NULL
            dq$Yr_em <- NULL
            scalar$max_grad <- scalar$max_grad_em
            ignore.cols <- which(names(scalar) %in%
                           c("max_grad_om", "params_on_bound_om",
                             "max_grad_em","params_stuck_low_om",
                             "params_stuck_high_om" ))
            scalar <- scalar[ , -ignore.cols]

            ## Also get some meta data and other convergence info like the
            ## version, runtime, etc. as checks
            temp <- readLines(con=paste0(rep,"/em/Report.sso"), n=10)
            scalar$version <- temp[1]
            scalar$RunTime <- calculate_runtime(temp[4],temp[5])
            scalar$hessian <- file.exists(paste0(rep,"/em/admodel.cov"))
            ## The number of iterations for the run is only in this file for
            ## some reason.
            if(!file.exists(paste0(rep,"/em/CumReport.sso"))) {
                Niterations <- NA
            } else {
                cumrep <- readLines(paste0(rep,"/em/CumReport.sso"), n=5)
                tmp <- grep("N_iter", cumrep)
                if(length(tmp)==0){
                    scalar$Niterations <- NA
                } else {
                    scalar$Niterations <-
                        as.numeric(strsplit(cumrep[tmp[1]],split=" ")[[1]][3])
                }
            }

            ## Write them to file in the scenario folder
            scalar.exists <- file.exists(scalar.file)
            write.table(x=scalar, file=scalar.file, append=scalar.exists,
                        col.names=!scalar.exists, row.names=FALSE, sep=",")
            ts.exists <- file.exists(ts.file)
            write.table(x=ts, file=ts.file, append=ts.exists,
                        col.names=!ts.exists, row.names=FALSE, sep=",")
            dq.exists <- file.exists(dq.file)
            write.table(x=dq, file=dq.file, append=dq.exists,
                        col.names=!dq.exists, row.names=FALSE, sep=",")
        }
    }
    ## ## Create df for the residuals
    ## resids <- do.call(rbind, resids.list)
    ## write.table(x=resids, file=resids.file, sep=",", row.names=FALSE)
    ## End of loops for extracting results
}

#' Extract time series from a model run.
#'
#' Extract time series from an \code{\link[r4ss]{SS_output}} list from a model run.
#' Returns a data.frame of the results for SSB, recruitment and effort by year.
#'
#' @template report.file
#' @export
#' @family get-results
#' @author Cole Monnahan
get_results_timeseries <- function(report.file){
    years <- report.file$startyr:(report.file$endyr +
                                  ifelse(is.na(report.file$nforecastyears) ==
                                      TRUE, 0,
                                         report.file$nforecastyears))
    xx <- subset(report.file$timeseries,
                 select=c("Yr","SpawnBio", "Recruit_0", "F:_1"))
    xx <- xx[xx$Yr %in% years,]
    names(xx) <- gsub(":_1","", names(xx))
    # Get SPR from derived_quants
    spr <- with(report.file$derived_quants,
      report.file$derived_quants[grep("SPRratio_", LABEL), ])
    spr$Yr <- sapply(strsplit(spr$LABEL, "_"), "[", 2)
    colnames(spr)[which(colnames(spr) == "Value")] <- "SPRratio"
    # Get recruitment deviations
    dev <- report.file$recruit
    dev <- dev[dev$year %in% years, c("year", "dev")]
    ## create final data.frame
    df <- merge(xx, spr[, c("SPRratio", "Yr")], by = "Yr", all.x = TRUE)
    df$SPRratio[is.na(df$SPRratio)] <- 0
    df <- merge(df, dev, by.x = "Yr", by.y = "year", all.x = TRUE)
    rownames(df) <- NULL
    return(invisible(df))
}

#' Extract time series from a model run with the associated standard deviation.
#'
#' Extract time series from an \code{\link[r4ss]{SS_output}} list from a model run.
#' Returns a data.frame of the results for SSB, recruitment,
#' forecasts, and effort by year.
#'
#' @template report.file
#' @export
#' @family get-results
#' @author Kelli Johnson
get_results_derived <- function(report.file){
    xx <- report.file$derived_quants
    xx <- xx[, c("LABEL", "Value", "StdDev")]
    xx$Yr <- sapply(strsplit(xx$LABEL, "_"), "[", 2)
    xx$name <- sapply(strsplit(xx$LABEL, "_"), "[", 1)
    if (all(xx$StdDev == 0)) xx <- xx[, -which(colnames(xx) == "StdDev")]
    final <- reshape(xx, timevar = "name", idvar = "Yr", direction = "wide",
      drop = "LABEL")
    final <- final[-which(final$Yr == "Virgin"), ]
    rownames(final) <- NULL
    invisible(final)
}

#' Extract scalar quantities from a model run.
#'
#' Extract scalar quantities from an \code{\link[r4ss]{SS_output}} list from a model run.
#' Returns a data.frame of the results (a single row) which can be rbinded later.
#' @template report.file
#' @family get-results
#' @export
#' @author Cole Monnahan; Merrill Rudd
get_results_scalar <- function(report.file){
    der <- report.file$derived_quants
    SSB_MSY <-  der[which(der$LABEL=="SSB_MSY"),]$Value
    TotYield_MSY <-  der[which(der$LABEL=="TotYield_MSY"),]$Value
    SSB_Unfished <-  der[which(der$LABEL=="SSB_Unfished"),]$Value
    F_MSY <- der[der$LABEL == "Fstd_MSY", "Value"]
    F_SPR <- der[der$LABEL == "Fstd_SPRtgt", "Value"]
    Catch_endyear <-
        rev(report.file$timeseries[,grep("dead\\(B\\)",
          names(report.file$timeseries))])[1]
    pars <- data.frame(t(report.file$parameters$Value))
    names(pars) <- report.file$parameters$Label
    ## Get the parameters stuck on bounds
    status <- report.file$parameters$Status
    params_stuck_low <- paste(names(pars)[which(status=="LO")], collapse=";")
    params_stuck_high <- paste(names(pars)[which(status=="HI")], collapse=";")
    if(params_stuck_low=="") params_stuck_low <- NA
    if(params_stuck_high=="") params_stuck_high <- NA
    ## Remove the recruitment devs and efforts as these are in the ts file
    recdev.index <- grep("MAIN_", toupper(names(pars)), fixed=TRUE)
    if(length(recdev.index)>0) pars <- pars[,-recdev.index]
    effort.index <- grep("F_FLEET_", toupper(names(pars)), fixed=TRUE)
    if(length(effort.index)>0) pars <- pars[,-effort.index]
    names(pars) <- gsub("\\(","_", names(pars))
    names(pars) <- gsub("\\)","", names(pars))
    max_grad <- report.file$maximum_gradient_component
    depletion <- report.file$current_depletion
    NLL_vec <- get_nll_components(report.file)
    ## Obtain bias adjustment parameters
    bias <- report.file$breakpoints_for_bias_adjustment_ramp
    ## get the number of params on bounds from the warning.sso file, useful for
    ## checking convergence issues
    warn <- report.file$warnings
    warn.line <- grep("Number_of_active_parameters", warn, fixed=TRUE)
    params_on_bound <-
        ifelse(length(warn.line)==1,
          as.numeric(strsplit(warn[warn.line], split=":")[[1]][2]), NA)
    ## Combine into final df and return it
    df <- data.frame(SSB_MSY, TotYield_MSY, SSB_Unfished, max_grad, depletion
                , F_MSY, F_SPR, bias,
                params_on_bound, params_stuck_low, params_stuck_high, pars,
                Catch_endyear, t(NLL_vec), stringsAsFactors=FALSE)
    return(invisible(df))
}

#' Get negative log likelihood (NLL) values from a report file list
#'
#' @template report.file
#' @author Merrill Rudd
get_nll_components <- function(report.file){
    ## Possible likelihood components from SS3.tpl
    NLL_components <- c("TOTAL", "Catch", "Equil_catch", "Survey", "Discard",
      "Mean_body_wt", "Length_comp", "Age_comp", "Size_at_age", "SizeFreq",
      "Morphcomp", "Tag_comp", "Tag_negbin", "Recruitment",
      "Forecast_Recruitment", "Parm_priors", "Parm_softbounds", "Parm_devs",
      "Crash_Pen")
    NLL_names <- paste("NLL", NLL_components, sep="_")

    like_mat <- report.file$likelihoods_used
    vec <- sapply(NLL_components, function(x)
      ifelse(length(like_mat[which(rownames(like_mat)==x), 1])==0,
                NA, like_mat[which(rownames(like_mat)==x), 1]))
    names(vec) <- NLL_names

    return(vec)
}
