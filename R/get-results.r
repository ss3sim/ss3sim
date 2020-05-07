#' Identify ss3sim scenarios within a directory
#'
#' @param directory The directory which contains scenario folders with
#'    results.
#' @return A character vector of folders
#' @author Merrill Rudd
#' @export
id_scenarios <- function(directory) {
    ## Get unique scenarios that exist in the folder. Might be other random
    ## stuff in the folder so be careful to extract only scenario folders.
    all.dirs <- list.dirs(path = directory, full.names = FALSE, recursive = FALSE)
    temp.dirs <- sapply(seq_along(all.dirs), function(i) {
        x <- unlist(strsplit(all.dirs[i], split = "/"))
        return(x[length(x)])
    })
    scens <- temp.dirs[grepl("^([A-Z]{1}[0-9]+-)+[a-z-]+$", temp.dirs)]
    if (length(scens) == 0) warning(paste("No scenario folders found in",
             directory))
    else return(scens)
}

#' Extract SS3 simulation output
#'
#' This high level function extracts results from SS3 model runs. Give it a
#' directory which contains directories for different "scenario" runs, within
#' which are iterations. It writes two data.frames to file:
#' one for single scalar values (e.g., MSY) and a second
#' that contains output for each year of the same model (timeseries, e.g.,
#' biomass(year)). These can always be joined later.
#'
#' @param directory The directory which contains scenario folders with
#'   results.
#' @param overwrite_files A switch to determine if existing files should be
#'   overwritten, useful for testing purposes or if new iterations are run.
#' @param user_scenarios A character vector of scenarios that should be read
#'   in. Default is \code{NULL}, which indicates find all scenario folders in
#'   \code{directory}.
#' @export
#' @return Returns a list of 3 dataframes: scalar, ts, and dq.
#' Creates two .csv files in the current working directory:
#' \code{ss3sim_ts.csv} and \code{ss3sim_scalar.csv}.
#' @author Cole Monnahan, Merrill Rudd
#' @family get-results
get_results_all <- function(directory = getwd(), overwrite_files = FALSE,
  user_scenarios = NULL) {

    old_wd <- getwd()
    on.exit(setwd(old_wd))

    ## Choose whether to do all scenarios or the vector passed by user
    if (is.null(user_scenarios)) {
        scenarios <- id_scenarios(directory = directory)
    } else {
        temp_scenarios <- dir(path = directory, include.dirs = TRUE)
        scenarios <- user_scenarios[which(user_scenarios %in% temp_scenarios)]
        if (any(user_scenarios %in% temp_scenarios == FALSE)) {
            warning(paste(user_scenarios[which(user_scenarios %in%
                temp_scenarios == FALSE)], "not in directory\n"))
        }
    }

    if (length(scenarios) == 0)
        stop(paste("Error: No scenarios found in:", directory))
    message(paste("Extracting results from", length(scenarios), "scenarios"))

    ## Loop through each scenario in folder in serial
  dq.list <- ts.list <- scalar.list <-
    vector(mode = "list", length = length(scenarios))
  setwd(directory)
  for (i in seq_along(scenarios)) {
      scen <- scenarios[i]
      ## If the files already exist just read them in, otherwise get results
      scalar.file <- file.path(scen, paste0("results_scalar_", scen, ".csv"))
      ts.file <- file.path(scen, paste0("results_ts_", scen, ".csv"))
      dq.file <- file.path(scen, paste0("results_dq_", scen, ".csv"))
      ## Delete them if this is flagged on
      if (overwrite_files) {
          if (file.exists(scalar.file)) file.remove(scalar.file)
          if (file.exists(ts.file)) file.remove(ts.file)
          if (file.exists(dq.file)) file.remove(dq.file)
          get_results_scenario(scenario = scen, directory = directory,
                               overwrite_files = overwrite_files)
      }
      ## Check if still there and skip if already so, otherwise read in
      ## and save to file
      if (!file.exists(scalar.file) | !file.exists(ts.file) | !file.exists(dq.file)) {
          get_results_scenario(scenario = scen, directory = directory,
                               overwrite_files = overwrite_files)
      }
      scalar.list[[i]] <- tryCatch(suppressWarnings(read.csv(scalar.file, stringsAsFactors = FALSE)), error = function(e) NA)
      ts.list[[i]] <- tryCatch(suppressWarnings(read.csv(ts.file, stringsAsFactors = FALSE)), error = function(e) NA)
      dq.list[[i]] <- tryCatch(suppressWarnings(read.csv(dq.file, stringsAsFactors = FALSE)), error = function(e) NA)
  }
  scalar.list <- scalar.list[which(!is.na(scalar.list))]
  ts.list <- ts.list[which(!is.na(ts.list))]
  dq.list <- dq.list[which(!is.na(dq.list))]
  ## Combine all scenarios together and save into big final files
  scalar.all <- add_colnames(scalar.list, bind = TRUE)
  ts.all <- add_colnames(ts.list, bind = TRUE)
  dq.all <- add_colnames(dq.list, bind = TRUE)
  if (file.exists("ss3sim_scalar.csv")) {
    if (overwrite_files) write.csv(scalar.all, file = "ss3sim_scalar.csv")
    else {
      warning("ss3sim_scalar.csv already exists and overwrite_files = FALSE, ",
                 "so a new file was not written")
    }
  } else { # can write either way
    write.csv(scalar.all, file = "ss3sim_scalar.csv")
  }
  if (file.exists("ss3sim_ts.csv")) {
    if (overwrite_files) write.csv(ts.all, file = "ss3sim_ts.csv")
    else {
      warning("ss3sim_ts.csv already exists and overwrite_files = FALSE, ",
              "so a new file was not written")
    }
  } else { # can write either way
    write.csv(ts.all, file = "ss3sim_ts.csv")
  }
ret <- list(scalar = scalar.all,
                  ts = ts.all,
                  dq = dq.all)
}

#' Extract SS3 simulation results for one scenario.
#'
#' Function that extracts results from all iterations inside a supplied
#' scenario folder. The function writes 3 .csv files to the scenario
#' folder: (1) scalar metrics with one value per iteration (e.g. \eqn{R_0},
#' \eqn{h}), (2) a timeseries data ('ts') which contains multiple values per
#' iteration (e.g.  \eqn{SSB_y} for a range of years \eqn{y}), and (3) [currently
#' disabled and not tested] residuals on the log scale from the surveys
#' across all iterations. The function \code{get_results_all} loops through
#' these .csv files and combines them together into a single "final"
#' dataframe.
#'
#' @param scenario A single character giving the scenario from which to
#'   extract results.
#' @param directory The directory which contains the scenario folder.
#' @param overwrite_files A boolean (default is \code{FALSE}) for whether to delete
#'   any files previously created with this function. This is intended to be
#'   used if iterations were added since the last time it was called, or any
#'   changes were made to this function.
#' @author Cole Monnahan and Kathryn Doering
#' @family get-results
#' @export
#' @examples
#' \dontrun{
#' d <- system.file("extdata", package = "ss3sim")
#' case_folder <- file.path(d, "eg-cases")
#' om <- file.path(d, "models", "cod-om")
#' em <- file.path(d, "models", "cod-em")
#' run_ss3sim(iterations = 1:2, scenarios =
#'   c("D0-F0-cod"),
#'   case_folder = case_folder, om_dir = om, em_dir = em,
#'   case_files = list(F = "F",
#'                     D = c("index", "lcomp", "agecomp")),
#'   bias_adjust = FALSE)
#' get_results_scenario(c("D0-F0-cod"), overwrite_files = TRUE)
#'
#' # clean up
#' unlink("D0-F0-cod", recursive = TRUE)
#' }
get_results_scenario <- function(scenario, directory = getwd(),
                                 overwrite_files = FALSE) {
    ## This function moves the wd around so make sure to reset on exit,
    ## especially in case of an error
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    if (file.exists(file.path(directory, scenario))) {
        setwd(file.path(directory, scenario))
    } else {
        stop(paste("Scenario", scenario, "does not exist in", directory))
    }
    ## Stop if the files already exist or maybe delete them
    scalar.file <- paste0("results_scalar_", scenario, ".csv")
    ts.file <- paste0("results_ts_", scenario, ".csv")
    dq.file <- paste0("results_dq_", scenario, ".csv")
    resids.file <- paste0("results_resids_", scenario, ".csv")
    if (file.exists(scalar.file) | file.exists(ts.file) | file.exists(dq.file)) {
        if (overwrite_files) {
            ## Delete them and continue
            message("Files deleted for ", scenario)
            file.remove(scalar.file, ts.file, dq.file)
        } else {
            ## Stop the progress
            stop("Files already exist for ", scenario, "
              and overwrite_files=FALSE")
        }
    }

    ## Loop through each iteration and get results from both models
    reps.dirs <- list.files(pattern = "[0-9]+$")
    reps.dirs <- as.character(sort(as.numeric(reps.dirs)))
    if (length(reps.dirs) == 0) {
      stop("Error:No iterations for scenario", scenario)
    }
    message("Starting ", scenario, " with ", length(reps.dirs), " iterations")
    ## Get the number of columns for this scenario
    get_results_scen_list <- lapply(reps.dirs, get_results_iter)
    # use this function to turn to bind the list components into 1 df

    ## Combine them together
    scen_dfs <- lapply(c("scalar", "timeseries", "derived"), make_df,
                          list_df = get_results_scen_list)
    names(scen_dfs) <- c("scalar", "ts", "dq")
    scalar <- scen_dfs[["scalar"]]
    ts <- scen_dfs[["ts"]]
    dq <- scen_dfs[["dq"]]
    scalar$scenario <- ts$scenario <- dq$scenario <- scenario

    ## Write them to file in the scenario folder
    scalar.exists <- file.exists(scalar.file)
    write.table(x = scalar, file = scalar.file, append = scalar.exists,
                col.names = !scalar.exists, row.names = FALSE, sep = ",")
    ts.exists <- file.exists(ts.file)
    write.table(x = ts, file = ts.file, append = ts.exists,
                col.names = !ts.exists, row.names = FALSE, sep = ",")
    dq.exists <- file.exists(dq.file)
    write.table(x = dq, file = dq.file, append = dq.exists,
                col.names = !dq.exists, row.names = FALSE, sep = ",")
    ret <- list(scalar = scalar,
                ts = ts,
                dq = dq)
}


#' Get results for 1 iteration
#'
#' @param dir_1_iter The full or relative path to the SS iteration folder.
#'  Assumed to contain multiple model folders that contain "om" or "em"
#'  (not case sensitive) somewhere in the model file name. If specified,
#'  mod_dirs need not be specified.
#' @param mod_dirs The full or relative path to the SS model folders as a
#'  vector of characters. If specified, dir_1_iter need not be specified.
#' @param iter_name Name of the iteration, which will be appended to the
#'  dataframes . Defaults to NULL, in which case the iter_name will be the
#'  folder name of dir_1_iter or the folder name 1 level up from the first
#'  mod_dirs specified
#' @author Kathryn Doering
#' @export
#' @return A list of 3 data frames called scalar, timeseries, and
#'  derived (for derived quantities). These lists contain information for
#'  multiple model runs (estimation models and operating models) for 1
#'  iteration.
get_results_iter <- function(dir_1_iter = NULL, mod_dirs = NULL,
                             iter_name = NULL) {
  # checks
  if (is.null(dir_1_iter) & is.null(mod_dirs)) {
    stop("Please specify either dir_1_iter or mod_dirs.")
  }
  if (!is.null(dir_1_iter) & !is.null(mod_dirs)) {
    stop("Please specify only dir_1_iter or mod_dirs, leaving the other NULL.")
  }
  if (!is.null(dir_1_iter)) {
    dir_1_iter <- normalizePath(dir_1_iter)
  }
  if (!is.null(mod_dirs)) {
    mod_dirs <- normalizePath(mod_dirs)
  }
  # get the directories if not prespecified.
  if (!is.null(dir_1_iter)) {
    mod_dirs <- list.dirs(dir_1_iter, recursive = FALSE)
    mod_dirs <- grep("[oe]m", mod_dirs, value = TRUE, ignore.case = TRUE)
  }
  if (is.null(iter_name)) {
   iter_name <- basename(dirname(mod_dirs[1]))
  }

  # call get_results_mod
  iter_list <- lapply(mod_dirs, get_results_mod)
  return_iter <- lapply(c("scalar", "timeseries", "derived"), make_df,
                        list_df = iter_list)
  names(return_iter) <- c("scalar", "timeseries", "derived")
  # todo: find the iteration value to set
  return_iter$scalar$iteration <- return_iter$timeseries$iteration <-
    return_iter$derived$iteration <- iter_name
  # return the iteration level dfs as a list
  return_iter
}

#' Get results for 1 model run
#'
#' @param dir The full or relative path to the SS model file folder. If not
#'  specified, uses the working directory.
#' @param is_EM Is this an estimation model? Defaults to NULL, which will look
#' for the letters "em" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @param is_OM Is this an operating model? Defaults to NULL, which will look
#' for the letters "om" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @author Kathryn Doering
#' @export
#' @importFrom r4ss SS_output
#' @return A list of 3 data frames called scalar, timeseries, and
#'  derived (for derived quantities). These data frames contain results for 1
#'  model run.
get_results_mod <- function(dir = getwd(), is_EM = NULL, is_OM = NULL) {
  # Input checks:
  if (!file.exists(file.path(dir, "Report.sso")) |
     file.size(file.path(dir, "Report.sso")) == 0) {
    message("Missing Report.sso file for: ", dir, "; skipping...")
    return(NA)
  }
  # figure out if is EM and if forecast report should be read
  if (is.null(is_EM)) {
    if (length(grep("em", basename(dir), ignore.case = TRUE)) > 0) {
      is_EM <- TRUE
    } else {
      is_EM <- FALSE
    }
  }
  if (is.null(is_OM)) {
    if (length(grep("om", basename(dir), ignore.case = TRUE)) > 0) {
      is_OM <- TRUE
    } else {
      is_OM <- FALSE
    }
  }
  if (is_EM) {
  forecastTF <- ifelse(
    file.size(file.path(dir, "Forecast-report.sso")) %in% c(0, NA),
    FALSE, TRUE)
  } else {
    forecastTF <- FALSE
  }
  report <- SS_output(file.path(dir), covar = FALSE, verbose = FALSE,
                      compfile = "none", forecast = forecastTF, warn = TRUE,
                      readwt = FALSE, printstats = FALSE, NoCompOK = TRUE,
                      ncols = NULL)
  ## Get dfs
  scalar <- get_results_scalar(report)
  timeseries <- get_results_timeseries(report)
  derived <- get_results_derived(report)
  # add additional values
  scalar$model_run <- timeseries$model_run <- derived$model_run <- basename(dir)

  if (is_OM) {
    # these values are meaningless in context of an OM.
    scalar$max_grad <- NA
    scalar$params_on_bound <- NA
    scalar$params_stuck_low <- NA
    scalar$params_stuck_high <- NA
  }
  ## Other calcs (TODO: change these at indiv spreadsheet level, if can find
  # how to get these output from SS_output)
  scalar$hessian <- file.exists(file.path(dir, "admodel.cov"))
  ## The number of iterations for the run is only in ss_summary.sso and
  # CumReport.sso for some reason.
  if (!file.exists(file.path(dir, "ss_summary.sso"))) {
    Niterations <- NA
  } else {
    sumrep <- readLines(file.path(dir, "ss_summary.sso"), n = 10)
    tmp <- grep("N_iterations: ", sumrep)
    if (length(tmp) == 0) {
      scalar$Niterations <- NA
    } else {
      scalar$Niterations <-
        as.numeric(strsplit(sumrep[tmp[1]], split = "N_iterations: ")[[1]][2])
    }
  }

  # list to return
  results_mod <- list(
                    scalar = scalar,
                    timeseries = timeseries,
                    derived = derived
                  )
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
get_results_timeseries <- function(report.file) {
    years <- report.file$startyr:(report.file$endyr +
                                  ifelse(is.na(report.file$nforecastyears) ==
                                      TRUE, 0,
                                         report.file$nforecastyears))
    xx <- subset(report.file$timeseries,
                 select = c("Yr", "SpawnBio", "Recruit_0", "F:_1"))
    xx <- xx[xx$Yr %in% years, ]
    names(xx) <- gsub(":_1", "", names(xx))
    # Get SPR from derived_quants
    spr <- report.file$derived_quants[grep("SPRratio_",
      report.file$derived_quants[,
      grep("label", colnames(report.file$derived_quants),
      ignore.case = TRUE)]), ]
    spr$Yr <- sapply(strsplit(
      spr[, grep("label", colnames(spr), ignore.case = TRUE)], "_"), "[", 2)
    colnames(spr)[which(colnames(spr) == "Value")] <- "SPRratio"
    # Get recruitment deviations
    dev <- report.file$recruit
    getcols <- c(grep("^y", colnames(dev), ignore.case = TRUE),
      grep("dev", colnames(dev), ignore.case = TRUE))
    dev <- dev[dev[, getcols[1]] %in% years, getcols]
    ## create final data.frame
    df <- merge(xx, spr[, c("SPRratio", "Yr")], by = "Yr", all.x = TRUE)
    df$SPRratio[is.na(df$SPRratio)] <- 0
    df <- merge(df, dev, by.x = "Yr",
      by.y = colnames(dev)[getcols[1]], all.x = TRUE)
    rownames(df) <- NULL
    # change year name
    df$year <- df$Yr
    df$Yr <- NULL
    df
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
get_results_derived <- function(report.file) {
    # todo: Move val-1/std to stddev column for those pars that need it
    # todo: move time series values to the time series data frame
    # todo: move the point estimates to the scalar data frame
    xx <- report.file$derived_quants
    xx <- xx[, c(
      grep("Label", colnames(report.file$derived_quants),
        ignore.case = TRUE, value = TRUE),
      c("Value", "StdDev"))]
    tosplit <- strsplit(
      xx[, grep("Label", colnames(xx), ignore.case = TRUE)], "_")
    xx$Yr <- sapply(tosplit, "[", 2)
    xx$name <- sapply(tosplit, "[", 1)
    badname <- grep("Label", colnames(xx), value = TRUE, ignore.case = TRUE)
    if (all(xx$StdDev == 0)) xx <- xx[, -which(colnames(xx) == "StdDev")]
    xx <- xx[grep("[0-9]", xx$Yr), ]
    xx$name <- gsub("\\(|\\)", "", xx$name)
    final <- reshape(xx, timevar = "name", idvar = "Yr", direction = "wide",
      drop = badname)
    rownames(final) <- NULL
    # change year name.
    final$year <- final$Yr
    final$Yr <- NULL
    final
}

#' Extract scalar quantities from a model run.
#'
#' Extract scalar quantities from an \code{\link[r4ss]{SS_output}} list from a model run.
#' Returns a data.frame of the results (a single row) which can be rbinded later.
#' @template report.file
#' @family get-results
#' @export
#' @author Cole Monnahan; Merrill Rudd
get_results_scalar <- function(report.file) {
    der <- report.file$derived_quants
    getcol <- grep("label", colnames(der), ignore.case = TRUE)
    SSB_MSY <- der[which(der[, getcol] =="SSB_MSY"), ]$Value
    TotYield_MSY <-  der[which(der[, getcol] =="Dead_Catch_MSY"),]$Value
    SSB_Unfished <-  der[grep("^SSB_unfished", der[, getcol], ignore.case = TRUE), "Value"]
    F_MSY <- der[grep("Fsdt_MSY|annF_MSY", der[, getcol]), "Value"]
    F_SPR <- der[grep("Fsdt_SPR|annF_SPR", der[, getcol]), "Value"]
    Catch_endyear <-
        utils::tail(report.file$timeseries[report.file$timeseries$Era == "TIME", grep("dead\\(B\\)",
          names(report.file$timeseries))], 1)
    pars <- data.frame(t(report.file$parameters$Value), stringsAsFactors = FALSE)
    names(pars) <- report.file$parameters[, grep("Label", colnames(report.file$parameters), ignore.case = TRUE)]
    ## Get the parameters stuck on bounds
    status <- report.file$parameters$Status
    params_stuck_low <- paste(names(pars)[which(status == "LO")], collapse = ";")
    params_stuck_high <- paste(names(pars)[which(status == "HI")], collapse = ";")
    if (params_stuck_low == "") params_stuck_low <- NA
    if (params_stuck_high == "") params_stuck_high <- NA
    ## Remove the recruitment devs and efforts as these are in the ts file
    recdev.index <- grep("MAIN_", toupper(names(pars)), fixed = TRUE)
    if (length(recdev.index) > 0) pars <- pars[, -recdev.index]
    effort.index <- grep("F_FLEET_", toupper(names(pars)), fixed = TRUE)
    if (length(effort.index) > 0) pars <- pars[, -effort.index]
    names(pars) <- gsub("\\(", "_", names(pars))
    names(pars) <- gsub("\\)", "", names(pars))
    # get the comps variables
    # todo: change this permanently to second option when converted to .15
    if ("Length_Comp_Fit_Summary" %in% names(report.file)) {
      report.file[["Length_comp_Eff_N_tuning_check"]] <-
        report.file[["Length_Comp_Fit_Summary"]]
    }
    if (nrow(report.file[["Length_comp_Eff_N_tuning_check"]]) > 0) {
      len_comp_tuning <- data.frame(t(report.file$Length_comp_Eff_N_tuning_check$Curr_Var_Adj),
                                    stringsAsFactors = FALSE)
      colnames(len_comp_tuning) <-
        paste0("Curr_Var_Adj_lcomp_flt_",
               report.file$Length_comp_Eff_N_tuning_check$Fleet, "_",
               report.file$Length_comp_Eff_N_tuning_check$Fleet_name)
    } else {
      len_comp_tuning <- data.frame(matrix(nrow = 1, ncol = 0),
                                    stringsAsFactors = FALSE)
    }
    # todo: change this permanently to second option when converted to .15
    if ("Age_Comp_Fit_Summary" %in% names(report.file)) {
      report.file[["Age_comp_Eff_N_tuning_check"]] <-
        report.file[["Age_Comp_Fit_Summary"]]
    }
    if (nrow(report.file[["Age_comp_Eff_N_tuning_check"]]) > 0) {
      age_comp_tuning <- data.frame(t(report.file$Age_comp_Eff_N_tuning_check$Curr_Var_Adj),
                                    stringsAsFactors = FALSE)
      colnames(age_comp_tuning) <-
        paste0("Curr_Var_Adj_agecomp_flt_",
               report.file$Age_comp_Eff_N_tuning_check$Fleet, "_",
               report.file$Age_comp_Eff_N_tuning_check$Fleet_name)
    } else {
      age_comp_tuning <- data.frame(matrix(nrow = 1, ncol = 0),
                                    stringsAsFactors = FALSE)
    }
    max_grad <- report.file$maximum_gradient_component
    depletion <- report.file$current_depletion
    NLL_vec <- get_nll_components(report.file)
    ## Obtain bias adjustment parameters
    bias <- report.file$breakpoints_for_bias_adjustment_ramp
    ## get the number of params on bounds from the warning.sso file, useful for
    ## checking convergence issues
    warn <- report.file$warnings
    warn.line <- grep("Number_of_active_parameters", warn, fixed = TRUE)
    params_on_bound <-
        ifelse(length(warn.line) == 1,
          as.numeric(strsplit(warn[warn.line], split = ":")[[1]][2]), NA)
    ## Combine into final df and return it
    df <- data.frame(SSB_MSY, TotYield_MSY, SSB_Unfished, max_grad, depletion
                , F_MSY, F_SPR, bias,
                params_on_bound, params_stuck_low, params_stuck_high, pars,
                Catch_endyear, t(NLL_vec), len_comp_tuning, age_comp_tuning,
                stringsAsFactors = FALSE)
    ## Also get some meta data and other convergence info like the
    ## version, runtime, etc. as checks
    df$version <- report.file$SS_version
    df$RunTime <- eval(parse(text = gsub(
      "([0-9]+) hours, ([0-9]+) minutes, ([0-9]+) seconds.",
      "\\1*60+\\2+\\3/60", report.file$RunTime)))
    return(invisible(df))
}

#' Get negative log likelihood (NLL) values from a report file list
#'
#' @template report.file
#' @author Merrill Rudd
get_nll_components <- function(report.file) {
    ## Possible likelihood components from SS3.tpl
    NLL_components <- c("TOTAL", "Catch", "Equil_catch", "Survey", "Discard",
      "Mean_body_wt", "Length_comp", "Age_comp", "Size_at_age", "SizeFreq",
      "Morphcomp", "Tag_comp", "Tag_negbin", "Recruitment",
      "Forecast_Recruitment", "Parm_priors", "Parm_softbounds", "Parm_devs",
      "Crash_Pen")
    NLL_names <- paste("NLL", NLL_components, sep = "_")

    like_mat <- report.file$likelihoods_used
    vec <- sapply(NLL_components, function(x)
      ifelse(length(like_mat[which(rownames(like_mat) == x), 1]) == 0,
                NA, like_mat[which(rownames(like_mat) == x), 1]))
    names(vec) <- NLL_names
    vec[is.na(vec)] <- NA

    return(vec)
}

#' Make a list of lists with dataframe components into a dataframes
#'
#' Bind together list of list components with the same name
#' @param list_name A name to subset from iter_list
#' @param list_df A list of dataframes. These need not have the same column
#'  names, as this function will fill in with NAs.
#' @author Kathryn Doering
#' @return A dataframe
make_df <- function(list_name, list_df) {
  list_df_comp <- lapply(list_df, function(x) x[[list_name]])
  all_nms <- unique(unlist(lapply(list_df_comp, names)))
  # this extra code is needed in case of extra colnames that are not in both
  # dataframes.
  df <- do.call(rbind,
                c(lapply(list_df_comp,
                         function(x) data.frame(c(x, vapply(setdiff(all_nms, names(x)),
                                                            function(y) NA, NA)),
                                                stringsAsFactors = FALSE)),
                  make.row.names = FALSE))
  df
}

#' Convert long-style ss3sim output to wide format
#'
#' This function exists for back compatibility. Note that this will only work
#' if the column model_run has only the strings"om" or "em".
#' @param lng A long dataframe produced from get_results_all().
#' @return A wide dataframe (separate columns for em and om results)
#' @export
#' @examples \dontrun{
#'   scalar <-  read.csv("ss3sim_scalar.csv")
#'   scalar_wide <- convert_to_wide(scalar)
#'
#'   ts <- read.csv("ss3sim_ts.csv")
#'   ts_wide <- convert_to_wide(scalar)
#' }
#' @author Kathryn Doering
convert_to_wide <- function(lng) {
  em_df <- lng[lng$model_run == "em", ,drop = FALSE]
  colnames(em_df) <- paste0(colnames(em_df), "_em")
  which(colnames(em_df) %in% c("iteration_em", "scenario_em"))
  colnames(em_df)[colnames(em_df) == "iteration_em"] <- "iteration"
  colnames(em_df)[colnames(em_df) == "scenario_em"] <- "scenario"
  if("year_em" %in% colnames(em_df)) {
    colnames(em_df)[colnames(em_df) == "year_em"] <- "year"
  }
  colnames(em_df)[colnames(em_df) == "max_grad_em"] <- "max_grad"
  colnames(em_df)[colnames(em_df) == "version_em"] <- "version"
  colnames(em_df)[colnames(em_df) == "RunTime_em"] <- "RunTime"
  colnames(em_df)[colnames(em_df) == "hessian_em"] <- "hessian"
  colnames(em_df)[colnames(em_df) == "Niterations_em"] <- "Niterations"
  em_df <- em_df[, setdiff(colnames(em_df), c("X_em", "model_run_em"))]

  om_df <- lng[lng$model_run == "om", ,drop = FALSE]
  colnames(om_df) <- paste0(colnames(om_df), "_om")
  colnames(om_df)[colnames(om_df) == "iteration_om"] <- "iteration"
  colnames(om_df)[colnames(om_df) == "scenario_om"] <- "scenario"
  if("year_om" %in% colnames(om_df)) {
    colnames(om_df)[colnames(om_df) == "year_om"] <- "year"
  }
  # remove some columns
  om_df <- om_df[, setdiff(colnames(om_df),
                           c("max_grad_om", "version_om", "RunTime_om",
                             "hessian_om", "Niterations_om",
                             "params_on_bound_om", "params_stuck_low_om",
                             "params_stuck_high_om", "X_om", "model_run_om"))]
  # merge back together
  wide <- merge(om_df, em_df, all = TRUE )

  # add in old cols
  wide$ID <- paste0(wide$scenario, "-", wide$iteration)

  # add code to divide ID into the different codes and species
  ## parse the scenarios into columns for plotting later
  # use this old code:
  new_cols <-
    data.frame(do.call(rbind, strsplit(gsub("([0-9]+-)", "\\1 ",
                                            as.character(wide$scenario)), "- ")),
               stringsAsFactors = FALSE)
  names(new_cols) <-
    c(substr(as.vector(as.character(
      new_cols[1,-ncol(new_cols)])), 1,1) ,"species")
  wide <- cbind(wide, new_cols)
}
