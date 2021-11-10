#' Identify scenarios in `directory`
#'
#' Find folders within `directory` that contain iterations,
#' i.e., "1", "2", "3", ..., and thus, allowing for unique scenario names.
#' @param directory The directory that you want to search for scenarios.
#'   The search is recursive, and thus, it is in one's best interest to
#'   provide a shorter path name rather than one high up in the call stack.
#' @return A character vector of relative paths to directories that contain
#'   iterations.
#' @author Merrill Rudd
#' @export
id_scenarios <- function(directory) {
  all.dirs <- list.dirs(path = directory, full.names = FALSE, recursive = TRUE)
  seperator <- paste0(.Platform$file.sep, "[0-9]+", .Platform$file.sep)
  scensfull <- grep(seperator, all.dirs, value = TRUE)
  scens <- unique(sapply(strsplit(scensfull, seperator), "[[", 1))
  if (length(scens) == 0) {
    stop("No scenario folders were found in ", directory)
  }
  return(scens)
}

#' Extract Stock Synthesis simulation output
#'
#' This high level function extracts results from Stock Synthesis model runs. Give it a
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
#'   in. Default is `NULL`, which indicates find all scenario folders in
#'   `directory`.
#' @param type A character string specifying if you want the results to be
#'   written to the disk and returned as a long or wide data frame, where the
#'   default is `"long"`.
#' @param filename_prefix A character string specifying a prefix to append to
#'   the filename. Defaults to "ss3sim".
#' @export
#' @return Returns a list of 3 dataframes: scalar, ts, and dq.
#' Creates two .csv files in the current working directory,
#' where the names of those files are based on `filename_prefix`
#' and the default leads to the following:
#' `ss3sim_ts.csv` and `ss3sim_scalar.csv`.
#' @author Cole Monnahan, Merrill Rudd, Kathryn Doering
#' @family get-results
get_results_all <- function(directory = getwd(), overwrite_files = FALSE,
  user_scenarios = NULL, type = c("long", "wide"), filename_prefix = "ss3sim") {

    old_wd <- getwd()
    on.exit(setwd(old_wd))

  type <- match.arg(type, several.ok = FALSE)

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

    if (length(scenarios) == 0) {
        stop("No scenarios found in:", directory)
    }
    message("Extracting results from", length(scenarios), "scenarios")

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
      scalar.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(scalar.file, stringsAsFactors = FALSE)), error = function(e) NA)
      ts.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(ts.file, stringsAsFactors = FALSE)), error = function(e) NA)
      dq.list[[i]] <- tryCatch(suppressWarnings(utils::read.csv(dq.file, stringsAsFactors = FALSE)), error = function(e) NA)
  }
  scalar.list <- scalar.list[which(!is.na(scalar.list))]
  ts.list <- ts.list[which(!is.na(ts.list))]
  dq.list <- dq.list[which(!is.na(dq.list))]
  ## Combine all scenarios together and save into big final files
  scalar.all <- add_colnames(scalar.list, bind = TRUE)
  ts.all <- add_colnames(ts.list, bind = TRUE)
  dq.all <- add_colnames(dq.list, bind = TRUE)
  if (type == "wide") {
    scalar.all <- convert_to_wide(scalar.all)
    ts.all <- convert_to_wide(ts.all)
    dq.all <- convert_to_wide(dq.all)
  }
  scalar.file.all <- paste0(filename_prefix, "_scalar.csv")
  ts.file.all <- paste0(filename_prefix, "_ts.csv")
  dq.file.all <- paste0(filename_prefix, "_dq.csv")
  if (file.exists(scalar.file.all) & !overwrite_files) {
    warning(scalar.file.all, " already exists and overwrite_files = FALSE, ",
      "so a new file was not written.")
  } else { # can write either way
    utils::write.csv(scalar.all, file = scalar.file.all, row.names = FALSE)
  }
  if (file.exists(ts.file.all) & !overwrite_files) {
    warning(ts.file.all, " already exists and overwrite_files = FALSE, ",
      "so a new file was not written.")
  } else { # can write either way
    utils::write.csv(ts.all, file = ts.file.all, row.names = FALSE)
  }
  if (file.exists(dq.file.all) & !overwrite_files) {
    warning(dq.file.all, " already exists and overwrite_files = FALSE, ",
            "so a new file was not written.")
  } else { # can write either way
    utils::write.csv(dq.all, file = dq.file.all, row.names = FALSE)
  }
ret <- list(scalar = scalar.all,
                  ts = ts.all,
                  dq = dq.all)
}

#' Extract Stock Synthesis simulation results for one scenario
#'
#' Extract results from all iterations inside a supplied scenario folder.
#' The function writes the following .csv files to the scenario folder
#' 1. scalar metrics with one value per iteration
#'    (e.g. \eqn{R_0}, \eqn{h}),
#' 1. a timeseries data ('ts') which contains multiple values per iteration
#'    (e.g.  \eqn{SSB_y} for a range of years \eqn{y}), and
#' 1. residuals on the log scale from the surveys across all iterations;
#'    this functionality is currently disabled and not tested.
#'
#' @seealso
#' [get_results_all()] loops through these .csv files and
#' combines them together into a single "final" dataframe.
#'
#' @param scenario A single character giving the scenario from which to
#'   extract results.
#' @param directory The directory which contains the scenario folder.
#' @param overwrite_files A boolean (default is `FALSE`) for whether to delete
#'   any files previously created with this function. This is intended to be
#'   used if iterations were added since the last time it was called, or any
#'   changes were made to this function.
#' @author Cole Monnahan and Kathryn Doering
#' @family get-results
#' @export
get_results_scenario <- function(scenario, directory = getwd(),
                                 overwrite_files = FALSE) {
    ## This function moves the wd around so make sure to reset on exit,
    ## especially in case of an error
    old_wd <- getwd()
    on.exit(setwd(old_wd))
    if(file.exists(normalizePath(directory, mustWork = FALSE))) {
        setwd(directory)
    }
    if (file.exists(file.path(scenario))) {
        setwd(file.path(scenario))
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
      return(vector(mode = "list", length = length(3)))
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
    utils::write.table(x = scalar, file = scalar.file, append = scalar.exists,
                col.names = !scalar.exists, row.names = FALSE, sep = ",")
    ts.exists <- file.exists(ts.file)
    utils::write.table(x = ts, file = ts.file, append = ts.exists,
                col.names = !ts.exists, row.names = FALSE, sep = ",")
    dq.exists <- file.exists(dq.file)
    utils::write.table(x = dq, file = dq.file, append = dq.exists,
                col.names = !dq.exists, row.names = FALSE, sep = ",")
    ret <- list(scalar = scalar,
                ts = ts,
                dq = dq)
}


#' Get results for 1 iteration
#'
#' @param dir_1_iter The full or relative path to the Stock Synthesis iteration folder.
#'  Assumed to contain multiple model folders that contain "om" or "em"
#'  (not case sensitive) somewhere in the model file name. If specified,
#'  mod_dirs need not be specified.
#' @param mod_dirs The full or relative path to the Stock Synthesis model folders as a
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
#' @param dir The full or relative path to the Stock Synthesis model file folder. If not
#'  specified, uses the working directory.
#' @param is_EM Is this an estimation model? Defaults to NULL, which will look
#' for the letters "em" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @param is_OM Is this an operating model? Defaults to NULL, which will look
#' for the letters "om" (lower or uppercase) to decide if this is an estimation
#' model or operating model.
#' @author Kathryn Doering
#' @export
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
  report <- r4ss::SS_output(file.path(dir), covar = FALSE, verbose = FALSE,
                      compfile = NULL, forecast = forecastTF, warn = FALSE,
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
  # how to get these output from r4ss::SS_output)
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
#' Extract time series from an [r4ss::SS_output()] list from a model run.
#' Returns a data.frame of the results for SSB, recruitment and effort by year.
#'
#' @template report.file
#' @export
#' @family get-results
#' @author Cole Monnahan
get_results_timeseries <- function(report.file) {
    years <- report.file$startyr:(report.file$endyr +
                                  ifelse(is.na(report.file$nforecastyears),
                                         0,
                                         report.file$nforecastyears))
    F_cols <- grep("^F:_", colnames(report.file$timeseries))
    catch_cols <- grep("^retain\\([B|N]\\):_", colnames(report.file$timeseries))
    dead_cols <- grep("^dead\\([B|N]\\):_", colnames(report.file$timeseries))
    other_cols <- which(colnames(report.file$timeseries) %in%
                          c("Yr", "Seas", "SpawnBio", "Recruit_0"))
    xx <- report.file$timeseries[, c(other_cols, catch_cols, dead_cols, F_cols)]
    # remove parentheses from column names because they make the names
    # non-synatic
    colnames(xx) <- gsub("\\(|\\)", "", colnames(xx))
    colnames(xx) <- gsub("\\:", "", colnames(xx))
    xx <- xx[xx$Yr %in% years, ]
    # Get SPR from derived_quants
    spr <- report.file$derived_quants[grep("SPRratio_",
      report.file$derived_quants[,
      grep("label", colnames(report.file$derived_quants),
      ignore.case = TRUE)]), ]
    if(isTRUE(nrow(spr) > 0)) {
      spr$Yr <- unlist(lapply(strsplit(
        spr[, grep("label", colnames(spr), ignore.case = TRUE)], "_"), "[", 2))
      colnames(spr)[which(colnames(spr) == "Value")] <- "SPRratio"
      spr[["Seas"]] <- 1 # need to add seasonal column; just assign to first? Or should be NA?
      df <- merge(xx, spr[, c("SPRratio", "Yr", "Seas")], by = c("Yr", "Seas"), all.x = TRUE)
      df$SPRratio[is.na(df$SPRratio)] <- 0
    } else {
      df <- xx
      df$SPRratio <- NA
    }
    # Get recruitment deviations
    dev <- report.file$recruit
    getcols <- c(grep("^y", colnames(dev), ignore.case = TRUE),
      grep("dev", colnames(dev), ignore.case = TRUE))
    dev <- dev[dev[, getcols[1]] %in% years, getcols]
    colnames(dev) <- gsub("dev", "rec_dev", colnames(dev), ignore.case = TRUE)
    dev[["Seas"]] <- 1 # Add Seas; just assign to 1? or should be NA?
    ## create final data.frame
    df <- merge(df, dev, by.x = c("Yr", "Seas"),
      by.y = c(colnames(dev)[getcols[1]], "Seas"), all.x = TRUE, all.y = TRUE)
    rownames(df) <- NULL
    # change year name
    df$year <- df$Yr
    df$Yr <- NULL
    df
}

#' Extract time series from a model run with the associated standard deviation.
#'
#' Extract time series from an [r4ss::SS_output()] list from a model run.
#' Returns a data.frame of the results for spawning stock biomass (SSB), recruitment,
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
    final <- stats::reshape(xx, timevar = "name", idvar = "Yr", direction = "wide",
      drop = badname)
    rownames(final) <- NULL
    # change year name.
    final$year <- final$Yr
    final$Yr <- NULL
    final
}

#' Extract scalar quantities from a model run.
#'
#' Extract scalar quantities from an [r4ss::SS_output()] list from a model run.
#' Returns a data.frame of the results (a single row) which can be rbinded later.
#' @template report.file
#' @family get-results
#' @export
#' @author Cole Monnahan; Merrill Rudd
get_results_scalar <- function(report.file) {
    der <- t(report.file$derived_quants[
      # Find MSY and Btarget variables
      grep("MSY$|Btgt$|SPR$|^[A-Za-z]{3,}_unfished",
      # Find the column of the derived quantities object
        report.file$derived_quants[,
          grep("Label", colnames(report.file$derived_quants))]),
      # Return the number in a transposed data frame
      "Value", drop = FALSE])
    colnames(der) <- gsub("Dead_Catch", "TotYield", colnames(der))
    colnames(der) <- gsub("_unfished", "_Unfished", colnames(der))
    colnames(der) <- gsub("annF_|Fstd_", "F_", colnames(der))
    Catch_endyear <-
        utils::tail(report.file$timeseries[report.file$timeseries$Era == "TIME", grep("dead\\(B\\)",
          names(report.file$timeseries))], 1)
    pars <- t(report.file$parameters[
      # Remove Main Recruitment Deviations and fleet_f from older Stock Synthesis output
      !grepl("main|_fleet_", report.file$parameters$Label, ignore.case = TRUE),
      # Return the number in a transposed data frame
      "Value", drop = FALSE])
    colnames(pars) <- gsub("\\(", "_", colnames(pars))
    colnames(pars) <- gsub("\\)|\\.$", "", colnames(pars))
    ## Get the parameters stuck on bounds
    params_stuck_low <- paste(report.file$parameters$Label[
      grep("LO", report.file$parameters$Status)], collapse = ";")
    params_stuck_high <- paste(report.file$parameters$Label[
      grep("HI", report.file$parameters$Status)], collapse = ";")
    if (params_stuck_low == "") params_stuck_low <- NA
    if (params_stuck_high == "") params_stuck_high <- NA
    # get the comps variables
    len_comp_tuning <- get_compfit(report.file, "Length_Comp_Fit_Summary")
    age_comp_tuning <- get_compfit(report.file, "Age_Comp_Fit_Summary")
    ## get the number of params on bounds from the warning.sso file, useful for
    ## checking convergence issues
    warn <- report.file$warnings
    warn.line <- grep("Number_of_active_parameters", warn, fixed = TRUE)
    params_on_bound <-
        ifelse(length(warn.line) == 1,
          as.numeric(strsplit(warn[warn.line], split = ":")[[1]][2]), NA)
    ## Combine into final df and return it
    df <- data.frame(der,
      max_grad = report.file$maximum_gradient_component,
      depletion = report.file$current_depletion,
      alt_sigma_r = report.file$sigma_R_info[1, "alternative_sigma_R"],
      report.file$breakpoints_for_bias_adjustment_ramp,
      params_on_bound, params_stuck_low, params_stuck_high, pars,
      Catch_endyear, get_nll_components(report.file),
      len_comp_tuning, age_comp_tuning,
      stringsAsFactors = FALSE, check.names = FALSE)
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
#' Names of the available NLL components will depend on the version
#' of the model. Names are native to the estimation framework and all
#' available components are extracted.
#' @template report.file
#' @author Merrill Rudd
#' @return A vector of named numeric values, where `"NLL_"` is
#' appended to the names in the `report.file`.
get_nll_components <- function(report.file) {
    vec <- t(report.file$likelihoods_used[, "values", drop = FALSE])
    colnames(vec) <- paste0("NLL_", row.names(report.file$likelihoods_used))
    vec[is.na(vec)] <- NA

    return(vec)
}

#' Get summaries of fits to composition data from report file list
#'
#' Extract the summary of fits to composition data, where the sections
#' are structured similarly for each type of data in the report file.
#'
#' @template report.file
#' @param name A character string that matches the element of
#' `report.file` that you wish to extract, e.g.,
#' `"Length_Comp_Fit_Summary"`.
get_compfit <- function(report.file, name) {
  if (NROW(report.file[[name]]) > 0) {
    tuning <- t(report.file[[name]][, "Curr_Var_Adj", drop = FALSE])
    cname <- switch(name,
      Length_Comp_Fit_Summary = "Curr_Var_Adj_lcomp_flt_",
      Age_Comp_Fit_Summary = "Curr_Var_Adj_agecomp_flt_")
    colnames(tuning) <- paste0(cname,
           report.file[[name]][, "Fleet"], "_",
           report.file[[name]][, "Fleet_name"])
  } else {
    tuning <- data.frame(matrix(nrow = 1, ncol = 0),
      stringsAsFactors = FALSE)
  }
  return(tuning)
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
#'   scalar <- utils::read.csv("ss3sim_scalar.csv")
#'   scalar_wide <- convert_to_wide(scalar)
#'
#'   ts <- utils::read.csv("ss3sim_ts.csv")
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
  wide <- merge(om_df, em_df, all = TRUE)
  wide <- wide[, apply(wide, 2, function(x) !all(is.na(x)))]

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
