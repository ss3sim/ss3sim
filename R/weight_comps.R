#' Weight comps in the EM
#'
#' Weight age or length composition data in the Estimation model using a
#' tuning method (MacAllister-Ianelli or Francis) or a different likelhood
#' (dirichlet multinomial)
#' @param method  Which method of comp weighting shoudl be use? Options are
#'  "MI" (for MacAllister-Ianelli), "Francis", or "DM" (for dirichlet
#'  multinomial). For now, only 1 option can be used

#' @param run A logical value specifying if the model should be run. If \code{FALSE},
#'  Stock Synthesis will not be run, just the parameters will be calculated. Note
#'  that run = \code{FALSE} will only work for \code{niters_weighting = 1} and
#'  \code{method = "MI"} or \code{"Francis"}
#' @param niters_weighting The number of times you want to tune the model.
# #'  @param writedir A directory to write new model runs to other than dir. If
# #'  NULL, the model runs will occur in the same directory (in multiple subfolders
# #'  if multiple method are used.)
# #' @param dirbase A path to the Stock Synthesis files.By default, this is the
# #'  working directory.
#' @param iter The iteration. A numeric value. This function assumes model
#'  files are in a directory relative to the path \code{"scen/iter/EM"} as in
#'  ss3sim.
#' @param scen The scenario, named as in ss3sim. This function assumes model
#'  files are in a directory relative to the path \code{"scen/iter/EM"} as in
#'  ss3sim.
#' @param bias_adjust Run bias adjustment first?.
#' @param hess_always If \code{TRUE} then the Hessian will always be calculated.
#'  If \code{FALSE} then the Hessian will only be calculated for
#'  bias-adjustment runs thereby saving time.
#' @param fleets A vector of numeric values specifying which fleets you want to tune.
weight_comps <- function(method = c("MI", "Francis", "DM"),
                         #dirbase = ".",
                         run = TRUE,
                         niters_weighting = 1,
                         #writedir = NULL,
                         iter,
                         scen,
                         bias_adjust = FALSE,
                         hess_always = FALSE,
                         fleets,
                         ...) {
# Input checks
  if(length(method) > 1) {
    stop("More than 1 method specified. Please specify method ",
         "as ONE of the following: 'MI', 'Francis', 'DM'.")
  }
  if(!all(method %in% c("MI", "Francis", "DM"))) {
    stop("Invalid input for parameter method: ", method, ". Please make sure method ",
         "is one or more of the following: 'MI', 'Francis', 'DM'." )
  }
  if(run == FALSE & niters_weighting > 1 & (!method %in% c("MI", "Francis"))) {
    stop("Invalid options specified. run = FALSE will only work for ",
         "niters_weighting = 1 and method = MI or Francis")
  }
  # need some input check for fleets? maybe check if they are present in
  # age or length comps and exclude if in neither?
  mod_path <- file.path(scen, iter, "em")
  # Tuning methods: MI, Francis ----
  if(method == "MI"| method == "Francis") {
    # 1. Initial model run
    if(run == TRUE) {
    run_ss3model(scenarios = scen, iterations = iter, type = "em",
                 hess = ifelse(bias_adjust, TRUE, hess_always), ...)
    success <- get_success(dir = mod_path)
    }
    weights <- vector("list", length = niters_weighting) # list for storing est. weights.
    for(it in seq_along(niters_weighting)) {
      # 2. get the tunings
      suppressWarnings(
        out <- r4ss::SS_output(mod_path, verbose = FALSE, printstats = FALSE,
                               hidewarn = TRUE)
      )
      # construct the variance adjustment
      #MI weights are here:
      var_adj <-  rbind(out$Length_comp_Eff_N_tuning_check,
                        out$Age_comp_Eff_N_tuning_check)
      if(method == "Francis") {
        var_adj <- r4ss::SS_tune_comps(out, write = FALSE, option = "Francis",
                                       verbose = FALSE)
      }
      var_adj <- var_adj[, 1:3]
      colnames(var_adj) <- c("Factor", "Fleet", "Value")
      var_adj$Value <- ifelse(var_adj$Value > 1, 1, var_adj$Value)
      var_adj <- var_adj[var_adj$Fleet %in% fleets, ]
      start <- r4ss::SS_readstarter(file.path(mod_path, "starter.ss"),
                                    verbose = FALSE)
      dat <- r4ss::SS_readdat(file.path(mod_path, start$datfile),
                              verbose = FALSE)
      ctl <- r4ss::SS_readctl(file.path(mod_path, start$ctlfile),
                              use_datlist = TRUE, datlist = dat,
                              verbose = FALSE)
     if((nrow(var_adj)) > 0) {
       ctl[["DoVar_adjust"]] <- 1
       if(is.null(ctl[["Variance_adjustment_list"]])) {
        # create the list if it does not already exist
        ctl$Variance_adjustment_list <- var_adj
       } else {
        # leave all var adj intact, unless they match factor and fleet in var_adj.
         cur_var_adj <- ctl[["Variance_adjustment_list"]]
         for (i in seq_len(nrow(var_adj))) {
           tmp_fac <- var_adj[i, "Factor"]
           tmp_flt <- var_adj[i, "Fleet"]
           tmp_row <- which(ctl$Variance_adjustment_list[,"Factor"] == tmp_fac &
                 ctl$Variance_adjustment_list[,"Fleet"] == tmp_flt)
           if(length(tmp_row) == 1) {
             ctl$Variance_adjustment_list[tmp_row,] <- var_adj[i, ]
           } else if (length(tmp_row) == 0) {
             ctl$Variance_adjustment_list <- rbind(ctl$Variance_adjustment_list, var_adj[i, ])
           }
           # sanity check. If user recieving this error message, function is not
           # working as developer intended.
           if(length(tmp_row) > 1) {
             stop("Multiple rows with same factor and fleet in the variance ",
                  "variance adjustment list, which should not be possible. Please",
                  " check that the control file will work with SS. If still having",
                  " issues, please report your problem: ",
                  "https://github.com/ss3sim/ss3sim/issues")
           }
         }
       }
     }
     r4ss::SS_writectl(ctl,
                       file.path(mod_path, start$ctlfile),
                       overwrite = TRUE,
                       verbose = FALSE)
      # 4. run SS again with reweighting
      if(run == TRUE) {
        run_ss3model(scenarios = scen, iterations = iter, type = "em",
                     hess = ifelse(bias_adjust, TRUE, hess_always), ...)
        #TODO:I think this only works if you delete files from old runs or
        # run it in a new directory; think about how to make this a real check.
        success <- get_success(dir = mod_path)
      }
       # save the weights from the run to a list
       weights[[it]] <- var_adj
    }
  }
  # DM ----
  #method using parameter estimation (i.e., do not need to iteratively rerun model)
  if("DM" %in% method) {
    # get the r4ss files
    start <- r4ss::SS_readstarter(file.path(mod_path, "starter.ss"), verbose = FALSE)
    dat <- r4ss::SS_readdat(file.path(mod_path, start$datfile), verbose = FALSE)
    ctl <- r4ss::SS_readctl(file.path(mod_path, start$ctlfile),
                                      use_datlist = TRUE, datlist = dat,
                            verbose = FALSE)
    # determine which fleets specified by user are included in model
    fleets_len <- fleets[fleets %in% unique(dat[["lencomp"]][,"FltSvy"])]
    fleets_age <- fleets[fleets %in% unique(dat[["agecomp"]][,"FltSvy"])]

    # 1. specify the parameters in the data file need to do dirichlet MN
    dat[["len_info"]][fleets_len,"CompError"] <- 1
    dat[["age_info"]][fleets_age,"CompError"] <- 1
    #TODO: make this more general so can share params across fleets?
    dat[["len_info"]][fleets_len, "ParmSelect"] <- seq_len(length(fleets_len))
    dat[["age_info"]][fleets_age, "ParmSelect"] <-
      (length(fleets_len)+1):(length(fleets_len)+length(fleets_age))
    npars <- length(fleets_len) + length(fleets_age)
    ctl[["dirichlet_parms"]] <- data.frame("LO" = rep(-5, times = npars),
                                           "HI" = 5,
                                           "INIT" = 0,
                                           "PRIOR" = 0,
                                           "PR_SD" = 1,
                                           "PR_type" = 0,
                                           "PHASE" = 2,
                                           "env_var&link" = 0,
                                           "dev_link" = 0,
                                           "dev_minyr" = 0,
                                           "dev_maxyr" = 0,
                                           "dev_PH" = 0,
                                           "Block" = 0,
                                           "Block_Fxn" = 0)
    # 3. Run the model once - look for convergence
    r4ss::SS_writedat(dat, file.path(mod_path, start$datfile), verbose = FALSE,
                overwrite = TRUE)
    r4ss::SS_writectl(ctl, file.path(mod_path, start$ctlfile), verbose = FALSE,
                overwrite = TRUE)
    run_ss3model(scenarios = scen, iterations = iter, type = "em",
                 hess = ifelse(bias_adjust, TRUE, hess_always), ...)
    success <- get_success(dir = mod_path)
    suppressWarnings(
      out <- r4ss::SS_output(mod_path, verbose = FALSE, printstats = FALSE,
                             hidewarn = TRUE)
    )
    # figure out what to read in for weights? maybe the DM param ests?
    weights <- out[["Dirichlet_Multinomial_pars"]]
  }
  weights
}

