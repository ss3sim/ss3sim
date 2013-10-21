#' Run both the operating model and assessment model
#'
#' This function is a wrapper function that can call
#' \code{\link{run_ss3model}} for the operating model, sample the
#' output (add recruitment deviations, survey the data, etc.), and run
#' the estimation model. \code{ss3sim_base} is the main internal
#' function for ss3sim.
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios Which scenarios to run.
#' @param tv_params A named list containing all the
#' \code{\link{change_tv}} (time-varying) options.
#' @param f_params A named list containing all the
#' \code{\link{change_f}} options.
#' @param index_params A named list containing all the
#' \code{\link{change_index}} options.
#' @param lcomp_params A named list containing all the
#' \code{\link{change_lcomp}} options.
#' @param agecomp_params A named list containing all the
#' \code{\link{change_agecomp}} options.
#' @param retro_params A named list containing all the
#' \code{\link{change_retro}} options.
#' @param estim_params A named list containing all the
#' \code{\link{change_e}} options.
#' @param om_model_dir The directory with the operating model you want
#' to copy and use for the specified simulations.
#' @param em_model_dir The directory with the estimation model you want
#' to copy and use for the specified simulations.
#' @param user_recdevs An optional 100x100 matrix of recruitment
#' deviations to replace the recruitment deviations built into the
#' package (\code{\link{recdevs}}). The columns represent run
#' iterations and the rows represent years. Note that these will be
#' multiplied by sigma R. So, under normal conditions this matrix
#' should contain standard-normal values \code{~N(0, 1)}.
#' @param bias_adjust Run bias adjustment first? See
#' \code{\link{run_bias_ss3}}.
#' @param bias_nsim If bias adjustment is run, how many simulations
#' should the bias be estimated from? It will take the mean of the
#' adjustment factors across these runs.
#' @param bias_already_run If you've already run the bias runs for a
#' scenario (the bias folders and \code{.dat} files already exist)
#' then you can set this to \code{TRUE} to avoid re-running the bias
#' adjustment routine.
#' @param hess_always If \code{TRUE} then the Hessian will always be
#' calculated. If \code{FALSE} then the Hessian will only be
#' calculated for bias-adjustment runs thereby saving time.
#' @param print_logfile Logical. Print a log file?
#' @param sleep A time interval (in seconds) to pause on each iteration.
#' Useful if you want to reduce average CPU time -- perhaps because you're
#' working on a shared server.
#' @param seed If set to a numeric vector then \code{set.seed} will be
#' set to each successive value of the vector \code{seed} on each
#' iteration. This can be useful to make simulations reproducible. If
#' left set to \code{NULL} then the seed will not be set.
#' @param conv_crit The maximum percentage of bias iterations that can
#' produce a non-invertible Hessian before a warning will be produced.
#' If this percentage is exceeded then a file \code{WARNINGS.txt} will
#' be produced. Currently, the simulations will continue to run.
#' @param ... Anything extra to pass to \code{\link{run_ss3model}}.
#' For example, you may want to pass additional options to \code{SS3}
#' through the argument \code{admb_options}.
#' Anything that doesn't match a named argument in
#' \code{\link{run_ss3model}} will be passed to the
#' \code{\link{system}} call that runs \code{SS3}. If you are on a
#' Windows computer then you might want to pass
#' \code{show.output.on.console = FALSE} to make the simulations runs
#' faster.
#
#' @author Sean C. Anderson
#' @seealso \code{\link{run_ss3sim}}
#' @export
#' @details
#' This function is written to be flexible. You can specify the
#' natural mortality, fishing mortality, survey index,
#' length comp, and age comp parameters in the function call as list
#' objects. For a generic higher-level function, see
#' \code{\link{run_ss3sim}}.
#' @examples
#' \dontrun{
#' # Pull in file paths from the package example data:
#' d <- system.file("extdata", package = "ss3sim")
#' om_model_dir <- paste0(d, "/models/cod-om")
#' em_model_dir <- paste0(d, "/models/cod-em")
#' a <- get_caseargs(folder = paste0(d, "/eg-cases"), scenario =
#' "M0-F0-D0-R0-E0-cod")
#'
#' ss3sim_base(iterations = 1, scenarios = "M0-F0-D0-R0-E0-cod",
#' f_params = a$F, index_params = a$index, lcomp_params = a$lcomp,
#' agecomp_params = a$agecomp, tv_params = a$tv_params, retro_params =
#' a$R, estim_params = a$E, om_model_dir = om_model_dir, em_model_dir
#' = em_model_dir)
#' }

ss3sim_base <- function(iterations, scenarios, f_params,
  index_params, lcomp_params, agecomp_params, estim_params,
  tv_params, om_model_dir, em_model_dir,
  retro_params = NULL, user_recdevs = NULL, bias_adjust = FALSE,
  bias_nsim = 5, bias_already_run = FALSE, hess_always = FALSE,
  print_logfile = TRUE, sleep = 0, seed = NULL, conv_crit = 0.2, ...)
{

  # The first bias_nsim runs will be bias-adjustment runs
  if(bias_adjust) {
    iterations <- c(paste0("bias/", c(1:bias_nsim)), iterations)
  }

  for(sc in scenarios) {
    for(i in iterations) {

      # Create folders, copy models, check for necessary files, rename
      # files for consistency
      copy_ss3models(model_dir = om_model_dir, scenarios = sc,
        iterations = i, type = "om")
      copy_ss3models(model_dir = em_model_dir, scenarios = sc,
        iterations = i, type = "em")

      # If we're bias adjusting, then copy over the .ctl file to the
      # em folder
      if(bias_already_run) {
        file.copy(from = pastef(sc, "bias", "em.ctl"), to = pastef(sc,
            i, "em", "em.ctl"), overwrite = TRUE)
      }

      # The following section adds recruitment deviations
      # First, pull in sigma R from the operating model
      sigmar <- get_sigmar(pastef(sc, i, "om", "om"))

      # Second, take the true iteration, even if we're working with
      # "bias" iterations
      # This turns "bias/1" into "1" and leaves "1" unchanged
      this_run_num <- as.numeric(rev(strsplit(as.character(i), "/")[[1]])[1])

      # set the seed for this iteration?
      if(!is.null(seed[this_run_num])) {
        if(is.na(seed[this_run_num]))
          warning("Seed value missing, not setting the seed.")
        else {
          print(paste("Setting seed to", seed[this_run_num]))
          set.seed(seed[this_run_num])
        }
      }

      # recdevs is a matrix stored in the package 'data' folder
      # Columns are for iterations and rows are for years
      if(is.null(user_recdevs)) {
        sc_i_recdevs <- sigmar * recdevs[, this_run_num] - sigmar^2/2 # from the package data
      } else {
        sc_i_recdevs <- user_recdevs[, this_run_num] # user specified recdevs
      }

      # Add new rec devs overwriting om/ss3.par
      change_rec_devs(recdevs_new = sc_i_recdevs, file_in =
        pastef(sc, i, "om", "ss3.par"), file_out = pastef(sc, i,
          "om", "ss3.par"))

      # Change F
      with(f_params,
        change_f(years               = years,
                 years_alter         = years_alter,
                 fvals               = fvals,
                 file_in             = pastef(sc, i, "om", "ss3.par"),
                 file_out            = pastef(sc, i, "om", "ss3.par")))

      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)

      # Read in the data.ss_new file and write to ss3.dat in the om folder
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
        data_out = pastef(sc, i, "om", "ss3.dat"))

      # Change time-varying parameters; e.g. M, selectivity, growth...
      wd <- getwd() 
      if(!is.null(tv_params)) {
        setwd(pastef(sc, i, "om"))
        with(tv_params,
          change_tv(change_tv_list      = tv_params,
                    ctl_file_in         = "om.ctl",
                    ctl_file_out        = "om.ctl"))

        setwd(wd)
      }
      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)

      # Read in the data.ss_new file and move it to the em folder
       extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
         data_out = pastef(sc, i, "em", "ss3.dat"))

      # Survey biomass index
      SS.dat = r4ss::SS_readdat(pastef(sc, i, "em", "ss3.dat"),
        verbose = FALSE)
      with(index_params,
        change_index(infile          = SS.dat,
                     outfile         = pastef(sc, i, "em", "ss3.dat"),
                     fleets          = fleets,
                     years           = years,
                     sds_obs         = sds_obs))

      # Add error in the length comp data
      SS.dat = r4ss::SS_readdat(pastef(sc, i, "em", "ss3.dat"),
        verbose = FALSE)
      with(lcomp_params,
        change_lcomp(infile           = SS.dat,
                     outfile          = pastef(sc, i, "em", "ss3.dat"),
                     fleets           = fleets,
                     Nsamp            = Nsamp,
                     years            = years,
                     lengthbin_vector = lengthbin_vector,
                     cpar             = cpar))

      # Add error in the age comp data
      SS.dat2 = r4ss::SS_readdat(pastef(sc, i, "em", "ss3.dat"),
        verbose = FALSE)
      with(agecomp_params,
        change_agecomp(infile         = SS.dat,
                     outfile          = pastef(sc, i, "em", "ss3.dat"),
                     fleets           = fleets,
                     Nsamp            = Nsamp,
                     years            = years,
                     agebin_vector    = agebin_vector,
                     cpar             = cpar))

      # Manipulate EM starter file for a possible retrospective analysis
      if(!is.null(retro_params)) {
      with(retro_params,
        change_retro(startfile_in    = pastef(sc, i, "em", "starter.ss"),
                     startfile_out   = pastef(sc, i, "em", "starter.ss"),
                     retro_yr        = retro_yr))
      }

      # Manipulate EM control file to adjust what gets estimated
      # We'll only a portion of the function, the ctl part if
      # it's a bias run or if bias adjustment isn't getting run.
      # This is because the bias adjustment runs
      # already manipulates the .ctl file appropriately.
      # Must always run the other portion for the forecast

      run_change_e_full <- FALSE # default
      if(grepl("bias", i))  # it's a bias run
        run_change_e_full <- TRUE
      if(!bias_adjust)      # we aren't running bias adjustment
        run_change_e_full <- TRUE

      setwd(pastef(sc, i, "em"))
      with(estim_params,
       change_e(ctl_file_in          = "em.ctl",
                ctl_file_out         = "em.ctl",
                dat_file_in          = "ss3.dat",
                for_file_in          = "forecast.ss",
                natM_type            = natM_type,
                natM_n_breakpoints   = natM_n_breakpoints,
                natM_lorenzen        = natM_lorenzen,
                natM_val             = natM_val,
                par_name             = par_name,
                par_int              = par_int,
                par_phase            = par_phase,
                forecast_num         = forecast_num,
                run_change_e_full    = run_change_e_full))
      setwd(wd)

      # Should we calculate the hessian?
        if(hess_always){
          hess <- TRUE           # estimate the hessian no matter what
        } else {
          if(grepl("bias", i)) { # it's a bias run so we need the hessian
            hess <- TRUE
          } else {               # not a bias run, and hessian not specified
            hess <- FALSE
        }}

      run_ss3model(scenarios = sc, iterations = i, type = "em",
        hess = hess, ...)

      # Should we run bias adjustment? We should if bias_adjust is
      # true, and we are done running bias adjustments (i.e. we're on
      # the last "bias" iteration), and we haven't already run this
      # yet.
      if(bias_adjust & i == pastef("bias", bias_nsim) & !bias_already_run) {
        run_bias_ss3(dir = pastef(sc, "bias"), outdir = pastef(sc,
            "bias"), nsim = bias_nsim, conv_crit = conv_crit)
        bias_already_run <- TRUE
      # Since we've now run the bias adjustment routine, copy the .ctl
      # on subsequent iterations
      }

# TODO pull the log file writing into a separate function and update
# for current arguments
      if(print_logfile) {
        today <- format(Sys.time(), "%Y-%m-%d")
        me <- Sys.info()["nodename"]
        sink(pastef(sc, i, "log.txt"))
        cat("These models were run on ", today,
            "\non the computer ", me,
            "\nin the folder ", getwd(),
            "\nwith the following arguments:", sep = "")
        cat("\n\n# Time-varying arguments\n")
        print(tv_params)
        cat("\n\n# F arguments\n")
        print(f_params)
        cat("\n\n# index arguments\n")
        print(index_params)
        cat("\n\n# lcomp arguments\n")
        print(lcomp_params)
        cat("\n\n# agecomp arguments\n")
        print(agecomp_params)
        cat("\n\n# retro arguments\n")
        print(retro_params)
        cat("\n\n# bias adjust?\n")
        print(bias_adjust)
        cat("\n\n# bias nsim\n")
        print(bias_nsim)
        cat("\n\n# hess always?\n")
        print(hess_always)
        cat("\n\n# User recdevs?\n")
        print(user_recdevs)
        cat("\n\n# Bias already run?\n")
        print(bias_already_run)
        cat("\n\n# This run used the recruitment deviations (before scaling to sigma r):\n")
        print(sc_i_recdevs)
        cat("\n\n# With sigma r of\n")
        print(sigmar)

        sink()
      }

      # Pause to reduce average CPUE use?
      Sys.sleep(sleep)

    } # end iterations
  } # end scenarios
}

