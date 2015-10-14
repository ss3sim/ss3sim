#' Base wrapper function to run an ss3sim simulation
#'
#' This function is a wrapper function that can call \code{\link{run_ss3model}}
#' for the operating model, sample the output (add recruitment deviations,
#' survey the data, etc.), and run the estimation model. \code{ss3sim_base} is
#' the main internal function for \pkg{ss3sim}. It is intended to be used
#' through \code{\link{run_ss3sim}}, but can also be used directly.
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios Which scenarios to run.
#' @param tv_params A named list containing arguments for
#'   \code{\link{change_tv}} (time-varying).
#' @param f_params A named list containing arguments for \code{\link{change_f}}.
#'   A mandatory case.
#' @param index_params A named list containing arguments for
#'   \code{\link{sample_index}}. A mandatory case.
#' @param lcomp_params A named list containing arguments for
#'   \code{\link{sample_lcomp}}. A mandatory case.
#' @param agecomp_params A named list containing arguments for
#'   \code{\link{sample_agecomp}}. A mandatory case.
#' @param calcomp_params A named list containing arguments for
#'   \code{\link{sample_calcomp}}, for conditional age-at-length data.
#' @param wtatage_params A named list containing arguments for
#'   \code{\link{sample_wtatage}}, for empirical weight-at-age data.
#' @param mlacomp_params A named list containing arguments for
#'   \code{\link{sample_mlacomp}}, for mean length-at-age data.
#' @param retro_params A named list containing arguments for
#'   \code{\link{change_retro}}.
#' @param estim_params A named list containing arguments for
#'   \code{\link{change_e}}.
#' @param em_binning_params A named list containing arguments for
#'   \code{\link{change_em_binning}}.
#' @param data_params A named list containing arguments for
#'   \code{\link{change_data}}.
#' @param call_change_data A boolean of whether to call
#'   \code{\link{change_data}} and modify the OM at each iteration. Defaults to
#'   \code{TRUE}. See the vignette for further information on why you might
#'   choose to turn this off.
#' @param om_dir The directory with the operating model you want to copy and use
#'   for the specified simulations.
#' @param em_dir The directory with the estimation model you want to copy and
#'   use for the specified simulations.
#' @template user_recdevs
#' @param user_recdevs_warn A logical argument allowing users to turn the
#'   warning regarding biased recruitment deviations off when \code{user_recdevs}
#'   are specified.
#' @param bias_adjust Run bias adjustment first? See \code{\link{run_bias_ss3}}.
#' @param bias_nsim If bias adjustment is run, how many simulations should the
#'   bias adjustment factor be estimated from? It will take the mean of the
#'   adjustment factors across these runs.
#' @param bias_already_run If you've already run the bias runs for a scenario
#'   (the bias folders and \code{.dat} files already exist) then you can set
#'   this to \code{TRUE} to avoid re-running the bias adjustment routine.
#' @param hess_always If \code{TRUE} then the Hessian will always be calculated.
#'   If \code{FALSE} then the Hessian will only be calculated for
#'   bias-adjustment runs thereby saving time.
#' @param print_logfile Logical. Print a log file?
#' @param sleep A time interval (in seconds) to pause on each iteration. Useful
#'   if you want to reduce average CPU time -- perhaps because you're working on
#'   a shared server.
#' @param conv_crit The maximum percentage of bias iterations that can produce a
#'   non-invertible Hessian before a warning will be produced. If this
#'   percentage is exceeded then a file \code{WARNINGS.txt} will be produced.
#'   Currently, the simulations will continue to run.
#' @param seed The seed value to pass to \code{\link{get_recdevs}} when
#'   generating recruitment deviations. The generated recruitment deviations
#'   depend on the iteration value, but also on the value of \code{seed}. A
#'   given combination of iteration, number of years, and \code{seed} value will
#'   result in the same recruitment deviations.
#' @param keep_compreport Logical: should the SS3 file \code{CompReport.sso} be
#'   kept or deleted? \code{CompReport.sso} is often rather large and so
#'   deleting it can save space but the file is needed for some of the \pkg{r4ss}
#'   plots among other purposes.
#' @param ... Anything extra to pass to \code{\link{run_ss3model}}. For
#' example, you may want to pass additional options to \code{SS3} through
#' the argument \code{admb_options}. Anything that doesn't match a named
#' argument in \code{\link{run_ss3model}} will be passed to the
#' \code{\link{system}} call that runs \code{SS3}.  Also, see the argument
#' \code{ss_mode} to choose between safe or optimized SS3 executables
#' (default is safe mode).
#' @author Sean Anderson with contributions from many others as listed in
#'   the DESCRIPTION file.
#' @importFrom r4ss SS_readdat
#' @return
#' The output will appear in whatever your current \R working directory
#' is. There will be folders named after your scenarios. They will
#' look like this:
#' \itemize{
#' \item \code{D0-F0-cod/bias/1/om}
#' \item \code{D0-F0-cod/bias/1/em}
#' \item \code{D0-F0-cod/bias/2/om}
#' \item ...
#' \item \code{D0-F0-cod/1/om}
#' \item \code{D0-F0-cod/1/em}
#' \item \code{D0-F0-cod/2/om}
#' \item ...
#' }
#'
# The input and output file structure of an \pkg{ss3sim} simulation:
#
# \figure{filestructure.png}
#'
#' @seealso \code{\link{run_ss3sim}}
#' @export
#' @details
#' This function is written to be flexible. You can specify the fishing
#' mortality, survey index, length composition, age composition, and
#' time-varying parameters in the function call as list objects (see the
#' example below). For a generic higher-level function, see
#' \code{\link{run_ss3sim}}.
#'
# The steps carried out within \code{ss3sim_base}:
#
# \figure{simsteps.png}
#'
#' @examples
#' \dontrun{
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-base-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#'
#' # Find the data in the ss3sim package:
#' d <- system.file("extdata", package = "ss3sim")
#' om <- paste0(d, "/models/cod-om")
#' em <- paste0(d, "/models/cod-em")
#' case_folder <- paste0(d, "/eg-cases")
#'
#' # Pull in file paths from the package example data:
#' d <- system.file("extdata", package = "ss3sim")
#' om_dir <- paste0(d, "/models/cod-om")
#' em_dir <- paste0(d, "/models/cod-em")
#' a <- get_caseargs(folder = paste0(d, "/eg-cases"), scenario =
#' "F0-D0-M0-E0-cod")
#'
#' ss3sim_base(iterations = 1, scenarios = "M0-F0-D0-E0-cod",
#'   f_params = a$F, index_params = a$index, lcomp_params = a$lcomp,
#'   agecomp_params = a$agecomp, tv_params = a$tv_params, estim_params = a$E,
#'   om_dir = om_dir, em_dir = em_dir)
#' unlink("M0-F0-D0-E0-cod", recursive = TRUE) # clean up
#'
#' # Or, create the argument lists directly in R and skip the case file setup:
#'
#' F0 <- list(years = 1913:2012, years_alter = 1913:2012, fvals = c(rep(0,
#'   25), rep(0.114, 75)))
#'
#' index1 <- list(fleets = 2, years = list(seq(1974, 2012, by = 2)), sds_obs =
#'   list(0.1))
#'
#' lcomp1 <- list(fleets = c(1, 2), Nsamp = list(100, 100), years =
#'   list(1938:2012, seq(1974, 2012, by = 2)), lengthbin_vector = NULL, cpar =
#'   c(1, 1))
#'
#' agecomp1 <- list(fleets = c(1, 2), Nsamp = list(100, 100), years =
#'   list(1938:2012, seq(1974, 2012, by = 2)), agebin_vector = NULL, cpar =
#'   c(1, 1))
#'
#' E0 <- list(natM_type = "1Parm", natM_n_breakpoints = NULL, natM_lorenzen =
#'   NULL, natM_val = c(NA,-1), par_name = "LnQ_base_3_CPUE", par_int = NA,
#'   par_phase = -1, forecast_num = 0)
#'
#' M0 <- list(NatM_p_1_Fem_GP_1 = rep(0, 100))
#'
#' ss3sim_base(iterations = 1, scenarios = "D1-E0-F0-M0-cod",
#'   f_params = F0, index_params = index1, lcomp_params = lcomp1,
#'   agecomp_params = agecomp1, estim_params = E0, tv_params = M0,
#'   om_dir = om, em_dir = em)
#'
#' unlink("D1-E0-F0-M0-cod", recursive = TRUE) # clean up
#'
#' setwd(wd)
#' }

ss3sim_base <- function(iterations, scenarios, f_params,
  index_params, lcomp_params, agecomp_params, calcomp_params = NULL,
  wtatage_params = NULL, mlacomp_params = NULL, em_binning_params = NULL,
  estim_params = NULL, tv_params = NULL, om_dir, em_dir,
  retro_params = NULL, data_params = NULL, call_change_data = TRUE,
  user_recdevs = NULL, user_recdevs_warn = TRUE, bias_adjust = FALSE,
  bias_nsim = 5, bias_already_run = FALSE, hess_always = FALSE,
  print_logfile = TRUE, sleep = 0, conv_crit = 0.2, seed = 21,
  keep_compreport = TRUE, ...) {

  # In case ss3sim_base is stopped before finishing:
  old_wd <- getwd()
  on.exit(setwd(old_wd))

  if(bias_already_run & bias_adjust){
      warning("bias_adjust set to FALSE because bias_already_run is TRUE")
      bias_adjust <- FALSE
  }


  # The first bias_nsim runs will be bias-adjustment runs
  if(bias_adjust) {
    iterations <- c(paste0("bias/", c(1:bias_nsim)), iterations)
  }

  for(sc in scenarios) {
    for(i in iterations) {

      # Create folders, copy models, check for necessary files, rename
      # files for consistency
      copy_ss3models(model_dir = om_dir, scenarios = sc,
        iterations = i, type = "om")
      iteration_existed <- copy_ss3models(model_dir = em_dir, scenarios = sc,
        iterations = i, type = "em")
      if(iteration_existed)
          next

      # Make fake .dat files to silence SS3/ADMB:
      fake_dat <- c("om/ss3_24o_opt.dat", "om/ss3_24o_safe.dat",
        "em/ss3_24o_opt.dat", "em/ss3_24o_safe.dat")
      sapply(fake_dat, function(fi) write("\n", pastef(pastef(sc, i, fi))))

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

      recdevs <- get_recdevs(iteration = this_run_num, n = 2000, seed = seed)
      if(is.null(user_recdevs)) {
        sc_i_recdevs <- sigmar * recdevs - sigmar^2/2 # from the package data
      } else {if(user_recdevs_warn & i == 1){
          warning(paste("No bias correction is done internally for user-supplied",
              "recruitment deviations and must be done manually. See the",
              "vignette for more details. Biased recruitment deviations can",
              "lead to biased model results."), .call = FALSE)
        }
        sc_i_recdevs <- user_recdevs[, this_run_num] # user specified recdevs
      }

      # Add new rec devs overwriting om/ss3.par
      change_rec_devs(recdevs_new = sc_i_recdevs, par_file_in =
        pastef(sc, i, "om", "ss3.par"), par_file_out = pastef(sc, i,
          "om", "ss3.par"))

      # Change F
      f_params <- add_nulls(f_params, c("years", "years_alter", "fvals"))
      with(f_params,
        change_f(years               = years,
                 years_alter         = years_alter,
                 fvals               = fvals,
                 par_file_in         = pastef(sc, i, "om", "ss3.par"),
                 par_file_out            = pastef(sc, i, "om", "ss3.par")))

      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)
      # Read in the data.ss_new file and write to ss3.dat in the om folder
      if(!file.exists(pastef(sc, i, "om", "data.ss_new")))
          stop(paste0("The data.ss_new not created in *first* OM run for ",
                     sc, "-",i, ": is something wrong with initial model files?"))
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
        data_out = pastef(sc, i, "om", "ss3.dat"))
      # Remove the ss_new file in case the next run doesn't work we can tell
      file.remove(pastef(sc, i, "om", "data.ss_new"))
      # Change time-varying parameters; e.g. M, selectivity, growth...
      wd <- getwd()
      if(!is.null(tv_params)) {
        setwd(pastef(sc, i, "om"))
        # not running add_null() b/c parameters in tv_params are userspecified
        with(tv_params,
          change_tv(change_tv_list      = tv_params,
                    ctl_file_in         = "om.ctl",
                    ctl_file_out        = "om.ctl"))
        setwd(wd)
      }

      # Change the data structure in the OM to produce the expected
      # values we want. This sets up the 'dummy' bins before we run
      # the OM one last time. Then we'll sample from the expected values
      # with error.
      sample_args <- list(lcomp_params, agecomp_params, calcomp_params,
        mlacomp_params)
      ## This returns a superset of all years/fleets/data types needed to
      ## do sampling.
      data_args <- calculate_data_units(lcomp_params    = lcomp_params,
                                        agecomp_params  = agecomp_params,
                                        calcomp_params  = calcomp_params,
                                        mlacomp_params  = mlacomp_params,
                                        wtatage_params  = wtatage_params)
      datfile.orig <- SS_readdat(pastef(sc, i, "om", "ss3.dat"),
                                 verbose = FALSE)
      datfile.orig <- change_fltname(datfile.orig)

      if (call_change_data) {
        # Start by clearing out the old data. Important so that extra data
        # doesn't trip up change_data:
        datfile.orig <- clean_data(dat_list = datfile.orig,
          index_params = index_params, verbose = FALSE)

        data_params <- add_nulls(data_params, c("age_bins", "len_bins",
          "pop_binwidth", "pop_minimum_size", "pop_maximum_size",
          "tail_compression", "lcomp_constant"))

        # Note some are data_args and some are data_params:
        change_data(dat_list         = datfile.orig,
                    outfile          = pastef(sc, i, "om", "ss3.dat"),
                    fleets           = data_args$fleets,
                    years            = data_args$years,
                    types            = data_args$types,
                    age_bins         = data_params$age_bins,
                    len_bins         = data_params$len_bins,
                    pop_binwidth     = data_params$pop_binwidth,
                    pop_minimum_size = data_params$pop_minimum_size,
                    pop_maximum_size = data_params$pop_maximum_size,
                    tail_compression = data_params$tail_compression,
                    lcomp_constant   = data_params$lcomp_constant,
                    write_file       = TRUE)
      }

      # Run the operating model and copy the dat file over
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)
      if(!file.exists(pastef(sc, i, "om", "data.ss_new")))
          stop(paste0("The data.ss_new not created in *second* OM run for ",
                     sc, "-",i, ": is something wrong with initial model files?"))
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
                            data_out = pastef(sc, i, "em", "ss3.dat"))
      ## Read in the datfile once and manipulate as a list object, then
      ## write it back to file at the end, before running the EM.
      dat_list <- SS_readdat(pastef(sc, i, "em", "ss3.dat"),
                            verbose = FALSE)
      dat_list <- change_fltname(dat_list)
      ## Survey biomass index
      index_params <- add_nulls(index_params, c("fleets", "years", "sds_obs"))

      dat_list <- with(index_params,
        sample_index(dat_list        = dat_list,
                     outfile         = NULL,
                     fleets          = fleets,
                     years           = years,
                     sds_obs         = sds_obs,
                     write_file      = FALSE))
      ## Add error in the length comp data
      if(!is.null(lcomp_params$fleets)){
          lcomp_params <- add_nulls(lcomp_params,
                     c("fleets", "Nsamp", "years", "cpar", "ESS"))
          dat_list <- with(lcomp_params,
               sample_lcomp(dat_list         = dat_list,
                            outfile          = NULL,
                            fleets           = fleets,
                            Nsamp            = Nsamp,
                            years            = years,
                            cpar             = cpar,
                            ESS              = ESS,
                            write_file       = FALSE))
      }

      ## Add error in the age comp data. Need to do this last since other
      ## sampling functions rely on the age data. Also, if user doesn't
      ## call this function we need to delete the data
      if(!is.null(agecomp_params$fleets)){
          agecomp_params <- add_nulls(agecomp_params,
                       c("fleets", "Nsamp", "years", "cpar", "ESS"))
          dat_list <- with(agecomp_params,
                          sample_agecomp(dat_list       = dat_list,
                                         outfile        = NULL,
                                         fleets         = fleets,
                                         Nsamp          = Nsamp,
                                         years          = years,
                                         cpar           = cpar,
                                         ESS            = ESS,
                                         write_file     = FALSE))
      }

      ## Add error in the empirical weight-at-age comp data. Note that if
      ## arguments are passed to this fucntion it's functionality is turned
      ## on by setting the maturity option to 5. If it's off SS will just
      ## ignore the wtatage.dat file so no need to turn it "off" like the
      ## other data.
      if(!is.null(wtatage_params)){
          wtatage_params <-
              add_nulls(wtatage_params, c("fleets", "Nsamp", "years", "cv_wtatage"))
          ## A value of NULL for fleets signifies not to use this function,
          ## so exit early if this is the case.
          if(!is.null(wtatage_params$fleets)){
              ## Make sure W@A option is turned on in the EM
              change_maturity(ctl_file_in=pastef(sc, i, "em", "em.ctl"),
                              ctl_file_out=pastef(sc, i, "em", "em.ctl"),
                              maturity_option=5)
              with(wtatage_params,
                   sample_wtatage(wta_file_in = pastef(sc, i, "om", "wtatage.ss_new"),
                                  outfile     = pastef(sc, i, "em", "wtatage.ss"),
                                  dat_list    = dat_list,
                                  ctl_file_in = pastef(sc, i, "om", "control.ss_new"),
                                  fleets      = fleets,
                                  years       = years,
                                  write_file  = TRUE,
                                  cv_wtatage  = cv_wtatage))
          }
      }

      ## Add error in the mean length-at-age comp data. This sampling
      ## function needs full age data so needs to be done before that
      ## sampling function is called. Also, if this function isn't called
      ## we need to delete that data, so I'm doing that based on whether it
      ## is NULL, so it always needs to be called.
      if(!is.null(mlacomp_params$fleets)){
          mlacomp_params <- add_nulls(mlacomp_params, c("fleets", "Nsamp", "years", "mean_outfile"))
          dat_list <- with(mlacomp_params,
                          sample_mlacomp(dat_list       = dat_list,
                                         outfile        = NULL,
                                         ctl_file_in    = pastef(sc, i, "om", "control.ss_new"),
                                         fleets         = fleets,
                                         Nsamp          = Nsamp,
                                         years          = years,
                                         mean_outfile   = pastef(sc, i, "em",
                                                                 paste0(mean_outfile, ".csv")),
                                         write_file     = FALSE))
      }

      ## Add error in the conditional age at length comp data. The
      ## cal data are independent of the agecomp data for this
      ## package. Thus the sampling of agecomps has no influence on the
      ## calcomp data and vice versa.
      if(!is.null(calcomp_params$fleets)){
          calcomp_params <- add_nulls(calcomp_params, c("fleets", "years", "Nsamp"))
          dat_list <- with(calcomp_params,
                          sample_calcomp(dat_list         = dat_list,
                                         outfile          = NULL,
                                         fleets           = fleets,
                                         years            = years,
                                         Nsamp            = Nsamp,
                                         write_file       = FALSE))
      }
      ## Manipulate EM starter file for a possible retrospective analysis
      if(!is.null(retro_params)) {
      retro_params <- add_nulls(retro_params, "retro_yr")
      with(retro_params,
        change_retro(str_file_in    = pastef(sc, i, "em", "starter.ss"),
                     str_file_out   = pastef(sc, i, "em", "starter.ss"),
                     retro_yr        = retro_yr))
      }

      ## End of manipulating the data file, so clean it and write it
      dat_list <- clean_data(dat_list      = dat_list,
                            index_params   = index_params,
                            lcomp_params   = lcomp_params,
                            agecomp_params = agecomp_params,
                            calcomp_params = calcomp_params,
                            mlacomp_params = mlacomp_params,
                            verbose        = FALSE)

	  ## Now change the binning structure in the EM ss3.dat file as needed
      if (!is.null(em_binning_params$lbin_method)) {
          em_binning_params <- add_nulls(em_binning_params,
            c("lbin_method", "bin_vector", "pop_binwidth",
              "pop_minimum_size", "pop_maximum_size"))
          dat_list <- change_em_binning(
              dat_list         = dat_list,
              dat_file_out     = NULL,
              bin_vector       = em_binning_params$bin_vector,
              lbin_method      = em_binning_params$lbin_method,
              pop_binwidth     = em_binning_params$pop_binwidth,
              pop_minimum_size = em_binning_params$pop_minimum_size,
              pop_maximum_size = em_binning_params$pop_maximum_size,
              write_file       = FALSE)
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

      if(!is.null(estim_params)) {
        setwd(pastef(sc, i, "em"))
        estim_params <- add_nulls(estim_params,
          c("natM_type", "natM_n_breakpoints", "natM_lorenzen", "natM_val",
            "par_name", "par_int", "par_phase", "forecast_num"))
        dat_list <- with(estim_params,
         change_e(ctl_file_in          = "em.ctl",
                  ctl_file_out         = "em.ctl",
                  dat_list             = dat_list,
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
      }

      SS_writedat(datlist = dat_list, outfile = pastef(sc, i, "em", "ss3.dat"),
        overwrite = TRUE, verbose = FALSE)

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
        cat("\n\n# change_tv arguments\n")
        print(tv_params)
        cat("\n\n# change_f arguments\n")
        print(f_params)
        cat("\n\n# sample_index arguments\n")
        print(index_params)
        cat("\n\n# sample_lcomp arguments\n")
        print(lcomp_params)
        cat("\n\n# sample_agecomp arguments\n")
        print(agecomp_params)
        cat("\n\n# sample_calcomp arguments\n")
        print(calcomp_params)
        cat("\n\n# sample_wtatage arguments\n")
        print(wtatage_params)
        cat("\n\n# sample_mlacomp arguments\n")
        print(mlacomp_params)
        cat("\n\n# tail compression arguments\n")
        print(data_params)
        cat("\n\n# change_em_lbin_params arguments\n")
        print(em_binning_params)
        cat("\n\n# change_data arguments\n")
        print(agecomp_params)
        cat("\n\n# chante_retro arguments\n")
        print(retro_params)
        cat("\n\n# call_change_data?\n")
        print(call_change_data)
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

      file.remove(pastef(sc, i, fake_dat))
      if (!keep_compreport) {
        file.remove(pastef(sc, i, "om", "CompReport.sso"))
        file.remove(pastef(sc, i, "em", "CompReport.sso"))
      }
      #  Pause to reduce average CPUE use?
      Sys.sleep(sleep)

    } # end iterations
  } # end scenarios
}
