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
#' @param a_params A named list containing arguments for
#'   \code{\link{change_a}}, to define ageing error.
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
#' @param p_params A named list containing arguments for
#'   \code{\link{change_p}}, for changing selectivity parameters.
#' @param data_params A named list containing arguments for
#'   \code{\link{change_data}}.
#' @param call_change_data A boolean of whether to call
#'   \code{\link{change_data}} and modify the OM at each iteration. Defaults to
#'   \code{TRUE}. See the vignette for further information on why you might
#'   choose to turn this off.
#' @param sex_misspec_params A named list containing arguments for
#'   \code{\link{change_x}}, for mis-specifying sex in EM if OM is sex-specific.
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
#'   the DESCRIPTION file; Gwladys Lambert added extra options that are not in the master version of the package
#' @importFrom r4ss SS_readdat
#' @importFrom methods is
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
#'
#'  # Example to use ageing error, sex mispecification and changing selectivity
#'
#' A0 <- list()
#' X0 <- list()
#' P0 <- list()
#'
#' }

ss3sim_base <- function(iterations, scenarios, f_params,
  index_params, lcomp_params, agecomp_params, calcomp_params = NULL, a_params = NULL,  ### ADDED AGERR
  wtatage_params = NULL, mlacomp_params = NULL, em_binning_params = NULL, p_params =NULL, ### ADDED SELEX SPEC/MIS-SPEC
  estim_params = NULL, tv_params = NULL, om_dir, em_dir,
  retro_params = NULL, data_params = NULL, call_change_data = TRUE, sex_misspec_params = NULL, ### ADDED MISSPEC FOR SEXES
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
      Sys.sleep(0.5)
      # Read in the data.ss_new file and write to ss3.dat in the om folder
      if(!file.exists(pastef(sc, i, "om", "data.ss_new")))
          stop(paste0("The data.ss_new not created in *first* OM run for ",
                     sc, "-",i, ": is something wrong with initial model files?"))
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
        data_out = pastef(sc, i, "om", "ss3.dat"))
      # Remove the ss_new file in case the next run doesn't work we can tell
      file.remove(pastef(sc, i, "om", "data.ss_new"))



      ##------------------------------------------------------------------##
      ##                 Changeing selectivity in both om and em          ##
      ##------------------------------------------------------------------##

      # Change parameters of selectivity - choose age-based or length-based (both om and em), dome-shaped or flat-top for em and em separately and if the model is sex-based, choose pars for each sex
      run_change_p_full <- FALSE # default
      if(grepl("bias", i))  # it's a bias run
        run_change_p_full <- TRUE
      if(!bias_adjust)      # we aren't running bias adjustment
        run_change_p_full <- TRUE


      wd <- getwd()
      #### NEED TO add an option to say not to run the change_p for em if it bias has already been run since changes will have already been made???
      if (!is.null(p_params)){
        setwd(pastef(wd, sc, i))
        p_params <- add_nulls(p_params, c("selex_om","selex_em","fleet","pars_om_selex","pars_em_selex","plot_selex_om","plot_selex_em","sex_flag","om_sex_offset","em_sex_offset"))
        with(p_params,
             change_p(om_dir               = "om",
                      om_ctl_file_in          = "om.ctl",
                      om_ctl_file_out         = "om.ctl",
                      par_file_in          = "ss3.par",
                      par_file_out         = "ss3.par",
                      em_dir               = "em",
                      em_ctl_file_in          = "em.ctl",
                      em_ctl_file_out         = "em.ctl",
                      selex_om               = selex_om,
                      selex_em               = selex_em,
                      fleet               = fleet,
                      sex_flag            = sex_flag,
                      pars_om_selex       = pars_om_selex,
                      pars_em_selex       = pars_em_selex,
                      om_sex_offset       = om_sex_offset,
                      em_sex_offset       = em_sex_offset,
                      plot_selex_om       = plot_selex_om,
                      plot_selex_em       = plot_selex_em
             ))
        setwd(wd)
      }


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
      ## For the purpose of the simulation - if calcomp in there, make sure to create dummy data for agecomp too
      if (!is.null(calcomp_params))  data_args$types <- unique(c(data_args$types, "age"))

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

        # to save having to copy the data file twice, as taking a very long time...
        write_file_flag <- TRUE
        if (!is.null(agecomp_params) | !is.null(calcomp_params) ) {
          if(!is.null(a_params)) {write_file_flag <- FALSE}
        }

        # Note some are data_args and some are data_params:
        datfile.orig <- change_data(dat_list         = datfile.orig,
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
                    write_file       = write_file_flag)
      }



      ##------------------------------------------------------------------##
      ## Add ageing error in the expected age data, before sampling error! #
      ##------------------------------------------------------------------##
      ### Include ageing error in the dummy data
      ### For each line where a data matrix has been defined - include the agerr # in
      ### If no ageing error has been defined arbitrarily use matrix 1 (i.e. no ageing error)

      # Change A - i.e. add and change ageing error matrices
      # datfile.orig <- SS_readdat(pastef(sc, i, "om", "ss3.dat"),
      #                             verbose = FALSE)
      if (!is.null(agecomp_params) | !is.null(calcomp_params) ) {
        if(!is.null(a_params)) {
          a_params <- add_nulls(a_params, c("df","agerr_mat","mis_spec"))
          if(!is.null(a_params$df)){
            dat_list <- with(a_params,
                             change_a(dat_list     = datfile.orig,
                                      outfile      = pastef(sc, i, "om", "ss3.dat"),
                                      df           = df,
                                      agerr_mat    = agerr_mat,
                                      # mis_spec     = mis_spec,
                                      write_file   = FALSE))
          }
          # trick to slow down process to avoid error message with over-writing
          try_write <- NULL
          try_write <- try(fast_SS_writedat(datlist = dat_list, outfile = pastef(sc, i, "om", "ss3.dat"), overwrite = TRUE, verbose = FALSE), silent=T)
          if (is(try_write)[1]=="try-error") {
            while (is(try_write)[1]=="try-error"){
              try_write <- try(fast_SS_writedat(datlist = dat_list, outfile = pastef(sc, i, "om", "ss3.dat"), overwrite = TRUE, verbose = FALSE), silent=T)
            }
          }
        }
      }

      ######################################################################################
      ######################################################################################

      setwd(wd)
      # Run the operating model and copy the dat file over
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)
      Sys.sleep(0.5)

      if(!file.exists(pastef(sc, i, "om", "data.ss_new")))
          stop(paste0("The data.ss_new not created in *second* OM run for ",
                     sc, "-",i, ": is something wrong with initial model files?"))

      try_write <- try(extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
                            data_out = pastef(sc, i, "em", "ss3.dat")), silent= T)
        if (is(try_write)[1]=="try-error") {
        while (is(try_write)[1]=="try-error"){
          try_write <- try(extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
                                                 data_out = pastef(sc, i, "em", "ss3.dat")), silent=T)
        }
      }

	     # this removes the files created in Linux runs
if (file.exists(pastef(getwd(),sc, i, "om","ss3_24o_safe"))) file.remove(pastef(getwd(), sc, i, "om","ss3_24o_safe"))
if (file.exists(pastef(getwd(),sc, i, "om","ss3_24o_opt"))) file.remove(pastef(getwd(), sc, i, "om","ss3_24o_opt"))
	     
	     
      ##---------------------------------------------------------------------------------------##
      ##                            Change tv for em model too                                 ##
      ##---------------------------------------------------------------------------------------##

      # Change time-varying parameters; e.g. M, selectivity, growth...
      wd <- getwd()
        if(!is.null(tv_params)) {
          if (any(as.character(tv_params$models) =="em")) {
            setwd(pastef(sc, i, "em"))
            # not running add_null() b/c parameters in tv_params are userspecified
            with(tv_params,
                 change_tv(change_tv_list      = tv_params,
                           ctl_file_in         = "em.ctl",
                           ctl_file_out        = "em.ctl"))
            setwd(wd)
          }
      }


      ##---------------------------------------------------------------------------------------##
      ## Mis-specifying sex in the Estimation Model by removing growth and sectivity dimorphism #
      ##---------------------------------------------------------------------------------------##

      ### THIS IS TO MERGE 2 SEXES BEFORE SAMPLING SO EM WILL NOT HAVE DIMORPHIC GROWTH

      ## Read in the datfile once and manipulate as a list object, then
      ## write it back to file at the end, before running the EM.
      #setwd(wd)

      dat_list <- try(SS_readdat(pastef(sc, i, "em", "ss3.dat"),verbose = FALSE), silent=T)
      if (is(dat_list)[1] == "try-error") {
        while(is(dat_list)[1] == "try-error") {
            dat_list <- try(SS_readdat(pastef(sc, i, "em", "ss3.dat"),verbose = FALSE), silent=T)
        }
      }

      dat_list <- change_fltname(dat_list)


      run_change_x_full <- FALSE # default
      if(grepl("bias", i))  # it's a bias run
        run_change_x_full <- TRUE
      if(!bias_adjust)      # we aren't running bias adjustment
        run_change_x_full <- TRUE
      ###################################### NEED TO ADD A CONDITION HERE TOO ##########################

      if(!is.null(sex_misspec_params)) {
        sex_misspec_params <- add_nulls(sex_misspec_params, c("change_dat","change_ctl"))
        dat_list <- with(sex_misspec_params,
                         change_x(dat_file_in = pastef(sc, i, "em", "ss3.dat"),
                                  dat_file_out = pastef(sc, i, "em", "ss3.dat"),
                                  ctl_file_in = pastef(sc, i, "em", "em.ctl"),
                                  ctl_file_out = pastef(sc, i, "em", "em.ctl"),
                                  change_dat= change_dat,
                                  change_ctl = change_ctl,
                                  write_file=FALSE))
        }

      ######################################################################################
      ######################################################################################

      ##-----------------------------------------------------------------------------------## ########################################### NEED TO ADD CHANGE OF AGEING ERROR IN MLA
      ## Run again the ageing error option in case the em model requires mis-specification ##
      ##-----------------------------------------------------------------------------------##

      if (!is.null(agecomp_params) | !is.null(calcomp_params) ) {
        if(!is.null(a_params)) {
          a_params <- add_nulls(a_params, c("df","agerr_mat","mis_spec"))
          if(!is.null(a_params$mis_spec)){
            dat_list <- with(a_params,
                             change_a(dat_list     = dat_list,
                                      outfile      = pastef(sc, i, "em", "ss3.dat"),
                                      df           = df,
                                      agerr_mat    = agerr_mat,
                                      mis_spec     = mis_spec,
                                      write_file   = FALSE))
          }
         }
      }

      ######################################################################################
      ######################################################################################
      message(paste0("Running sampling for scenario: ", sc, "; iteration: ", i))

      message(paste0(".... Index"))

      ## Survey biomass index
      index_params <- add_nulls(index_params, c("fleets", "years", "sds_obs"))

      dat_list <- with(index_params,
        sample_index(dat_list        = dat_list,
                     outfile         = NULL, #pastef(sc, i, "em", "ss3.dat")
                     fleets          = fleets,
                     years           = years,
                     sds_obs         = sds_obs,
                     write_file      = FALSE))



      ## Add error in the length comp data

      if(!is.null(lcomp_params$fleets)){
        message(paste0(".... Length"))
        hauls <- NULL
        wd <- getwd()
        setwd(pastef(sc, i))
        lcomp_params <- add_nulls(lcomp_params,
                                  c("fleets", "Nsamp", "years","Nhauls", "cl_factor","vals_at_cutoff","cpar", "plot.schools",
                                    "ESS", "change_ess_now","parallel_lgths", "empirical_aggs")) #, "effNsamp""cutoff",
        if (is.null(lcomp_params$parallel_lgths)) lcomp_params$parallel_lgths <- FALSE
        if (is.null(lcomp_params$plot.schools)) lcomp_params$plot.schools   <- FALSE
        dat_list_temp <- with(lcomp_params,
                              sample_lcomp(dat_list         = dat_list,
                                           outfile          = pastef(sc, i, "em", "ss3.dat"),
                                           fleets           = fleets,
                                           Nsamp            = Nsamp,
                                           years            = years,
                                           Nhauls           = Nhauls,
                                           cl_factor        = cl_factor,
                                           vals_at_cutoff   = vals_at_cutoff,
                                           empirical_aggs   = empirical_aggs,
                                           cpar             = cpar,
                                           ESS              = ESS,
                                           change_ess_now   = change_ess_now,
                                           write_file       = FALSE,
                                           plot.schools     = plot.schools,
                                           parallel_lgths   = parallel_lgths#,
                                            ))
        if (length(dat_list_temp)==2) {
          hauls    <- dat_list_temp[[2]]
          write.table(hauls,"hauls.txt", row.names=F, sep="\t")
          dat_list <- dat_list_temp[[1]]
        } else {
          dat_list <- dat_list_temp
        }
        setwd(wd)
      }

      lcomp_params_print <- lcomp_params
      lcomp_params_print$Nsamp <- as.list(unique(dat_list$lencomp$Nsamp))

      ## Add error in the age comp data. Need to do this last since other
      ## sampling functions rely on the age data. Also, if user doesn't
      ## call this function we need to delete the data

      if(!is.null(agecomp_params$fleets)){
        message(paste0(".... Ages"))
        agecomp_params <- add_nulls(agecomp_params,
                       c("fleets", "Nsamp", "years", "cpar", "ESS"))
          dat_list <- with(agecomp_params,
                          sample_agecomp(dat_list       = dat_list,
                                         outfile        = pastef(sc, i, "em", "ss3.dat"),
                                         fleets         = fleets,
                                         Nsamp          = Nsamp,
                                         years          = years,
                                         cpar           = cpar,
                                         ESS            = ESS,
                                         write_file     = FALSE))
      }


      if(!is.null(calcomp_params$fleets)){
        message(paste0(".... Ages"))
        hauls_temp <- NULL
        pop_agecompa <- NULL
        if (!is.null(hauls)) hauls_temp <- hauls
        if (!is.null(lcomp_params$ESS)) calcomp_params$Nsamp.len <- lcomp_params$Nsamp
        calcomp_params <- add_nulls(calcomp_params, c("fleets", "years", "Nsamp", "Nhauls","fixed.number",
                                                      "fit.on.agecomp","method", "ageplus", "ctl","rw",
                                                      "ESS", "change_ess_now","percent_read", "max_ss")) ####################""Nsamp.len",ESS",  "add.MLA",ADDING AGE PLUS GROUP IN HERE
        if (!is.null(calcomp_params$fit.on.agecomp) & is.null(wtatage_params)) calcomp_params$ctl <- pastef(sc, i, "em", "em.ctl")
        dat_list_temp <- with(calcomp_params,
                              sample_calcomp(dat_list         = dat_list,
                                             outfile          = pastef(sc, i, "em", "ss3.dat"),
                                             fleets           = fleets,
                                             years            = years,
                                             Nsamp            = Nsamp,
                                             Nhauls           = Nhauls,
                                             percent_read     = percent_read,
                                             max_ss     = max_ss,
                                             fixed.number     = fixed.number,
                                           #  Nsamp.len        = Nsamp.len,
                                             fit.on.agecomp   = fit.on.agecomp,
                                          #   add.MLA          = add.MLA,
                                             write_file       = FALSE,
                                             hauls            = hauls_temp,
                                             method           = method,
                                             ESS              = ESS,
                                             change_ess_now   = change_ess_now,
                                          #   effNsamp         = effNsamp,
                                             ageplus          = ageplus,
                                             ctl              = ctl))

         if (length(dat_list_temp)==2) {
           pop_agecompa <- dat_list_temp[[2]]
           dat_list <- dat_list_temp[[1]]
         } else {
           dat_list <- dat_list_temp
         }
      }

      calcomp_params_print <- calcomp_params
      calcomp_params_print$Nsamp <- dat_list$agecomp[dat_list$agecomp$Lbin_lo==-1,c(1,3,4,6,9)]

      ## Add error in the mean length-at-age comp data. This sampling
      ## function needs full age data so needs to be done before that
      ## sampling function is called. Also, if this function isn't called
      ## we need to delete that data, so I'm doing that based on whether it
      ## is NULL, so it always needs to be called.
      if(!is.null(mlacomp_params$fleets)){
        message(paste0(".... Mean Length At Age"))
        pop_agecompb <-  NULL
        mlacomp_params <- add_nulls(mlacomp_params, c("fleets", "Nsamp", "years", "ageplus", "mean_outfile"))
        dat_list_temp <- with(mlacomp_params,
                         sample_mlacomp(dat_list       = dat_list,
                                        outfile        = pastef(sc, i, "em", "ss3.dat"),
                                        ctl_file_in    = pastef(sc, i, "om", "control.ss_new"),
                                        fleets         = fleets,
                                        Nsamp          = Nsamp,
                                        years          = years,
                                        mean_outfile   = pastef(sc, i, "em",
                                                                paste0(mean_outfile, ".csv")),
                                        ageplus        = ageplus,
                                        pop_agecomp    = pop_agecompa,
                                        write_file     = FALSE))
        if (length(dat_list_temp)==2) {
          dat_list <- dat_list_temp[[1]]
          pop_agecompb <- dat_list_temp[[2]]
        } else {
          dat_list <- dat_list_temp
        }

      }


      ## Add error in the empirical weight-at-age comp data. Note that if
      ## arguments are passed to this fucntion it's functionality is turned
      ## on by setting the maturity option to 5. If it's off SS will just
      ## ignore the wtatage.dat file so no need to turn it "off" like the
      ## other data.
        lcomp_params_save <- lcomp_params
        if(!is.null(wtatage_params)){
        message(paste0(".... Weight At Age"))
        wtatage_params <-
          add_nulls(wtatage_params, c("fleets", "Nsamp", "years", "cv_wtatage"))
        ## A value of NULL for fleets signifies not to use this function,
        ## so exit early if this is the case.
        if(!is.null(wtatage_params$fleets)){
          ## Make sure W@A option is turned on in the EM
          change_maturity(ctl_file_in=pastef(sc, i, "em", "em.ctl"),
                          ctl_file_out=pastef(sc, i, "em", "em.ctl"),
                          maturity_option=5)
          # Rmve length dat ##################################################################################### ADDED IN
          dat_list$N_lencomp <- 0
          dat_list$lencomp <- NULL
          lcomp_params <- NULL
          pop_agecomp <- list()
          pop_agecomp[[1]] <- pop_agecompa[[1]] # pop age comp
          pop_agecomp[[2]] <- pop_agecompb      # pop mla
          pop_agecomp[[3]] <- pop_agecompa[[2]] # pop ages
          if (length(pop_agecomp) == 0) pop_agecomp = NULL
          with(wtatage_params,
               sample_wtatage(wta_file_in = pastef(sc, i, "om", "wtatage.ss_new"),
                              outfile     = pastef(sc, i, "em", "wtatage.ss"),
                              dat_list    = dat_list,
                              pop_agecomp  = pop_agecomp,
                              ctl_file_in = pastef(sc, i, "om", "control.ss_new"),
                              fleets      = fleets,
                              years       = years,
                              write_file  = TRUE,
                              cv_wtatage  = cv_wtatage))
        }

        dat_list$N_MeanSize_at_Age_obs <- 0
        dat_list$MeanSize_at_Age_obs <- NULL
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

      ########## THIS IS WHERE WE CHANGE THE ESS
      dat_list=change_ESS(dat_list, lcomp_params, calcomp_params)

      setwd(wd)

      # trick to slow down process to avoid error message with over-writing
      try_write <- NULL
      try_write <- try(fast_SS_writedat(datlist = dat_list, outfile = pastef(sc, i, "em", "ss3.dat"), overwrite = TRUE, verbose = FALSE), silent=T)
      if (is(try_write)[1]=="try-error") {
        while (is(try_write)[1]=="try-error"){
          try_write <- try(fast_SS_writedat(datlist = dat_list, outfile = pastef(sc, i, "em", "ss3.dat"), overwrite = TRUE, verbose = FALSE), silent=T)
        }
      }

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
      Sys.sleep(0.5)
										
# remove files created in Linux runs
if (file.exists(pastef(getwd(),sc, i, "em","ss3_24o_safe"))) file.remove(pastef(getwd(),sc, i, "em","ss3_24o_safe"))
if (file.exists(pastef(getwd(),sc, i, "em","ss3_24o_opt"))) file.remove(pastef(getwd(),sc, i, "em","ss3_24o_opt"))
										 
										 

      ##-----------------------------------------------------------------------------------##
      ##                  iterative re-weighting                                           ##
      ##-----------------------------------------------------------------------------------##

      ############### CHECK THIS IS THE CORRECT WAY TO RE-WEIGHT THE DATA + ADD OPTION TO CHOOSE FRANCIS VS MC&I ##############
      ############### AT THE MOMOENT THE RE-WEIGHTING WILL ONLY HAPPEN BASED ON AN ARGUMENT IN CALCOMP_PARAMS    ##############
      ############### AND INDEX, LENGTH AND AGE WILL BE REWEIGHTED - WHICH IS NOT GREAT, NEED TO INCLUDE SEPARATE OPTIONS #####
      #########################################################################################################################

      if(bias_already_run) {
        if (!is.null(calcomp_params$rw) ){ #&& calcomp_params$rw=="iterative"
          # first copy entire folder into em_adjeffN
        #  wd <- getwd()
          model_dir <- pastef(sc, i, type = "em") ###get directory right
          from <- pastef(model_dir)
          to <- pastef(sc, i, 'em_adjeffN')
          create.sim.folder(from, to)

          # find new weights, replace and re-run
          # Get correction factors
          MyOutput = SS_output(dir = to,readwt = F, covar=FALSE, ncols=300)

          #Get info on McAllister Ianelli weights:
          McIanelli_Length <-MyOutput$Length_comp_Eff_N_tuning_check
          McIanelli_Length <- McIanelli_Length[order(McIanelli_Length$Fleet),]
          McIanelli_Age <-MyOutput$Age_comp_Eff_N_tuning_check
          McIanelli_Age <- McIanelli_Age[order(McIanelli_Age$Fleet),]
          val_age <- round(McIanelli_Age$'HarEffN/MeanInputN'* McIanelli_Age$Var_Adj,2)
          val_length <- round(McIanelli_Length$'HarEffN/MeanInputN' * McIanelli_Length$Var_Adj,2)

          # read the var adjust section of code and check that it is turned on
          ctl_temp <- readLines(pastef(sc, i,'em_adjeffN',"control.ss_new"))
          idx_var <- grep("_Variance_adjustments_to_input_values",ctl_temp)
          var_adj_line <- ctl_temp[idx_var]
          var_adj_line_split <- strsplit(var_adj_line," ")[[1]]
          if (as.numeric(var_adj_line_split[1])==0){
            ctl_temp[idx_var] <- paste(1, var_adj_line_split[2])
            new_table <- as.data.frame(matrix(0, ncol=length(MyOutput$fleet_ID), nrow=6))
		  
            new_table[4:6,] <- 1
            var_adj_vals_lines <- ctl_temp[c(idx_var+2):c(idx_var+7)]
            var_adj_vals_split <- strsplit(var_adj_vals_lines,"#")#[[1]]
            var_adj_vals_comments <- paste("#", sapply(var_adj_vals_split,"[",3),sep="")
            new_var_adj_vals <- cbind(new_table, var_adj_vals_comments)
            new_var_adj_vals <- apply(new_var_adj_vals, 1, FUN=function(x) paste(x, collapse=" "))
            ctl_temp[c(idx_var+2):c(idx_var+7)] <- new_var_adj_vals
            writeLines(ctl_temp, con=pastef(sc, i,'em_adjeffN',"control.ss_new"))
          }

          # Replace in ctl file ###

          new_table <- as.data.frame(matrix(0, ncol=length(MyOutput$fleet_ID), nrow=6))
          names(new_table) <- dat_list$fleetnames
          new_table[5,] <- 1
          new_table[4,] <- 1
          new_table[5,names(new_table) %in% c(McIanelli_Age$FleetName)] <- c(val_age)
          new_table[4,names(new_table) %in% c(McIanelli_Length$FleetName)] <- c(val_length)
          new_table[6,] <- 1

          SS_varadjust2(dir = to, ctlfile = "control.ss_new",
                        newctlfile = "control_modified.ss", keyword = "Variance_adjustments",
                        newtable = new_table, newrow = NULL, rownumber = NULL, maxcols = 1000,
                        overwrite = T, verbose = FALSE)

          #state what old_SS_dat_filename and old_SS_ctl_filename are!!!
          old_SS_dat_filename <- "ss3"
          old_SS_ctl_filename <- "em"
          file.rename(paste(to,"data.ss_new", sep="\\"), paste(to,paste(old_SS_dat_filename,".dat", sep=""), sep="\\"))
          file.rename(paste(to,"forecast.ss_new", sep="\\"), paste(to,"Forecast.ss", sep="\\"))
          file.rename(paste(to,"starter.ss_new", sep="\\"), paste(to,"Starter.ss", sep="\\"))
          file.rename(paste(to,"control_modified.ss", sep="\\"), paste(to,paste(old_SS_ctl_filename,".ctl", sep=""), sep="\\"))

          # Re-run

          run_ss3model(scenarios = sc, iterations = i, type = "em_adjeffN",
                       hess = hess, ...)
          Sys.sleep(0.5)
	# remove files created in Linux runs
				
          setwd(wd)
        }
      }


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


      lcomp_params <- lcomp_params_save

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
        print(lcomp_params_print) ################################# NEED TO MAKE THIS COMPATIBLE
        cat("\n\n# sample_agecomp arguments\n")
        print(agecomp_params)
        cat("\n\n# sample_calcomp arguments\n")
        print(calcomp_params_print) ################################# NEED TO MAKE THIS COMPATIBLE
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


