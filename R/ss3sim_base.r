#' Base wrapper function to run an ss3sim simulation
#'
#' This function is a wrapper function that can call \code{\link{run_ss3model}}
#' for the operating model, sample the output (add recruitment deviations,
#' survey the data, etc.), and run the estimation model. \code{ss3sim_base} is
#' the main internal function for \pkg{ss3sim}. It is intended to be used
#' through \code{\link{run_ss3sim}}, but can also be used directly.
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios A name to use as the folder name for the unique combination
#'   of parameters for the OM and EM.
#' @param tv_params A named list containing arguments for
#'   \code{\link{change_tv}} (time-varying).
#' @param operat_params A named list containing arguments for \code{\link{change_o}}.
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
#' @param retro_params A named list containing the arguments for
#'   \code{\link{change_retro}}.
#' @param estim_params A named list containing arguments for
#'   \code{\link{change_e}}.
#' @param em_binning_params A named list containing arguments for
#'   \code{\link{change_em_binning}}.
#' @param data_params A named list containing arguments for
#'   \code{\link{change_data}}.
#' @param weight_comps_params A named list containing arguments for
#'   \code{\link{weight_comps}}.
#' @param om_dir The directory with the operating model you want to copy and use
#'   for the specified simulations.
#' @param em_dir The directory with the estimation model you want to copy and
#'   use for the specified simulations.
#' @template user_recdevs
#' @param user_recdevs_warn A logical argument allowing users to turn the
#'   warning regarding biased recruitment deviations off when \code{user_recdevs}
#'   are specified.
#' @param bias_adjust Run bias adjustment first?.
#' @param hess_always If \code{TRUE} then the Hessian will always be calculated.
#'   If \code{FALSE} then the Hessian will only be calculated for
#'   bias-adjustment runs thereby saving time.
#' @param print_logfile Logical. Print a log file?
#' @param sleep A time interval (in seconds) to pause on each iteration. Useful
#'   if you want to reduce average CPU time -- perhaps because you're working on
#'   a shared server.
#' @param seed The seed value to pass to \code{\link{get_recdevs}} when
#'   generating recruitment deviations. The generated recruitment deviations
#'   depend on the iteration value, but also on the value of \code{seed}. A
#'   given combination of iteration, number of years, and \code{seed} value will
#'   result in the same recruitment deviations.
#' @param ... Anything extra to pass to \code{\link{run_ss3model}}. For
#' example, you may want to pass additional options to \code{SS3} through
#' the argument \code{admb_options}. Anything that doesn't match a named
#' argument in \code{\link{run_ss3model}} will be passed to the
#' \code{\link{system}} call that runs \code{SS3}.
#' @author Sean Anderson with contributions from many others as listed in
#'   the DESCRIPTION file.
#' @import r4ss
#' @importFrom stats setNames
#' @return
#' The output will appear in whatever your current \R working directory
#' is. There will be folders named after your scenarios. They will
#' look like this:
#' \itemize{
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
#'   # Create a temporary folder for the output and set the working directory:
#'   temp_path <- file.path(tempdir(), "ss3sim-base-example")
#'   dir.create(temp_path, showWarnings = FALSE)
#'   wd <- getwd()
#'   setwd(temp_path)
#'   on.exit(setwd(wd), add = TRUE)
#'
#'   # Find the data in the ss3sim package:
#'   d <- system.file("extdata", package = "ss3sim")
#'   om_dir <- file.path(d, "models", "cod-om")
#'   em_dir <- file.path(d, "models", "cod-em")
#'
#'   # Or, create the argument lists directly in R and skip the case file setup:
#'
#'   F0 <- list(years = 1:100,
#'              fleets = 1,
#'              fvals = c(rep(0, 25), rep(0.114, 75)))
#'
#'   index1 <- list(fleets = 2, years = list(seq(62, 100, by = 2)),
#'                  sds_obs = list(0.1))
#'
#'   lcomp1 <- list(fleets = c(1, 2), Nsamp = list(50, 100),
#'                  years = list(26:100, seq(62, 100, by = 2)),
#'                  lengthbin_vector = NULL)
#'
#'   agecomp1 <- list(fleets = c(1, 2), Nsamp = list(50, 100),
#'                    years = list(26:100, seq(62, 100, by = 2)),
#'                    agebin_vector = NULL)
#'
#'   E0 <- list(par_name = c("LnQ_base_Fishery", "NatM_p_1_Fem_GP_1"),
#'              par_int = c(NA, NA), par_phase = c(-5, -1), forecast_num = 0)
#'
#'   ss3sim_base(iterations = 1,
#'               scenarios = "D1-E0-F0-cod", #name as desired
#'               f_params = F0,
#'               index_params = index1,
#'               lcomp_params = lcomp1,
#'               agecomp_params = agecomp1,
#'               estim_params = E0,
#'               om_dir = om_dir,
#'               em_dir = em_dir)
#'
#'   unlink("D1-E0-F0-cod", recursive = TRUE) # clean up
#' }

ss3sim_base <- function(iterations, scenarios, f_params,
  index_params, lcomp_params = NULL, agecomp_params = NULL, calcomp_params = NULL,
  wtatage_params = NULL, mlacomp_params = NULL, em_binning_params = NULL,
  estim_params = NULL, tv_params = NULL, operat_params = NULL, om_dir, em_dir,
  retro_params = NULL, data_params = NULL, weight_comps_params = NULL,
  user_recdevs = NULL, user_recdevs_warn = TRUE,
  bias_adjust = FALSE, hess_always = FALSE,
  print_logfile = TRUE, sleep = 0, seed = 21,
  ...) {

  # In case ss3sim_base is stopped before finishing:
  old_wd <- getwd()
  on.exit(setwd(old_wd), add = TRUE)

    # TODO maybe: manipulate the OM for each scenario ONLY; this can't be done
    # in parallel, but may be faster than doing OM model runs for each
    # scenario in parallel (test this). Then, once the OM is created, it can
    # then be copied into each folder (along with the EM) and sampled from for
    # each iteration.
  if (rlang::is_missing(scenarios)) {
    scenarios <- setup_scenarios_name(check = TRUE)
  }
  stopifnot(length(scenarios) == 1)
  sc <- scenarios

  if(!is.null(user_recdevs)) {
    if(ncol(user_recdevs) < max(iterations)) {
      stop("The number of columns in user_recdevs is less than the ",
        "specified number of iterations.")
    }
  }
  if (is.null(agecomp_params) & is.null(lcomp_params) & is.null(calcomp_params)) {
    stop("Simulations must have age or length data and both are NULL.")
  }
    for(i in iterations) {

      # Create folders, copy models, check for necessary files, rename
      # files for consistency
      iteration_existed <- copy_ss3models(model_dir = om_dir, scenarios = sc,
        iterations = i, type = "om")
      if (iteration_existed) next
      iteration_existed <- copy_ss3models(model_dir = em_dir, scenarios = sc,
        iterations = i, type = "em")
      pathom <- file.path(sc, i, "om")
      pathem <- file.path(sc, i, "em")

      # Make the OM as specified by the user -----------------------------------

      # Change the control file if using time varying
      if(!is.null(tv_params)) {
        # Change time-varying parameters; e.g. M, selectivity, growth...
        wd <- getwd()
        setwd(pathom)
        change_tv(change_tv_list      = tv_params,
                  ctl_file_in         = "om.ctl",
                  ctl_file_out        = "om.ctl")
        setwd(wd)
      }
      # change the OM control file for NOT time varying parameters.
      if(!is.null(operat_params)) {
        do.call("change_o", c(operat_params,
                 ctl_file_in = file.path(sc, i, "om", "om.ctl"),
                 ctl_file_out = file.path(sc,i, "om", "om.ctl")
                 ))
      }
      # The following section adds recruitment deviations
      # First, pull in sigma R from the operating model
      sigmar <- get_sigmar(file.path(sc, i, "om", "om"))
      recdevs <- get_recdevs(iteration = i, n = 2000, seed = seed)
      if(is.null(user_recdevs)) {
        sc_i_recdevs <- sigmar * recdevs - sigmar^2/2 # from the package data
      } else {if(user_recdevs_warn & i == 1) {
          warning("No bias correction is done internally for user-supplied ",
              "recruitment deviations and must be done manually. See the ",
              "vignette for more details. Biased recruitment deviations can ",
              "lead to biased model results.", call. = FALSE)
        }
        sc_i_recdevs <- user_recdevs[, i] # user specified recdevs
      }

      # Find number of years in OM to change recdevs and F
      datfile.orig <- SS_readdat(file.path(sc, i, "om", "ss3.dat"),
                                 version = NULL, verbose = FALSE)
      forfile.orig <- SS_readforecast(file.path(sc, i, "om", "forecast.ss"),
        verbose = FALSE)
      xyears <- seq(datfile.orig[["styr"]],
        datfile.orig[["endyr"]] + forfile.orig$Nforecastyrs)
      sc_i_recdevs <- setNames(sc_i_recdevs[seq_along(xyears)], xyears)
      change_rec_devs(recdevs      = sc_i_recdevs,
                      ctl_file_in  = file.path(sc, i, "om", "om.ctl"),
                      ctl_file_out = file.path(sc, i, "om", "om.ctl"))

      do.call(change_f, c(f_params,
        ctl_file_in         = file.path(sc, i, "om", "om.ctl"),
        ctl_file_out        = file.path(sc, i, "om", "om.ctl"))
      )

      # Change the data structure in the OM to produce the expected
      # values we want. This sets up the 'dummy' bins before we run
      # the OM one last time. Then we'll sample from the expected values
      # with error.

      ## This returns a superset of all years/fleets/data types needed to
      ## do sampling.
      data_args <- calculate_data_units(
                                        # why no specification of index_params here? Need?
                                        lcomp_params    = lcomp_params,
                                        agecomp_params  = agecomp_params,
                                        calcomp_params  = calcomp_params,
                                        mlacomp_params  = mlacomp_params,
                                        wtatage_params  = wtatage_params)

      # Start by clearing out the old data. Important so that extra data
      # doesn't trip up change_data:
      datfile.modified <- clean_data(dat_list = datfile.orig,
        index_params = index_params, verbose = FALSE) # why only index_params called here?

      # check qs are correct.
    ctlom <- r4ss::SS_readctl(file = file.path(sc,i, "om", "om.ctl"),
      use_datlist = TRUE, datlist = datfile.orig,
      verbose = FALSE, echoall = FALSE)
    qtasks <- check_q(ctl_list = ctlom, Nfleets = datfile.orig$Nfleets,
      desiredfleets = index_params$fleets)
    ctlom <- change_q(string_add = qtasks$add, string_remove = qtasks$remove,
      overwrite = TRUE,
      ctl_file_in = file.path(sc,i, "om", "om.ctl"),
      ctl_list = NULL, dat_list = datfile.modified,
      ctl_file_out = file.path(sc,i, "om", "om.ctl"))
      datfile.modified <- change_catch(dat_list = datfile.modified,
                                       f_params = f_params)
      # Note some are data_args and some are data_params:
      do.call("change_data", c(
                  dat_list         = list(datfile.modified),
                  outfile          = file.path(sc, i, "om", "ss3.dat"),
                  data_args, data_params,
                  nsex = datfile.orig$Ngenders))

      # Run the operating model and copy the dat file over
      run_ss3model(dir = pathom, ...)
      if(!file.exists(file.path(sc, i, "om", "data.ss_new")))
          stop("The data.ss_new not created in the OM run for ",
                     sc, "-",i, ": is something wrong with initial model files?")
      expdata <- r4ss::SS_readdat(file.path(sc, i, "om", "data.ss_new"),
        section = 2, verbose = FALSE)
      # check that there is some catch to ensure the OM data data generation
      # did not fail.
      if(all(expdata[["catch"]][,"catch"] == 0)) {
        stop("Stock Synthesis failed to generate catch for the OM (i.e., all ",
             "catch is 0.) This may occur due to not sampling at least 1 type ",
             "of composition data for the fishery. If failing inexplicably, ",
             "please contact the ss3sim developers.")
      }
      #TODO: rather than write expdata to file: dat_list <- expdata; rm(expdata)
      r4ss::SS_writedat(expdata, file.path(sc, i, "em", "ss3.dat"),
        overwrite = TRUE, verbose = FALSE)
      # Sample from the OM -----------------------------------------------------
      ## Read in the datfile once and manipulate as a list object, then
      ## write it back to file at the end, before running the EM.
      # todo: use expdata rather than reading in the file again
      dat_list <- SS_readdat(file.path(sc, i, "em", "ss3.dat"),
                             version = NULL, verbose = FALSE)
      ## Survey biomass index
      dat_list <- do.call("sample_index", c(dat_list = list(dat_list), index_params))

      ## Add error in the length comp data
      if(!is.null(lcomp_params$fleets)){
        dat_list <- do.call("sample_lcomp", c(list(dat_list), lcomp_params))
        lcomps_sampled <- TRUE # needed if calcomps, if using.
      } else {
         lcomps_sampled <- FALSE # needed for calcomps, if using
      }

      ## Add error in the age comp data. Need to do this last since other
      ## sampling functions rely on the age data. Also, if user doesn't
      ## call this function we need to delete the data
      if(!is.null(agecomp_params$fleets)){
        dat_list <- do.call("sample_agecomp", c(list(dat_list), agecomp_params))
      }

      ## Add error in the empirical weight-at-age comp data. Note that if
      ## arguments are passed to this function it's functionality is turned
      ## on by setting the wtatage switch to 1. If it's off SS will just
      ## ignore the wtatage.dat file so no need to turn it "off" like the
      ## other data.
      #TODO: check below section, as wtatage implementation has changed from 3.24
      # to 3.30.
      if(!is.null(wtatage_params)){
          if(!is.null(wtatage_params$fleets)){
              ## Make sure W@A option is turned on in the EM
              tmp_ctl <- readLines(file.path(sc,i,"em","em.ctl"))
              wtatage_line <- grep("0 means do not read wtatage.ss", tmp_ctl, fixed =TRUE)
              wtatage_option <- strsplit(tmp_ctl[wtatage_line], " ")[[1]]
              wtatage_option[1] <- 1
              tmp_ctl[wtatage_line] <- paste(wtatage_option, collapse = " ")
              writeLines(file.path(sc, i, "em", "em.ctl"))
              #sample wtatage.
              do.call("sample_wtatage", c(
                                  wta_file_in = file.path(sc, i, "om", "wtatage.ss_new"),
                                  outfile     = file.path(sc, i, "em", "wtatage.ss"),
                                  dat_list    = list(dat_list),
                                  ctl_file_in = file.path(sc, i, "om", "control.ss_new"),
                                  wtatage_params))
          }
      }

      ## Add error in the mean length-at-age comp data. This sampling
      ## function needs full age data so needs to be done before that
      ## sampling function is called. Also, if this function isn't called
      ## we need to delete that data, so I'm doing that based on whether it
      ## is NULL, so it always needs to be called.
      if(!is.null(mlacomp_params$fleets)){
          dat_list <- do.call("sample_mlacomp", c(dat_list = list(dat_list),
                                         outfile        = NULL,
                                         ctl_file_in    = file.path(sc, i, "om", "control.ss_new"),
                                         mlacomp_params,
                                         mean_outfile   = file.path(sc, i, "em",
                                           paste0(mlacomp_params$mean_outfile, ".csv"))))
      }

      ## Add error in the conditional age at length comp data. The
      ## cal data are independent of the marginal agecomp and length comp data
      ## in the SS 3.30 implementation.New length comp data is created
      ## especially for conditional age at length comp data.
      if(!is.null(calcomp_params$fleets)){
        dat_list <- do.call("sample_calcomp", c(
          dat_list = list(dat_list),
          lcomps_sampled = lcomps_sampled,
          exp_vals_list = list(expdata),
          calcomp_params))
      }

      ## End of manipulating the data file (except for rebinning), so clean it
      dat_list <- clean_data(dat_list      = dat_list,
                            index_params   = index_params,
                            lcomp_params   = lcomp_params,
                            agecomp_params = agecomp_params,
                            calcomp_params = calcomp_params,
                            mlacomp_params = mlacomp_params,
                            verbose        = FALSE)

      # Make EM as specified by user -------------------------------------------

      ## Manipulate EM starter file for a possible retrospective analysis
      if(!is.null(retro_params)) {
        do.call("change_retro", c(
          str_file_in = file.path(sc, i, "em", "starter.ss"),
          str_file_out = file.path(sc, i, "em", "starter.ss"),
          retro_params))
      }

	    ## Now change the binning structure in the EM ss3.dat file as needed
      if (!is.null(em_binning_params$lbin_method)) {
          dat_list <- do.call("change_em_binning", c(
              dat_list         = list(dat_list),
              outfile          = NULL,
              em_binning_params))
      }

      # Manipulate EM control file to adjust what gets estimated

      if(!is.null(estim_params)) {
        wd <- getwd()
        setwd(pathem)
        dat_list <- do.call("change_e", c(
                  ctl_file_in          = "em.ctl",
                  ctl_file_out         = "em.ctl",
                  dat_list             = list(dat_list),
                  for_file_in          = "forecast.ss",
                  estim_params))
        setwd(wd)
      }

# check q EM values are correct.
    ctlem <- r4ss::SS_readctl(file = file.path(sc,i, "em", "em.ctl"),
      use_datlist = TRUE, datlist = datfile.orig,
      verbose = FALSE, echoall = FALSE)
    qtasks <- check_q(ctl_list = ctlem, Nfleets = datfile.orig$Nfleets,
      desiredfleets = index_params$fleets)
    ctl_list <- change_q(string_add = qtasks$add, string_remove = qtasks$remove,
      overwrite = TRUE,
      ctl_file_in = file.path(sc,i, "em", "em.ctl"),
      ctl_list = NULL, dat_list = datfile.modified,
      ctl_file_out = NULL)
      ss_version <- get_ss_ver_dl(dat_list)
      newlists <- change_year(dat_list, ctl_list)
      SS_writedat(datlist = newlists$dat_list,
        outfile = file.path(sc, i, "em", "ss3.dat"),
        version = ss_version, overwrite = TRUE, verbose = FALSE)
      SS_writectl(ctllist = newlists$ctl_list,
        outfile = file.path(sc, i, "em", "em.ctl"),
        version = ss_version, overwrite = TRUE, verbose = FALSE)
      # Add dirichlet multinomial parameters if using
      if(!is.null(weight_comps_params)) {
        if(weight_comps_params$method == "DM") {
           # convert model so it can be used
          weight_comps(method = weight_comps_params$method,
                       fleets = weight_comps_params$fleets,
                       init_run = FALSE, # although not really needed for DM
                       niters_weighting = 0,
                       bias_adjust = bias_adjust,
                       hess_always = hess_always,
                       dir = pathem,
                       iter = i)
        }
      }
      # Run the EM -------------------------------------------------------------
      # run model 1x as-is, regardless if data weighting used or not.
      run_ss3model(dir = pathem,
                   hess = ifelse(bias_adjust, TRUE, hess_always), ...)
      success <- get_success(dir = pathem)
      if(bias_adjust & all(success > 0)) {
        #run model to do the bias adjustment
        bias <- calculate_bias(dir = pathem,
                               ctl_file_in = "em.ctl")
        run_ss3model(dir = pathem,
                     hess = ifelse(bias_adjust, TRUE, hess_always), ...)
      }
      if(!is.null(weight_comps_params)) {
        # need to do data tuning, fregardless of if bias adjustment was done.
        if(weight_comps_params$method %in% c("MI", "Francis")) {
          weight_comps(method = weight_comps_params$method,
                       fleets = weight_comps_params$fleets,
                       niters_weighting = weight_comps_params$niters_weighting,
                       init_run = FALSE,
                       bias_adjust = bias_adjust,
                       hess_always = hess_always,
                       dir = pathem)
        }
      }

# Write log file ---------------------------------------------------------------
# TODO pull the log file writing into a separate function and update
# for current arguments
      if(print_logfile) {
        today <- format(Sys.time(), "%Y-%m-%d")
        me <- Sys.info()["nodename"]
        sink(file.path(sc, i, "log.txt"))
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
        cat("\n\n# bias adjust?\n")
        print(bias_adjust)
        cat("\n\n# hess always?\n")
        print(hess_always)
        cat("\n\n# User recdevs?\n")
        print(user_recdevs)
        cat("\n\n# This run used the recruitment deviations (before scaling to sigma r):\n")
        print(sc_i_recdevs)
        cat("\n\n# With sigma r of\n")
        print(sigmar)

        sink()
      }

      #  Pause to reduce average CPUE use?
      Sys.sleep(sleep)

    } # end iterations
return(scenarios)
}
