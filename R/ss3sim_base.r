#' High-level wrapper to run a simulation
#'
#' A wrapper function that
#' * calls [r4ss::run()] to run the operating model,
#' * samples the output to create fishery and survey data, and
#' * calls [r4ss::run()] to run the estimation model.
#' This function is the main workhorse of this package and
#' is typically not called by the user but called from [run_ss3sim()].
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios A name to use as the folder name for the unique combination
#'   of parameters for the OM and EM.
#' @param tv_params A named list containing arguments for
#'   [change_tv()] (time-varying).
#' @param operat_params A named list containing arguments for [change_o()].
#' @param f_params A named list containing arguments for [change_f()].
#'   A mandatory input.
#' @param index_params A named list containing arguments for
#'   [sample_index()]. A mandatory input.
#' @param discard_params A named list containing arguments for
#'   [sample_discard()].
#' @param lcomp_params A named list containing arguments for
#'   [sample_lcomp()]. A mandatory input.
#' @param agecomp_params A named list containing arguments for
#'   [sample_agecomp()]. A mandatory input.
#' @param calcomp_params A named list containing arguments for
#'   [sample_calcomp()], for conditional age-at-length data.
#' @param wtatage_params A named list containing arguments for
#'   [sample_wtatage()], for empirical weight-at-age data.
#' @param mlacomp_params A named list containing arguments for
#'   [sample_mlacomp()], for mean length-at-age data.
#' @param retro_params A named list containing the arguments for
#'   [change_retro()].
#' @param estim_params A named list containing arguments for
#'   [change_e()].
#' @param em_binning_params A named list containing arguments for
#'   [change_em_binning()].
#' @param data_params A named list containing arguments for changing data.
#' @param weight_comps_params A named list containing arguments for
#'   [r4ss::tune_comps()].
#' @param om_dir The directory with the operating model you want to copy and use
#'   for the specified simulations.
#' @param em_dir The directory with the estimation model you want to copy and
#'   use for the specified simulations. If `NA`, then no estimation method is
#'   included and only data is created.
#' @template user_recdevs
#' @param user_recdevs_warn A logical argument allowing users to turn the
#'   warning regarding biased recruitment deviations off when `user_recdevs`
#'   are specified.
#' @param bias_adjust A logical argument specifying bias adjustment is
#'   conducted. Bias adjustment helps assure that the estimated recruitment
#'   deviations, which are assumed to be log-normally distributed, are mean
#'   unbiased leading to mean-unbiased estimates of biomass [Methot and Taylor,
#'   2011](https://cdnsciencepub.com/doi/abs/10.1139/f2011-092). Bias
#'   adjustment should always be performed when using maximum likelihood
#'   estimation when running simulations for publication or management. The
#'   argument allows users to turn bias adjustment off because it involves
#'   running the EM multiple times with the hessian and is not needed when
#'   initially exploring your simulation structure.
#' @param sleep A time interval (in seconds) to pause on each iteration. Useful
#'   if you want to reduce average CPU time -- perhaps because you're working on
#'   a shared server.
#' @param seed The seed value to pass to [get_recdevs()] when
#'   generating recruitment deviations. The generated recruitment deviations
#'   depend on the iteration value, but also on the value of `seed`. A
#'   given combination of iteration, number of years, and `seed` value will
#'   result in the same recruitment deviations.
#' @param extras A character string that will be passed to the `extras`
#'   argument of [r4ss::run()]. The default is `" "` which results in the
#'   hessian being inverted if the model has positive phases, i.e., the EM.
#'   Pass `"-nohess "` if you do not want to estimate the hessian or
#'   `"-stopph 3 -nohess"` if you want to stop the model in phase 3 and you
#'   do not want to invert the hessian. The key is that the entry must be one
#'   string with spaces between the arguments that will be passed to ADMB.
#' @author Sean Anderson with contributions from many others as listed in
#'   the DESCRIPTION file.
#' @return
#' The output will appear in whatever your current \R working directory
#' is. There will be folders named after your scenarios with one folder
#' per iteration. Each iteration folder with include an operating model and
#' an estimation method. Your directory will look like the following:
#' \itemize{
#' \item `scen-cod/1/om`
#' \item `scen-cod/1/em`
#' \item `scen-cod/2/om`
#' \item ...
#' }
#' If `em_dir = NA`, then the contents of the `em` directories will be minimal
#' because they will only contain the simulated data and
#' not any fits to those data.
#'
#' @seealso [run_ss3sim()]
#' @export
#' @details
#' This function is written to be flexible. You can specify the fishing
#' mortality, survey catch-per-unit-effort settings,
#' length-composition data settings, etc. in the function call as list objects
#' (see the example below). For a generic higher-level function, see
#' [run_ss3sim()].
#'
#'
#' @examples
#' \dontrun{
#' # Create a temporary folder for the output and set the working directory:
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-base-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#' on.exit(setwd(wd), add = TRUE)
#'
#' # Find the data in the ss3sim package:
#' d <- system.file("extdata", package = "ss3sim")
#' om_dir <- file.path(d, "models", "cod-om")
#' em_dir <- file.path(d, "models", "cod-em")
#'
#' # Or, create the argument lists directly in R:
#'
#' F0 <- list(
#'   years = 1:100,
#'   fleets = 1,
#'   fvals = c(rep(0, 25), rep(0.114, 75))
#' )
#'
#' index1 <- list(
#'   fleets = 2, years = list(seq(62, 100, by = 2)),
#'   sds_obs = list(0.1)
#' )
#'
#' lcomp1 <- list(
#'   fleets = c(1, 2), Nsamp = list(50, 100),
#'   years = list(26:100, seq(62, 100, by = 2))
#' )
#'
#' agecomp1 <- list(
#'   fleets = c(1, 2), Nsamp = list(50, 100),
#'   years = list(26:100, seq(62, 100, by = 2))
#' )
#'
#' E0 <- list(
#'   par_name = c("LnQ_base_Fishery", "NatM_uniform_Fem_GP_1"),
#'   par_int = c(NA, NA), par_phase = c(-5, -1), forecast_num = 0
#' )
#'
#' ss3sim_base(
#'   iterations = 1,
#'   scenarios = "D1-E0-F0-cod", # name as desired
#'   f_params = F0,
#'   index_params = index1,
#'   lcomp_params = lcomp1,
#'   agecomp_params = agecomp1,
#'   estim_params = E0,
#'   om_dir = om_dir,
#'   em_dir = em_dir
#' )
#' replist <- r4ss::SS_output(file.path("D1-E0-F0-cod", 1, "em"),
#'   verbose = FALSE, printstats = FALSE, covar = FALSE
#' )
#' testthat::expect_equivalent(replist[["cpue"]][, "year"], index1[["years"]][[1]])
#'
#' test <- replist
#' unlink("D1-E0-F0-cod", recursive = TRUE) # clean up
#'
#' # Run without an EM, where this package operates as a data-generating tool
#' ss3sim_base(
#'   iterations = 1,
#'   scenarios = "noEM",
#'   f_params = F0,
#'   index_params = index1,
#'   lcomp_params = lcomp1,
#'   agecomp_params = agecomp1,
#'   estim_params = E0,
#'   om_dir = om_dir,
#'   em_dir = NA
#' )
#' }
#'
ss3sim_base <- function(iterations,
                        scenarios,
                        f_params,
                        index_params,
                        discard_params = NULL,
                        lcomp_params = NULL,
                        agecomp_params = NULL,
                        calcomp_params = NULL,
                        wtatage_params = NULL,
                        mlacomp_params = NULL,
                        em_binning_params = NULL,
                        estim_params = NULL,
                        tv_params = NULL,
                        operat_params = NULL,
                        om_dir,
                        em_dir,
                        retro_params = NULL,
                        data_params = NULL,
                        weight_comps_params = NULL,
                        user_recdevs = NULL,
                        user_recdevs_warn = TRUE,
                        bias_adjust = FALSE,
                        sleep = 0,
                        seed = 21,
                        extras = " ") {
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

  if (!is.null(user_recdevs)) {
    stopifnot(class(user_recdevs) %in% c("matrix", "data.frame", "array"))
    if (ncol(user_recdevs) < max(iterations)) {
      stop(
        "The number of columns in user_recdevs is less than the ",
        "specified number of iterations."
      )
    }
  }
  if (is.null(agecomp_params) & is.null(lcomp_params) & is.null(calcomp_params)) {
    stop("Simulations must have age or length data and both are NULL.")
  }
  for (i in iterations) {
    # todo: fix spacing, organize comments

    # Create folders, copy models, check for necessary files, rename
    # files for consistency
    iteration_existed <- copy_ss3models(
      model_dir = om_dir, scenario = sc,
      iteration = i, type = "om"
    )
    if (iteration_existed) next
    pathom <- file.path(sc, i, "om")
    pathem <- file.path(sc, i, "em")
    if (!is.na(em_dir)) {
      copy_ss3models(
        model_dir = em_dir, scenario = sc,
        iteration = i, type = "em"
      )
    } else {
      dir.create(pathem, showWarnings = FALSE, recursive = TRUE)
    }

    # Make the OM as specified by the user -----------------------------------

    # Change the control file if using time varying
    if (!is.null(tv_params)) {
      # Change time-varying parameters; e.g. M, selectivity, growth...
      wd <- getwd()
      setwd(pathom)
      change_tv(
        change_tv_list = tv_params,
        ctl_file_in = "om.ctl",
        ctl_file_out = "om.ctl"
      )
      setwd(wd)
    }
    # change the OM control file for NOT time varying parameters.
    if (!is.null(operat_params)) {
      do.call("change_o", c(operat_params,
        ctl_file_in = file.path(sc, i, "om", "om.ctl"),
        ctl_file_out = file.path(sc, i, "om", "om.ctl")
      ))
    }
    # The following section adds recruitment deviations
    # First, pull in sigma R from the operating model
    sigmar <- get_sigmar(file.path(sc, i, "om", "om"))
    recdevs <- get_recdevs(iteration = i, n = 2000, seed = seed)
    if (is.null(user_recdevs)) {
      sc_i_recdevs <- sigmar * recdevs - sigmar^2 / 2 # from the package data
    } else {
      if (user_recdevs_warn & i == 1) {
        warning("No bias correction is done internally for user-supplied ",
          "recruitment deviations and must be done manually. See the ",
          "vignette for more details. Biased recruitment deviations can ",
          "lead to biased model results.",
          call. = FALSE
        )
      }
      sc_i_recdevs <- user_recdevs[, i] # user specified recdevs
    }

    # Find number of years in OM to change recdevs and F
    datfile.orig <- r4ss::SS_readdat(file.path(sc, i, "om", "ss3.dat"),
      verbose = FALSE
    )
    forfile.orig <- r4ss::SS_readforecast(file.path(sc, i, "om", "forecast.ss"),
      verbose = FALSE
    )
    xyears <- seq(
      datfile.orig[["styr"]],
      datfile.orig[["endyr"]] +
        ifelse(
          test = forfile.orig[["Forecast"]] == 0,
          yes = 0,
          no = ifelse(
            test = is.null(forfile.orig$Nforecastyrs),
            yes = 0,
            no = forfile.orig[["Nforecastyrs"]]
          )
        )
    )
    sc_i_recdevs <- stats::setNames(sc_i_recdevs[seq_along(xyears)], xyears)
    change_rec_devs(
      recdevs = sc_i_recdevs,
      ctl_file_in = file.path(sc, i, "om", "om.ctl"),
      ctl_file_out = file.path(sc, i, "om", "om.ctl")
    )

    ctlom <- r4ss::SS_readctl(
      file = file.path(sc, i, "om", "om.ctl"),
      use_datlist = TRUE, datlist = datfile.orig,
      verbose = FALSE
    )
    ctlom <- do.call(change_f, c(f_params, ctl_list = list(ctlom)))

    # Change the data structure in the OM to produce the expected
    # values we want. This sets up the 'dummy' bins before we run
    # the OM one last time. Then we'll sample from the expected values
    # with error.

    ## This returns a superset of all years/fleets/data types needed to
    ## do non-catch and -index sampling.
    # todo: think aboout removing calculate_data_units
    # todo: think about how to remove data types that aren't used, should we?
    data_args <- calculate_data_units(
      lcomp_params    = lcomp_params,
      agecomp_params  = agecomp_params,
      calcomp_params  = calcomp_params,
      mlacomp_params  = mlacomp_params,
      wtatage_params  = wtatage_params
    )

    datfile.modified <- datfile.orig
    ## OM: index
    # todo: find a better way to check if month exists
    # maybe warn users that this default is being set for index & discard
    if (!is.null(index_params)) {
      if (!any(grepl("month", names(index_params), ignore.case = TRUE))) {
        index_params[["month"]] <- standardize_sampling_args(
          index_params[["fleets"]], index_params[["years"]],
          other_input = list(1)
        )
      }
      datfile.modified[["CPUE"]] <- do.call(
        "rbind",
        mapply(data.frame,
          SIMPLIFY = FALSE,
          year = index_params[["years"]],
          month = index_params[["month"]],
          index = as.list(index_params[["fleets"]]),
          MoreArgs = list(obs = 1, se_log = 0.1)
        )
      )
    }
    ## OM: discard
    if (!is.null(discard_params)) {
      if (!any(grepl("month", names(discard_params), ignore.case = TRUE))) {
        discard_params[["month"]] <- standardize_sampling_args(
          index_params[["fleets"]], index_params[["years"]],
          other_input = list(1)
        )
      }
      datfile.modified[["discard_data"]] <- do.call(
        "rbind",
        mapply(data.frame,
          SIMPLIFY = FALSE,
          year = discard_params[["years"]],
          month = discard_params[["month"]],
          Flt = as.list(discard_params[["fleets"]]),
          MoreArgs = list(Discard = 1, stderr = 0.1)
        )
      )
    }

    # check qs are correct.
    qtasks <- check_q(
      ctl_list = ctlom,
      desiredfleets = index_params$fleets
    )
    ctlom <- change_q(
      string_add = qtasks$add, string_remove = qtasks$remove,
      ctl_list = ctlom,
    )

    ## OM: change bins
    # todo: do this for mean size at age
    fixlist <- function(local) {
      out <- stats::setNames(
        unlist(local, recursive = FALSE, use.names = FALSE),
        names(local)
      )
      if (inherits(out, "numeric")) {
        return(local)
      } else {
        return(out)
      }
    }
    data_params <- fixlist(data_params)
    datfile.modified <- change_pop_bin(
      dat_list = datfile.modified,
      binwidth = data_params[["pop_binwidth"]],
      minimum_size = data_params[["pop_minimum_size"]],
      maximum_size = data_params[["pop_maximum_size"]]
    )
    if (!is.null(data_params[["age_bins"]])) {
      datfile.modified[["agecomp"]] <- change_dat_bin(datfile.modified[["agecomp"]],
        bins = setup_bins(data_params[["age_bins"]],
          nsex = datfile.modified[["Nsexes"]],
          leader = "a"
        )
      )
      datfile.modified[["agebin_vector"]] <- data_params[["age_bins"]]
      datfile.modified[["N_agebins"]] <- length(datfile.modified[["agebin_vector"]])
    }
    if (!is.null(data_params[["len_bins"]])) {
      datfile.modified[["lencomp"]] <- change_dat_bin(datfile.modified[["lencomp"]],
        bins = setup_bins(data_params[["len_bins"]],
          nsex = datfile.modified[["Nsexes"]],
          leader = "l"
        )
      )
      datfile.modified[["lbin_vector"]] <- data_params[["len_bins"]]
      datfile.modified[["N_lbins"]] <- length(datfile.modified[["lbin_vector"]])
    }

    datfile.modified <- change_catch(
      dat_list = datfile.modified,
      ctl_list = ctlom
    )

    ## OM: composition data
    if (!is.null(agecomp_params)) {
      datfile.modified <- change_comp(
        dat_list = datfile.modified,
        type = "age",
        bins = if (is.null(data_params$age_bins)) {
          datfile.modified[["agebin_vector"]]
        } else {
          data_params$age_bins
        },
        paramlist = agecomp_params,
        nsex = datfile.modified[["Nsexes"]]
      )
    }
    if (!is.null(lcomp_params)) {
      datfile.modified <- change_comp(
        dat_list = datfile.modified,
        type = "len",
        bins = if (is.null(data_params$len_bins)) {
          datfile.modified[["lbin_vector"]]
        } else {
          data_params$len_bins
        },
        paramlist = lcomp_params,
        nsex = datfile.modified[["Nsexes"]]
      )
    }
    datfile.modified <- change_lcomp_constant(
      lcomp_constant = data_params[["lcomp_constant"]],
      dat_list = datfile.modified
    )
    datfile.modified <- change_tail_compression(
      tail_compression = data_params[["tail_compression"]],
      dat_list = datfile.modified
    )
    if (!is.null(calcomp_params)) {
      params <- mapply(standardize_sampling_args,
        other_input = calcomp_params[grep("sex|part|Age|years", names(calcomp_params))],
        MoreArgs = list(
          fleets = calcomp_params$fleets,
          years = calcomp_params$years
        ),
        SIMPLIFY = FALSE, USE.NAMES = TRUE
      )
      params$fleets <- calcomp_params$fleets
      datfile.modified <- change_comp(
        dat_list = datfile.modified,
        type = "cal",
        paramlist = params,
        nsex = datfile.modified[["Nsexes"]]
      )
    }
    if (!is.null(mlacomp_params)) {
      # todo: fix this to use comp function
      # dummy_dat <- as.data.frame(do.call(lapply(fleets, function(fleet)
      #     data.frame("year"   = years, "month" = 1, "Flt"  = fleet, "sex" = 0,
      #                "part"   = 0, "AgeErr"=1, "Nsamp" = 10, stringsAsFactors = FALSE))))
      # dummy_df <- data.frame(matrix(1, nrow=nrow(dummy_dat), ncol=length(age_bins)*2))
      # names(dummy_df) <- c(paste0("a", c(age_bins)), paste0("N", c(age_bins)))
      # datfile.modified$MeanSize_at_Age_obs <- cbind(dummy_dat, dummy_df)
      # datfile.modified$N_MeanSize_at_Age_obs <- NROW(datfile.modified$MeanSize_at_Age_obs)
    }
    r4ss::SS_writedat(
      datlist = datfile.modified,
      outfile = file.path(sc, i, "om", "ss3.dat"),
      overwrite = TRUE, verbose = FALSE
    )
    r4ss::SS_writectl(
      ctllist = ctlom,
      outfile = file.path(sc, i, "om", "om.ctl"),
      overwrite = TRUE
    )

    # Run the operating model and copy the dat file over
    r4ss::run(
      dir = pathom,
      exe = get_bin(),
      # Ensure no hess is in extras
      extras = paste("-nohess", gsub("-nohess *", " ", extras)),
      skipfinished = FALSE,
      show_in_console = FALSE,
      verbose = FALSE
    )
    if (!file.exists(file.path(sc, i, "om", "data_expval.ss"))) {
      stop(
        "The data_expval.ss was not created in the OM run for ",
        sc, "-", i, ": is something wrong with initial model files?"
      )
    }
    expdata <- r4ss::SS_readdat(
      file = file.path(sc, i, "om", "data_expval.ss"),
      verbose = FALSE
    )
    # check that there is some catch to ensure the OM data data generation
    # did not fail.
    if (all(expdata[["catch"]][, "catch"] == 0)) {
      stop(
        "Stock Synthesis failed to generate catch for the OM (i.e., all ",
        "catch is 0.) This may occur due to not sampling at least 1 type ",
        "of composition data for the fishery. If failing inexplicably, ",
        "please contact the ss3sim developers."
      )
    }

    # Sample from the OM -----------------------------------------------------
    dat_list <- expdata

    ## Sample catches
    dat_list <- sample_catch(dat_list = dat_list)
    ## Survey biomass index
    dat_list <- do.call(
      "sample_index",
      c(dat_list = list(dat_list), index_params)
    )
    ## Discards
    if (!is.null(discard_params)) {
      dat_list <- do.call("sample_discard", c(dat_list = list(dat_list), discard_params))
    }

    ## Add error in the length comp data
    if (!is.null(lcomp_params$fleets)) {
      dat_list <- do.call("sample_lcomp", c(list(dat_list), lcomp_params))
      lcomps_sampled <- TRUE # needed if calcomps, if using.
    } else {
      lcomps_sampled <- FALSE # needed for calcomps, if using
    }

    ## Add error in the age comp data. Need to do this last since other
    ## sampling functions rely on the age data. Also, if user doesn't
    ## call this function we need to delete the data
    if (!is.null(agecomp_params$fleets)) {
      dat_list <- do.call("sample_agecomp", c(list(dat_list), agecomp_params))
    }

    ## Add error in the empirical weight-at-age comp data.
    # TODO: check below section, as wtatage implementation has changed from 3.24
    # to 3.30.
    if (!is.null(wtatage_params)) {
      if (!is.null(wtatage_params$fleets)) {
        do.call("sample_wtatage", c(
          wta_file_in = file.path(sc, i, "om", "wtatage.ss_new"),
          outfile     = file.path(sc, i, "em", "wtatage.ss"),
          dat_list    = list(dat_list),
          ctl_file_in = file.path(sc, i, "om", "control.ss_new"),
          wtatage_params
        ))
      }
    }

    ## Add error in the mean length-at-age comp data. This sampling
    ## function needs full age data so needs to be done before that
    ## sampling function is called. Also, if this function isn't called
    ## we need to delete that data, so I'm doing that based on whether it
    ## is NULL, so it always needs to be called.
    if (!is.null(mlacomp_params$fleets)) {
      dat_list <- do.call("sample_mlacomp", c(
        dat_list = list(dat_list),
        outfile = NULL,
        ctl_file_in = file.path(sc, i, "om", "control.ss_new"),
        mlacomp_params,
        mean_outfile = file.path(
          sc, i, "em",
          paste0(mlacomp_params$mean_outfile, ".csv")
        )
      ))
    }

    ## Add error in the conditional age at length comp data. The
    ## cal data are independent of the marginal agecomp and length comp data
    ## in the Stock Synthesis 3.30 implementation.New length comp data is created
    ## especially for conditional age at length comp data.
    if (!is.null(calcomp_params$fleets)) {
      dat_list <- do.call("sample_calcomp", c(
        dat_list = list(dat_list),
        lcomps_sampled = lcomps_sampled,
        exp_vals_list = list(expdata),
        calcomp_params
      ))
    }

    ## End of manipulating the data file (except for rebinning), so clean it
    dat_list <- clean_data(
      dat_list = dat_list,
      lcomp_params = lcomp_params,
      agecomp_params = agecomp_params,
      calcomp_params = calcomp_params,
      mlacomp_params = mlacomp_params,
      verbose = FALSE
    )

    # Make EM as specified by user -------------------------------------------
    ## Change the binning structure in the EM ss3.dat file as needed
    if (!is.null(em_binning_params)) {
      em_binning_params <- fixlist(em_binning_params)
      dat_list <- do.call("change_em_binning", c(
        dat_list         = list(dat_list),
        outfile          = NULL,
        em_binning_params
      ))
    }

    # Exit this iteration and write dat file early if no EM
    if (is.na(em_dir)) {
      r4ss::SS_writedat(
        datlist = dat_list,
        outfile = file.path(sc, i, "em", "ss3.dat"),
        overwrite = TRUE,
        verbose = FALSE
      )
      next
    }

    # Manipulate EM control file to adjust what gets estimated
    if (!is.null(estim_params)) {
      wd <- getwd()
      setwd(pathem)
      dat_list <- do.call("change_e", c(
        ctl_file_in          = "em.ctl",
        ctl_file_out         = "em.ctl",
        dat_list             = list(dat_list),
        for_file_in          = "forecast.ss",
        estim_params
      ))
      setwd(wd)
    }

    # check q EM values are correct.
    ctlem <- r4ss::SS_readctl(
      file = file.path(sc, i, "em", "em.ctl"),
      use_datlist = TRUE, datlist = datfile.orig,
      verbose = FALSE
    )
    qtasks <- check_q(
      ctl_list = ctlem,
      desiredfleets = index_params$fleets
    )
    ctl_list <- change_q(
      string_add = qtasks$add,
      string_remove = qtasks$remove,
      ctl_list = ctlem,
    )
    newlists <- change_year(dat_list, ctl_list)
    r4ss::SS_writedat(
      datlist = newlists$dat_list,
      outfile = file.path(sc, i, "em", "ss3.dat"),
      overwrite = TRUE,
      verbose = FALSE
    )
    r4ss::SS_writectl(
      ctllist = newlists$ctl_list,
      outfile = file.path(sc, i, "em", "em.ctl"),
      overwrite = TRUE,
      verbose = FALSE
    )
    # Add dirichlet multinomial parameters if using
    if (!is.null(weight_comps_params)) {
      weight_comps_params <- purrr::modify_depth(weight_comps_params, 1, unlist)
      if (weight_comps_params$method == "DM") {
        # convert model so it can be used
        out <- r4ss::tune_comps(
          fleets = weight_comps_params[["fleets"]],
          option = weight_comps_params[["method"]],
          dir = pathem,
          exe = get_bin(),
          extras = extras,
          verbose = FALSE,
          show_in_console = FALSE
        )
      }
    }
    # Manipulate EM starter file for a possible retrospective analysis
    if (!is.null(retro_params)) {
      do.call("change_retro", c(
        str_file_in = file.path(sc, i, "em", "starter.ss"),
        str_file_out = file.path(sc, i, "em", "starter.ss"),
        retro_params
      ))
    }
    # Run the EM -------------------------------------------------------------
    # run model 1x as-is, regardless if data weighting used or not.
    r4ss::run(
      dir = pathem,
      exe = get_bin(),
      extras = ifelse(bias_adjust, gsub("-nohess *", "", extras), extras),
      skipfinished = FALSE,
      show_in_console = FALSE,
      verbose = FALSE
    )
    success <- get_success(dir = pathem)
    if (bias_adjust & all(success > 0)) {
      # run model to do the bias adjustment
      bias <- calculate_bias(
        dir = pathem,
        ctl_file_in = "em.ctl"
      )
      r4ss::run(
        dir = pathem,
        exe = get_bin(),
        extras = extras,
        skipfinished = FALSE,
        show_in_console = FALSE,
        verbose = FALSE
      )
    }
    if (!is.null(weight_comps_params)) {
      # need to do data tuning, regardless of if bias adjustment was done.
      if (weight_comps_params$method %in% c("MI", "Francis")) {
        replist <- r4ss::SS_output(
          pathem,
          verbose = FALSE,
          hidewarn = TRUE,
          printstats = FALSE,
          covar = !grepl("-nohess", extras) || bias_adjust
        )
        out <- r4ss::tune_comps(
          replist = replist,
          fleets = weight_comps_params[["fleets"]],
          option = weight_comps_params[["method"]],
          niters_tuning = weight_comps_params[["niters_weighting"]],
          dir = pathem,
          exe = get_bin(),
          extras = extras,
          verbose = FALSE,
          show_in_console = FALSE
        )
      }
    }

    #  Pause to reduce average CPUE use?
    Sys.sleep(sleep)
  } # end iterations

  return(scenarios)
}
