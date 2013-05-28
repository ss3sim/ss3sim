#' Run both the operating model and assessment model
#'
#' This function is a wrapper function that can call
#' \code{\link{run_ss3model}} for the operating model, manipulate the
#' output (add recruitment deviations, survey the data, etc.), and run
#' the estimation model.
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios Which scenarios to run.
#' @param m_params A named list containing all the
#' \code{\link{change_m}} options.
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
#' @param om_model_dir The directory with the operating model you want
#' to copy and use for the specified simulations.
#' @param em_model_dir The directory with the estimation model you want
#' to copy and use for the specified simulations.
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
#' @param ... Anything extra to pass to \code{\link{run_ss3model}}.
#' For example, you may want to pass additional options to \code{SS3}
#' through the argument \code{admb_options}. Just don't pass
#' \code{-noest} or \code{-nohess} and enable bias correction.
#
#' @author Sean C. Anderson
#' @seealso \code{\link{run_fish600}}
#' @export
#' @details
#' This function is written to be flexible. You can specify the
#' natural mortality, fishing mortality, survey index,
#' length comp, and age comp parameters in the function call as list
#' objects. For a higher-level wrapper function specific to the setup
#' of the Fish600 projects, see \code{\link{run_fish600}}.
#' @examples
#' \dontrun{
#' # Pull in file paths from the package example data:
#' d <- system.file("extdata", package = "ss3sim")
#' f <- paste0(d, "/run_ss3sim_eg/")
#' om_model_dir <- paste0(f, "cod_om")
#' em_model_dir <- paste0(f, "cod_em")
#' a <- get_caseargs(folder = paste0(f, "case-arguments"), scenario =
#' "M1-F1-D1-R1-cod") 
#'
#' # With bias adjustment:
#' # (Note that bias_nsim should be bigger, say 5, but it is set to 1
#' here so the example runs faster.)
#'
#' run_ss3sim(iterations = 1, scenarios = "M1-F1-D1-R1-cod", m_params
#' = a$M, f_params = a$F, index_params = a$index, lcomp_params =
#' a$lcomp, agecomp_params = a$agecomp, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir)
#' }

run_ss3sim <- function(iterations, scenarios, m_params, f_params,
  index_params, lcomp_params, agecomp_params, retro_params,
  om_model_dir, em_model_dir, bias_adjust = FALSE, bias_nsim = 5,
  bias_already_run = FALSE, hess_always = FALSE, print_logfile = TRUE, 
  ...) {

  # The first bias_nsim runs will be bias-adjustment runs
  if(bias_adjust) {
    iterations <- c(paste0("bias/", c(1:bias_nsim)), iterations)
  }

  for(sc in scenarios) {
    for(i in iterations) {

      if(print_logfile) {
        #today <- format(Sys.time(), "%Y-%m-%d")
        today <- Sys.time()
        me <- Sys.info()["nodename"]
        sink(pastef(sc, i, "log.txt"))
        cat("These models were run at ", today, 
            "\non", me, 
            "\nin the folder ", getwd(), 
            "\nwith the following arguments.\n\n", sep = "")
        cat("M arguments")
        print(m_params)
        cat("F arguments")
        print(f_params)
        cat("index arguments")
        print(index_params)
        cat("lcomp arguments")
        print(lcomp_params)
        cat("agecomp arguments")
        print(agecomp_params)
        cat("retro arguments")
        print(retro_params)
        cat("bias adjust?")
        print(bias_adjust)
        cat("bias nsim")
        print(bias_nsim)
        cat("hess always?")
        print(hess_always)
        sink()
      }

      # Create folders, copy models, check for necessary files, rename
      # files for consistency
      copy_ss3models(model_dir = om_model_dir, scenarios = sc,
        iterations = i, type = "om")
      copy_ss3models(model_dir = em_model_dir, scenarios = sc,
        iterations = i, type = "em")

      # If we're bias adjusting, then copy over the .ctl file to the
      # em folder
      if(bias_already_run & bias_adjust) {
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

      # recdevs is a 100x100 matrix stored in the package 'data' folder
      # Columns are for iterations and rows are for years
      sc_i_recdevs <- sigmar * recdevs[, this_run_num]

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

      # Read in the data.ss_new file and write to data.dat in the om folder
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
        data_out = pastef(sc, i, "om", "data.dat"))

      # Change M
      wd <- getwd() # change_m() must be in the working directory
      setwd(pastef(sc, i, "om"))
      with(m_params,
        change_m(n_blocks            = n_blocks,
                 block_pattern       = block_pattern,
                 dev                 = dev,
                 ctl_file_in         = "om.ctl",
                 ctl_file_out        = "om.ctl",
                 dat_file            = "data.dat",
                 dat_file_out        = "data.dat",
                 how_time_varying    = how_time_varying)) 
      setwd(wd)

      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)

      # Read in the data.ss_new file and move it to the em folder
      extract_expected_data(data_ss_new = pastef(sc, i, "om", "data.ss_new"),
        data_out = pastef(sc, i, "om", "data.dat"))

      # Copy the new data.dat to the em folder
      file.copy(from = pastef(sc, i, "om", "data.dat"), to = 
        pastef(sc, i, "em", "data.dat"))

      # Survey biomass index
      with(index_params, 
        change_index(dat_file_in     = pastef(sc, i, "em", "data.dat"), 
                     dat_file_out    = pastef(sc, i, "em", "data.dat"),
                     start_surv      = start_surv,
                     end_surv        = end_surv,
                     use_index       = use_index,
                     start_fish      = start_fish,
                     end_fish        = end_fish,
                     freq_fish       = freq_fish,
                     sd_obs_fish     = sd_obs_fish)) 
 
      # Add error in the length comp data
      SS.dat = r4ss::SS_readdat(pastef(sc, i, "em", "data.dat"))
      with(lcomp_params,
        change_lcomp(infile          = SS.dat,
                     outfile         = pastef(sc, i, "em", "data.dat"),
                     distribution    = distribution,
                     Nsamp           = Nsamp,
                     minyear         = minyear,
                     maxyear         = maxyear,
                     years           = years,
                     svyears         = svyears,
                     lbin_method     = lbin_method,
                     binwidth        = binwidth,
                     minimum_size    = minimum_size,
                     maximum_size    = maximum_size,
                     N_lbins         = N_lbins,
                     lbin_vector     = lbin_vector,
                     lencomp         = lencomp))

      # Add error in the age comp data
      SS.dat2 = r4ss::SS_readdat(pastef(sc, i, "em", "data.dat"))
      with(agecomp_params, 
        change_agecomp(infile        = SS.dat2,
                       outfile       = pastef(sc, i, "em", "data.dat"),
                       distribution  = distribution,
                       Nsamp         = Nsamp,
                       minyear       = minyear,
                       maxyear       = maxyear,
                       years         = years,
                       svyears       = svyears,
                       N_agebins     = N_agebins,
                       agebin_vector = agebin_vector,
                       agecomp       = agecomp))

      # Manipulate EM starter file for a possible retrospective analysis
      with(retro_params, 
        change_retro(startfile_in    = pastef(sc, i, "em", "starter.ss"),  
                     startfile_out   = pastef(sc, i, "em", "starter.ss"), 
                     retro_yr        = retro_yr))

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
            "bias"), nsim = bias_nsim)
        bias_already_run <- TRUE
      # Since we've now run the bias adjustment routine, copy the .ctl
      # on subsequent iterations
      } 


    } # end iterations
  } # end scenarios
}

