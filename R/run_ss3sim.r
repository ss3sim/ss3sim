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
#' @param om_model_dir The directory with the operating model you want
#' to copy and use for the specified simulations.
#' @param em_model_dir The directory with the estimation model you want
#' to copy and use for the specified simulations.
#' @param bias_correct Run bias correction first? See
#' \code{\link{run_bias_ss3}}.
#' @param bias_nsim If bias correction is run, how many simulations
#' should the bias be estimated from? It will take the mean of the
#' correction factors across these runs.
#' @param ... Anything extra to pass to \code{\link{run_ss3model}}.
#' For example, you may want to pass \code{ss3path} if you haven't
#' placed \code{SS3} in your path, or you may want to pass additional
#' options to \code{SS3} through the argument \code{admb_options}.
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
#' # With bias correction:
#' # (Note that bias_nsim should be bigger, say 5, but it is set to 1
#' here so the example runs faster.)
#'
#' run_ss3sim(iterations = 1, scenarios = "M1-F1-D1-R1-cod", m_params
#' = a$M, f_params = a$F, index_params = a$index, lcomp_params =
#' a$lcomp, agecomp_params = a$agecomp, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir)
#' }


# todo change params to args
# todo change iter to reps

run_ss3sim <- function(iterations, scenarios, m_params, f_params,
  index_params, lcomp_params, agecomp_params, om_model_dir,
  em_model_dir, bias_correct = FALSE, bias_nsim = 5, 
  bias_already_run = FALSE, ...) {

  # The first bias_nsim runs will be bias-adjustment runs
  if(bias_correct) {
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

      # If we're bias correcting, then copy over the .ctl file to the
      # em folder
      if(bias_already_run & bias_correct) {
        file.copy(from = pastef(sc, "bias", "em.ctl"), to = pastef(sc,
            i, "em", "em.ctl"), overwrite = TRUE)
      }

      # Pull in sigma R from the operating model
      sigmar <- get_sigmar(pastef(sc, i, "om", "om"))

      # Exponentiate with bias adjustment
      # recdevs is a 100x100 matrix stored in the package 'data' folder
      # Columns are for iterations and rows are for years

      # Take the true iteration, even if we're working with "bias" iterations
      # this turns "bias/1" into "1" and leaves "1" unchanged
      this_run_num <- as.numeric(rev(strsplit(as.character(i), "/")[[1]])[1])
      sc_i_recdevs <- exp(sigmar * recdevs[, this_run_num] - (sigmar^2)/2)

      # Add new rec devs overwriting om/ss3.par
      change_rec_devs(recdevs_new = sc_i_recdevs, file_in =
        pastef(sc, i, "om", "ss3.par"), file_out = pastef(sc, i,
          "om", "ss3.par"))


      #wd <- getwd()
      #setwd(pastef(sc, i, "om"))
      # Change M
      #with(m_params,
        #change_m(n_blocks            = n_blocks,
                 #block_pattern       = block_pattern,
                 #dev                 = dev,
                 #ctl_file_in         = "om.ctl",
                 #ctl_file_out        = "om.ctl",
                 #dat_file            = "data.dat",
                 #dat_file_out        = "data.dat",
                 #how_time_varying    = how_time_varying) 
      #setwd(wd)

      # Change F
      with(f_params,
        change_f(years               = years,
                 years_alter         = years_alter,
                 fvals               = fvals,
                 file_in             = pastef(sc, i, "om", "ss3.par"),
                 file_out            = pastef(sc, i, "om", "ss3.par")))

      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om", ...)

      # Survey biomass index
      with(index_params, 
        change_index(dat_file_in     = pastef(sc, i, "om", "data.dat"), 
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

      run_ss3model(scenarios = sc, iterations = i, type = "em", ...)

      # Should we run bias correction? We should if bias_correct is
      # true, and we are done running bias corrections (i.e. we're on
      # the last "bias" iteration), and we haven't already run this
      # yet.
      if(bias_correct & i == pastef("bias", bias_nsim) & !bias_already_run) { 
        run_bias_ss3(dir = pastef(sc, "bias"), outdir = pastef(sc,
            "bias"), nsim = bias_nsim)
        bias_already_run <- TRUE
      # Since we've now run bias correction routine, copy the .ctl on
      # subsequent iterations
      } 

    } # end iterations
  } # end scenarios
}

