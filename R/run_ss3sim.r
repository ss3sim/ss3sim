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
#' @export
#' @details
#' This function is written to be flexible. You can specify the index,
#' lcomp, and agecomp parameters in the function call. Eventually the
#' plan is to create a script which derives the parameters from the
#' case identifiers and calls this function. That will be a FISH600
#' specific function.
#'
#' Note that bias adjustment isn't implemented yet.

# todo change params to args
# todo change iter to reps

run_ss3sim <- function(iterations, scenarios, m_params, f_params,
  index_params, lcomp_params, agecomp_params, om_model_dir,
  em_model_dir, bias_correct = FALSE, bias_nsim = 5) {

   warning("Manipulation steps have not been tested yet!")

  for(sc in scenarios) {
    for(i in iterations) {

      # Create folders, copy models, check for necessary files, rename
      # files for consistency
      copy_ss3models(model_dir = om_model_dir, scenarios = sc,
        iterations = i, type = "om")
      copy_ss3models(model_dir = em_model_dir, scenarios = sc,
        iterations = i, type = "em")

      # Should we run bias correction?
      if(i == min(iterations) & bias_correct) {
        # Make the folders for bias correction
        for(j in 1:bias_nsim) {
          dir.create(pastef(sc, "bias", j), showWarnings = FALSE,
            recursive = TRUE)
        }
        # And run the bias correction routine
        run_bias_ss3(dir = pastef(sc, "bias"), outdir = pastef(sc,
            "bias"), nsim = bias_nsim)
      } # end bias correction runs

      # If we're bias correcting, then copy over the .ctl file to the
      # em folder
      if(bias_correct) {
        file.copy(from = pastef(sc, "bias", "em.ctl"), to = pastef(sc,
            i, "em"), overwrite = TRUE)
      }

      # Pull in sigma R from the operating model
      sigmar <- get_sigmar(pastef(sc, i, "om", "om"))

      # Exponentiate with bias adjustment
      # recdevs is a 100x100 matrix stored in the package 'data' folder
      # Columns are for iterations and rows are for years
      sc_i_recdevs <- exp(sigmar * recdevs[, i] - (sigmar^2)/2)

      # Add new rec devs overwriting om/ss3.par
      change_rec_devs(recdevs_new = sc_i_recdevs, file_in =
        pastef(sc, i, "om", "ss3.par"), file_out = paste_f(sc, i,
          "om", "ss3.par"))

      # Change M
      with(m_params,
        change_m(n_blocks          = n_blocks,
                 block_pattern     = block_pattern,
                 dev               = dev,
                 ctl_file_in       = pastef(sc, i, "om", "om.ctl"),
                 ctl_file_out      = pastef(sc, i, "om", "om.ctl"),
                 dat_file          = pastef(sc, i, "om", "data.dat"),
                 par_file          = pastef(sc, i, "om", "ss3.par"),
                 how_time_varying  = how_time_varying)) 

      # Change F
      with(f_params,
        change_f(years       = years,
                 years_alter = years_alter,
                 fvals       = fvals,
                 file_in     = pastef(sc, i, "om", "ss3.par"),
                 file_out    = pastef(sc, i, "om", "ss3.par")))

      # Run the operating model
      run_ss3model(scenarios = sc, iterations = i, type = "om")

      # Survey biomass index
      with(index_params, 
        change_index(dat_file_in    = pastef(sc, i, "om", "data.dat"), 
                     dat_file_out   = pastef(sc, i, "em", "data.dat"),
                     start_surv     = start_surv,
                     end_surv       = end_surv))

      # Add error in the length comp data
      SS.dat = r4ss::SS_readdat(pastef(sc, i, "em", "data.dat"))
      with(lcomp_params,
        change_lcomp(infile       = SS.dat,
                     outfile      = pastef(sc, i, "em", "data.dat"),
                     distribution = multinomial,
                     Nsamp        = Nsamp,
                     minyear      = minyear,
                     maxyear      = maxyear,
                     years        = years,
                     svyears      = svyears,
                     lbin_method  = lbin_method,
                     binwidth     = binwidth,
                     minimum_size = minimum_size,
                     maximum_size = maximum_size,
                     N_lbins      = N_lbins,
                     lbin_vector  = lbin_vector,
                     lencomp      = lencomp))

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

      run_ss3model(scenarios = sc, iterations = i, type = "em")
    } # end iterations
  } # end scenarios
}

