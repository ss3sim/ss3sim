#' Run both the operating model and assessment model
#'
#' This function is a wrapper function that can call
#' \code{\link{run_ss3model}} for the operating model, manipulate the
#' output (add recruitment deviations, survey the data, etc.), and run
#' the estimation model.
#'
#' @param iterations Which iterations to run. A numeric vector.
#' @param scenarios Which scenarios to run.
#' @param index_params A named list containing all the
#' \code{\link{change_index}} options.
#' @param lcomp_params A named list containing all the
#' \code{\link{change_lcomp}} options.
#' @param agecomp_params A named list containing all the
#' \code{\link{change_agecomp}} options.
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
  em_model_dir) {

  for(sc in scenarios) {
    for(i in iterations) {


      # Pull in sigma R from the operating model
      sigmar <- get_sigmar(pastef(sc, i, "om", "om"))

      # Exponentiate with bias adjustment
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

      warning("Manipulation steps aren't all implemented or checked yet!")

      run_ss3model(scenarios = sc, iterations = i, type = "em")
    } # end iterations
  } # end scenarios
}


    #The OM is in the folder "flatfish OM"
    #For the EM, you have to check the codes in the "model files" folder.

    #Basically, the way I had set-up the simulation setting is that (if not
    #understandable with my code)
    #1. I copy paste all things from the base operating model to another folder.
    #2. then modify this to adapt it to each scenario (change F and choose rec
    #devs)
    #3. copy paste the above files into another folder to then create the
    #assessment model
    #4. modify different things inside this last folder (starter file, dat file)
    #and choose the ctl file that we want to use for estimation (these stl files
    #specify which parameters we want to estimate and which we do not want to)
    #5. run the estimation
    #I don't have Carey's code to do the recruitment bias adjustment in it yet.

