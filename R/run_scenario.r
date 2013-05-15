#' Run both the operating model and assessment model
#'
#' This function is a wrapper function that can call \code{run_model} for
#' the operating model, manipulate the output (add recruitment
#' deviations, survey the data, etc.), and run the estimation model.
#'
#' @param scenarios Which scenarios to run.
#' @param iterations Which iterations to run.
#' @param index_params A named list containing all the \code{jitter_index} options.
#' @param lcomp_params A named list containing all the \code{change_lcomp} options.
#' @param agecomp_params A named list containing all the \code{change_agecomp} options.
#' @param type Are you running the both the operating and estimation models or just one
#' or the other?
#' @export

run_scenario <- function(scenarios, iterations, index_params =
  "default", lcomp_params = "default", agecomp_params = "default",
  type = c("om and em", "om", "em")) {

  # create default list objects
  #if(index_params == "default") data(default_index_params)
  #if(lcomp_params == "default") data(default_lcomp_params)
  #if(agecomp_params == "default") data(default_agecomp_params)

  switch(type,
    "om and em" = {
      for(sc in scenarios) {
        for(i in iterations) {

          # Run the operating model
          run_model(scenarios = sc, iterations = i, type = "om")

          # Add rec devs
          data(recdevs)

          # Pull in the SD of the rec devs here...
          sd_recdev <- 1 # Hardcoded in the mean time

          # Add new rec devs overwriting ss3.par
          change_rec_devs(recdevs_new = sd_recdev * recdevs[, i])

          # Surveys
          with(index_params, 
            jitter_index(dat_file_in    = pastef(sc, i, "om", "data.dat"), 
                         dat_file_out   = pastef(sc, i, "em", "data.dat"),
                         start_surv     = start_surv,
                         end_surv       = end_surv,
                         start_fish     = start_fish,
                         end_fish       = end_fish,
                         freq_surv      = freq_surv,
                         sd_obs_surv    = sd_obs_surv,
                         freq_fish      = freq_fish,
                         sd_obs_fish    = sd_obs_fish,
                         make_plot      = make_plot,
                         use_index      = use_index))

          # Add error in the length comp
          SS.dat = r4ss::SS_readdat(pastef(sc, i, "em", "data.dat"))
          with(lcomp_params, 
            change_lcomp(infile         = SS.dat, 
                         outfile        = pastef(sc, i, "em", "data.dat"),
                         distribution   = distribution,
                         Nsamp          = Nsamp,
                         minyear        = minyear,
                         maxyear        = maxyear,
                         years          = years,
                         svyears        = svyears))

          # Add error in the age comp
          SS.dat2 = r4ss::SS_readdat(pastef(sc, i, "em", "data.dat"))
          with(agecomp_params, 
            change_agecomp(infile       = SS.dat2,
                           outfile      = pastef(sc, i, "em", "data.dat"),
                           distribution = distribution,
                           Nsamp        = Nsamp,
                           minyear      = minyear,
                           maxyear      = maxyear,
                           years        = years,
                           svyears      = svyears))

          warning("Manipulation steps aren't all implemented or checked yet!")

          run_model(scenarios = sc, iterations = i, type = "em")
        } # end iterations
      } # end scenarios
    }, # end "om and em" switch option
    "om" = run_model(scenarios, iterations, type = "om"),
    "em" = run_model(scenarios, iterations, type = "em")
    ) # end switch() call
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

