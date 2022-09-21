# Tasks To-Do
# write a test in test to see if everything works

# Tasks Completed
# Use one value per fleet, not all fleets need to have a value
    # function below does this
# Check to make sure that all values in list are in ageing error definitions
    # error check in for loop does this, had to be in for loop otherwise only
    # the first value in fleet and definitions was checked
# look up values to see which values
# need to be changed, then change the value to the given integer from the list
    # function below looks up fleet and applies new definition
# return the dat list so it can be used by the em
    # function below returns invisible dat_list
# In simdf use "ca." to send arguments to change_em_ageerr
    # Example, ca.definition.1 <- 2 would change Ageerr def of fleet 1 to 2
# Next after creating function change ss3sim_base to use the function
    # added code to run change_em_ageerr function if em_ageerr_params
    # are present in ss3sim_base.R (lines 642-652)
# Change scenario stuff to use ca.
    #added c("ca", "em_ageerr_params") to lookuptable in setup_scenarios.R (line 256)

# Example input for ageerr_def
#em_ageerr_params <- list(
#  "fleets" = c(1:3,5),
#  "definition" = c(1, 3, 2, 1)
#)

change_em_ageerr <- function(dat_list, outfile = NULL, fleets,
definition) {
  #check if Ageerr is being changed
  change_err <- ifelse(is.null(definition), FALSE, TRUE)
  if (change_err){
      for(i in 1:length(fleets)){
        #check if definition and fleet are in dat file
        if (!definition[i] %in% seq(dat_list$N_ageerror_definitions)) {
          stop("Supplied ageing error definition is not in your dat file.")
        }
        if (!fleets[i] %in% unique(dat_list$agecomp$FltSvy)) {
          stop("Supplied fleet is not in your dat file. Ageing error 
          definition cannot be changed.")
        }
        # the following code looks up values to see which fleets need
        # to be changed then changes the Ageerr value to the given
        # integer from the list
        dat_list$agecomp$Ageerr[dat_list$agecomp$FltSvy 
        %in% fleets[i]] <- definition[i]
      }

  }
  # write the dat file so it can be used by the em
    if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }
  invisible(dat_list)
}


#test code to run change_em_age_err function
#this is called the same way ss3sim_base calls the function
#test<-do.call("change_em_ageerr", c(
#        dat_list         = list(dat_list),
#        outfile          = NULL,
#        em_ageerr_params
#))
#test$agecomp$Ageerr
