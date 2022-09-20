rm(list=ls());  options(show.error.locations = TRUE)
library(r4ss)
dir_in<- "C:/Users/derekc/Desktop/My Files/Research/Florida/Age_Error_Simulation/GT_Simulation_OM_EM/GT_test/om_precision"
ssfiles <- r4ss::SS_read(dir_in, verbose = FALSE)
ssfiles$dat$agecomp$Ageerr

names(ssfiles$dat)
ssfiles$dat$N_ageerror_definitions
ssfiles$dat$ageerror

defs <- data.frame(
  "fleet" = 1:2,
  "definition" = c(1, 1)
)
# Example input for ageerr_def
# Use one value per fleet, not all fleets need to have a value
list(1 = 2, 2 = 2, 3 = 1)
# Check to make sure that all values in list are in ageing error definitions
# use the following function to look up values to see which values
# need to be changed, then change the value to the given integer from the list
?match
# return the dat list so it can be used by the em
# In simdf use "ca." to send arguments to change_em_ageerr
change_em_ageerr <- function(dat_list, ageerr_def) {
  if (!age_err_def %in% seq(dat_list$N_ageerror_definitions)) {
    stop("Supplied ageing error definition is not in your dat file.")
  }
  dat_list$agecomp$Ageerr<-rep(age_err_def,length(dat_list$agecomp$Ageerr))
  return(dat_list)
}
test<-change_age_error(ssfiles$dat,age_err_def=2)
test$agecomp$Ageerr


# Next after creating function change ss3sim_base to use the function
# Change scenario stuff to use ca.
# write a test in test to see if everything works
