# Example input for ageerr_def
defs <- data.frame(
  "fleet" = c(1:3,5),
  "definition" = c(1, 3, 2, 3)
)

change_em_ageerr <- function(dat_list, defs) {
  #check if Ageerr is being changed
  change_err<- ifelse(is.null(defs), FALSE, TRUE)
  if (change_err){
  # the following code looks up values to see which fleets need to be changed,
  # then changes the Ageerr value to the given integer from the list
      for(i in 1:length(defs$fleet)){
        #check if deginition and fleet are in dat file
        if (!defs$definition[i] %in% seq(dat_list$N_ageerror_definitions)) {
          stop("Supplied ageing error definition is not in your dat file.")
        }
        if (!defs$fleet[i] %in% unique(dat_list$agecomp$FltSvy)) {
          stop("Supplied fleet is not in your dat file. Ageing error 
          definition cannot be changed.")
        }
        dat_list$agecomp$Ageerr[dat_list$agecomp$FltSvy 
        %in% defs$fleet[i]] <- defs$definition[i]
      }

  }
  # return the dat list so it can be used by the em
  return(dat_list)
}

# In simdf use "ca." to send arguments to change_em_ageerr
# Next after creating function change ss3sim_base to use the function
# Change scenario stuff to use ca.
# write a test in test to see if everything works
