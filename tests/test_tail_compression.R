### ------------------------------------------------------------
### Code for testing the change_tail_compression

## Copied over from a local environment where testing was initially
## done. Thus it won't run with the CRAN version. This needs to be updated
## at some point. For now I'm leaving it here in case someone wants to fix
## it, adapt it, or get ideas from it. -Cole 8/28/2014.

## Test whether cases are parsed correctly
## stop("Not ready to be sourced, see code comments for reasons")
get_caseargs("cases", scenario = "D0-E0-F0-M0-R0-S0-T0-cod",
             case_files = list(E = "E", D = c("index", "lcomp", "agecomp"), F =
             "F", M = "M", R = "R", S = "S", T="T"))$T

## Run the example simulation with tail compression option
case_folder <- 'cases'
d <- '../inst/extdata'
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")
run_ss3sim(iterations = 1, scenarios =
           c("D0-E0-F0-R0-M0-T0-cod", "D0-E0-F0-R0-M0-T1-cod"),
           case_folder = case_folder, om_dir = om,
           em_dir = em, case_files = list(M = "M", F = "F", D =
    c("index", "lcomp", "agecomp"), R = "R", E = "E", T="T"))
## Make sure it runs with no tail compression option
run_ss3sim(iterations = 1, scenarios =
           c("D0-E0-F0-R0-M0-cod"),
           case_folder = case_folder, om_dir = om,
           em_dir = em)
## quickly grab results to see if any difference
get_results_all(user_scenarios=
                c("D0-E0-F0-R0-M0-T0-cod",
                  "D0-E0-F0-R0-M0-T1-cod",
                  "D0-E0-F0-R0-M0-cod" ))
results <- read.csv("ss3sim_scalar.csv")
print("The first two values should be the same, the third different:")
print( results$NLL_em[c(1,3,2)])

## End of session so clean up
file.remove("ss3sim_scalar.csv", "ss3sim_ts.csv")
unlink("D0-E0-F0-R0-M0-T0-cod", TRUE)
unlink("D0-E0-F0-R0-M0-T1-cod", TRUE)
unlink("D0-E0-F0-R0-M0-cod", TRUE)
### ------------------------------------------------------------
