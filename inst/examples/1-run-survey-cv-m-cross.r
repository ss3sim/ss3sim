# Look at the benefit of increasing survey effort to halve survey CV
# crossed with estimating M

library(ss3sim)

# get local folder locations:
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")

run_fish600(iterations = 1, scenarios = c("D0-E0-F0-G0-R0-S0-M0-cod",
    "D1-E0-F0-G0-R0-S0-M0-cod", "D0-E1-F0-G0-R0-S0-M0-cod",
    "D1-E1-F0-G0-R0-S0-M0-cod"), case_folder = case_folder,
  om_model_dir = om, em_model_dir = em)

    
run_fish600(iterations = 1, scenarios = c("D0-E100-F0-G0-R0-S0-M0-cod",
    "D1-E100-F0-G0-R0-S0-M0-cod", "D0-E101-F0-G0-R0-S0-M0-cod",
    "D1-E101-F0-G0-R0-S0-M0-cod"), case_folder = case_folder,
  om_model_dir = om, em_model_dir = em)



