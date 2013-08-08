# Look at the benefit of increasing survey effort to halve survey CV
library(ss3sim)

# get local folder locations:
d <- system.file("extdata", package = "ss3sim")
case_folder <- paste0(d, "/eg-cases")
om <- paste0(d, "/models/cod-om")
em <- paste0(d, "/models/cod-em")

# get case arguments:
x0 <- get_caseargs(case_folder, "D0-E0-F0-G0-R0-S0-M0-cod")
x1 <- get_caseargs(case_folder, "D1-E0-F0-G0-R0-S0-M0-cod")
dx0 <- get_caseargs(case_folder, "D0-E100-F0-G0-R0-S0-M0-cod") 
dx1 <- get_caseargs(case_folder, "D1-E100-F0-G0-R0-S0-M0-cod") 
recdevs_det <- matrix(0.001, nrow=100, ncol=20)

# run deterministic cases:
with(dx0, run_ss3sim(
  iterations = 1, 
  scenarios = "D0-E100-F0-G0-R0-S0-M0-cod", 
  bias_adjust = FALSE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = om, 
  em_model_dir = em,
  user_recdevs = recdevs_det))

with(dx1, run_ss3sim(
  iterations = 1:20, 
  scenarios = "D1-E100-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = om, 
  em_model_dir = em,
  user_recdevs = recdevs_det))

# run stochastic cases:
with(x0, run_ss3sim(
  iterations = 1:100, 
  scenarios = "D0-E0-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = om, 
  em_model_dir = em))

with(x1, run_ss3sim(
  iterations = 1:100, 
  scenarios = "D1-E0-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = om, 
  em_model_dir = em))

