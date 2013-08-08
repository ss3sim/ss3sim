# Look at the benefit of increasing survey effort to halve survey CV

# set the working directory to *this* directory, for example:
# setwd("~/src/ss3sim/inst/examples/")

library(ss3sim)

x0 <- get_caseargs("cases", "D0-E0-F0-G0-R0-S0-M0-cod")
x1 <- get_caseargs("cases", "D0-E0-F0-G0-R0-S0-M0-cod")
dx0 <- get_caseargs("cases", "D0-E100-F0-G0-R0-S0-M0-cod") 
dx1 <- get_caseargs("cases", "D0-E100-F0-G0-R0-S0-M0-cod") 
recdevs_det <- matrix(0.001, nrow=100, ncol=20)

# deterministic cases:
with(dx0, run_ss3sim(
  iterations = 20, 
  scenarios = "D0-E100-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = "../extdata/models/cod-om/", 
  em_model_dir = "../extdata/models/cod-em/",
  sleep = 20,
  user_recdevs = recdevs_det))

with(dx1, run_ss3sim(
  iterations = 20, 
  scenarios = "D1-E100-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = "../extdata/models/cod-om/", 
  em_model_dir = "../extdata/models/cod-em/",
  sleep = 20,
  user_recdevs = recdevs_det))

# stochastic cases:
with(x0, run_ss3sim(
  iterations = 100, 
  scenarios = "D0-E0-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = "../extdata/models/cod-om/", 
  em_model_dir = "../extdata/models/cod-em/",
  sleep = 30))

with(x1, run_ss3sim(
  iterations = 100, 
  scenarios = "D1-E0-F0-G0-R0-S0-M0-cod", 
  bias_adjust = TRUE,
  m_params = M, f_params = F, index_params = index, 
  lcomp_params = lcomp, agecomp_params = agecomp, 
  estim_params = E, 
  om_model_dir = "../extdata/models/cod-om/", 
  em_model_dir = "../extdata/models/cod-em/",
  sleep = 30))

