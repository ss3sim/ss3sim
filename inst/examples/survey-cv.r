# Look at the benefit of increasing survey effort to halve survey CV

library(ss3sim)

setwd("~/src/ss3sim/inst/examples/")

cases <- get_caseargs(folder = "cases", scenario = "D0-E0-F0-G0-R0-S0-M0-cod")

with(cases, run_ss3sim(iterations = 1, scenarios = "D0-E0-F0-G0-R0-S0-M0-cod", m_params = M, f_params = F, index_params = index, lcomp_params = lcomp, agecomp_params = agecomp, estim_params = E, om_model_dir = "../extdata/models/cod-om/", em_model_dir = "../extdata/models/cod-em/"))


