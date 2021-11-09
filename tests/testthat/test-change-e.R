context("Changing EM binning and parameters in an EM")
wd.old <- getwd()
temp_path <- file.path(tempdir(), "pars")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
#use OM because has a datafile, unlike codEM< and is a 1 sex model.
file.copy(file.path(d, "models", "cod-om"), ".", recursive = TRUE)
setwd("cod-om")
on.exit(setwd(wd.old), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)
datalist <- r4ss::SS_readdat("codOM.dat", verbose = F, version = NULL)


test_that("change_e changes forecast year successfully", {
  nfors <- 10
  # Manipulate files
  datnew <- suppressWarnings(change_e(ctl_file_in = "codOM.ctl",
    ctl_file_out = "change.ctl",
    dat_list = datalist, for_file_in = "forecast.ss",
    forecast_num = nfors))
  test <- readLines("forecast.ss")
  t1 <- grep("#_Forecast", test)
  t2 <- grep("#_Nforecastyrs", test)
  expect_true(as.numeric(strsplit(test[t1], "#")[[1]][1]) == 2)
  expect_true(as.numeric(strsplit(test[t2], "#")[[1]][1]) == nfors)
  expect_true(datnew$endyr == datalist$endyr - nfors)
})

test_that("change_e works as expected", {
 output <- suppressWarnings(change_e(ctl_file_in = "codOM.ctl",
                            ctl_file_out = "change_e.ctl",
                            dat_list = datalist,
                            for_file_in = "forecast.ss",
                            natM_type = NULL,
                            natM_n_breakpoints = NULL,
                            natM_lorenzen = NULL,
                            natM_val = NULL,
                            par_name = c("SR_BH_steep", "SizeSel_P1_Fishery"),
                            par_int = c(0.3, 40),
                            par_phase = c(3, 2),
                            forecast_num = 0))
 new_ctl <- suppressWarnings(r4ss::SS_parlines("change_e.ctl"))
 steep_line <- which(new_ctl$Label == "SR_BH_steep")
 sel_line <- which(new_ctl$Label == "SizeSel_P1_Fishery(1)")
 expect_equal(new_ctl[steep_line, "INIT"], 0.3)
 expect_equal(new_ctl[steep_line, "PHASE"], 3)
 expect_equal(new_ctl[sel_line, "INIT"], 40)
 expect_equal(new_ctl[sel_line, "PHASE"], 2)
})
test_that("change_e M inputs are properly deprecated", {
  expect_error(change_e(ctl_file_in = "codOM.ctl",
                            ctl_file_out = "change_e.ctl",
                            dat_list = datalist,
                            for_file_in = "forecast.ss",
                            natM_type = "n_breakpoints",
                            natM_n_breakpoints = NULL,
                            natM_lorenzen = NULL,
                            natM_val = NULL,
                            par_name = c("SR_BH_steep", "SizeSel_P1_Fishery"),
                            par_int = c(0.3, 40),
                            par_phase = c(3, 2),
                            forecast_num = 0),
  "Parameters in change_e: natM_type, natM_n_breakpoints, natM_lorenzen, and natM_val have been deprecated",
  fixed = TRUE)
  expect_error(change_e(ctl_file_in = "codOM.ctl",
                        ctl_file_out = "change_e.ctl",
                        dat_list = datalist,
                        for_file_in = "forecast.ss",
                        natM_type = NULL,
                        natM_n_breakpoints = c(1, 4),
                        natM_lorenzen = NULL,
                        natM_val = NULL,
                        par_name = c("SR_BH_steep", "SizeSel_P1_Fishery"),
                        par_int = c(0.3, 40),
                        par_phase = c(3, 2),
                        forecast_num = 0),
               "Parameters in change_e: natM_type, natM_n_breakpoints, natM_lorenzen, and natM_val have been deprecated",
               fixed = TRUE)
  expect_error(change_e(ctl_file_in = "codOM.ctl",
                        ctl_file_out = "change_e.ctl",
                        dat_list = datalist,
                        for_file_in = "forecast.ss",
                        natM_type = NULL,
                        natM_n_breakpoints = NULL,
                        natM_lorenzen = NULL,
                        natM_val = c(.2, 3, 0.4, 5),
                        par_name = c("SR_BH_steep", "SizeSel_P1_Fishery"),
                        par_int = c(0.3, 40),
                        par_phase = c(3, 2),
                        forecast_num = 0),
               "Parameters in change_e: natM_type, natM_n_breakpoints, natM_lorenzen, and natM_val have been deprecated",
               fixed = TRUE)
  expect_error(change_e(ctl_file_in = "codOM.ctl",
                        ctl_file_out = "change_e.ctl",
                        dat_list = datalist,
                        for_file_in = "forecast.ss",
                        natM_type = NULL,
                        natM_n_breakpoints = NULL,
                        natM_lorenzen = c(0.1,0.2,0.3),
                        natM_val = NULL,
                        par_name = c("SR_BH_steep", "SizeSel_P1_Fishery"),
                        par_int = c(0.3, 40),
                        par_phase = c(3, 2),
                        forecast_num = 0),
               "Parameters in change_e: natM_type, natM_n_breakpoints, natM_lorenzen, and natM_val have been deprecated",
               fixed = TRUE)

})

test_that("change_em_binning works with method = 1", {
  new_bin_vec <- seq(min(datalist$lbin_vector), max(datalist$lbin_vector), by = 4)
  # add the max value if necessary.
  if(new_bin_vec[length(new_bin_vec)] != datalist$lbin_vector[length(datalist$lbin_vector)]){
    new_bin_vec <- c(new_bin_vec,
                     datalist$lbin_vector[length(datalist$lbin_vector)])
  }
  output <- change_em_binning(dat_list = datalist,
                              bin_vector = new_bin_vec,
                              lbin_method = 1)
  expect_true(is.null(output$binwidth))
  expect_true(is.null(output$minimum_size))
  expect_true(is.null(output$maximum_size))
  expect_equal(output$lbin_method, 1)
  expect_equal(output$lbin_vector, new_bin_vec)
  expect_equal(new_bin_vec, output$lbin_vector)
  expect_equal(new_bin_vec, output$lbin_vector_pop)
  expect_equal(ncol(output$lencomp)-6, length(new_bin_vec))
})

test_that("change_em_binning works with method = 2", {
  new_bin_vec <- seq(min(datalist$lbin_vector), max(datalist$lbin_vector), by = 4)
  # add the max value if necessary.
  if(new_bin_vec[length(new_bin_vec)] != datalist$lbin_vector[length(datalist$lbin_vector)]){
    new_bin_vec <- c(new_bin_vec,
                     datalist$lbin_vector[length(datalist$lbin_vector)])
  }
  pop_bin_input <- 5
  pop_min_size_input <- min(datalist$lbin_vector_pop) - 1
  pop_max_size_input <- max(datalist$lbin_vector_pop) + 5
  lbin_vec_pop <-seq(pop_min_size_input,
                     pop_max_size_input,
                     length.out = (pop_max_size_input - pop_min_size_input)/
                                   pop_bin_input + 1
                     )
  output <- change_em_binning(dat_list = datalist,
                              bin_vector = new_bin_vec,
                              lbin_method = 2,
                              pop_binwidth = pop_bin_input,
                              pop_minimum_size = pop_min_size_input,
                              pop_maximum_size = pop_max_size_input)
  expect_equal(output$lbin_method, 2)
  expect_equal(output$binwidth, pop_bin_input)
  expect_equal(output$minimum_size, pop_min_size_input)
  expect_equal(output$maximum_size, pop_max_size_input)
  expect_equal(new_bin_vec, output$lbin_vector)
  expect_equal(output$lbin_vector_pop, lbin_vec_pop)
  expect_equal(ncol(output$lencomp)-6, length(new_bin_vec))
})

test_that("change_em_binning works with cond. age at length", {
  # a valid bin vector when there is CAL must only include values that are in
  # the population bins, supposedly.
  #I think this test is broken and needs to be fixed.
 datalist_CAL <- datalist
 pop_bin_input <- 2
 pop_min_size_input <- min(datalist$lbin_vector_pop)
 pop_max_size_input <- max(datalist$lbin_vector_pop)
 new_bin_vec <- c(20, 50, 152)
 # change approximately half of the obs to CAL
 a_col <- nrow(datalist_CAL$agecomp)
 max_change <- 20
 datalist_CAL$agecomp$Lbin_lo[1:max_change] <- rep(datalist_CAL$lbin_vector, length.out = max_change)
 datalist_CAL$agecomp$Lbin_hi[1:max_change] <- rep(datalist_CAL$lbin_vector, length.out = max_change)
 new_dat <- change_em_binning(dat_list = datalist_CAL,
                                bin_vector = new_bin_vec,
                                lbin_method = 2,
                                pop_binwidth = pop_bin_input,
                                pop_minimum_size = pop_min_size_input,
                                pop_maximum_size = pop_max_size_input)
 new_cal <- new_dat[["agecomp"]][new_dat$agecomp$Lbin_lo != -1, ]
 old_cal <- datalist_CAL[["agecomp"]][datalist_CAL$agecomp$Lbin_lo != -1, ]
 expect_true(all(new_cal$Lbin_lo %in% c(-1, new_bin_vec)))
})


test_that("change_EM_binning stops on error when expected", {
  expect_error(change_em_binning(dat_list = datalist, bin_vector = c("a", "b"),
                                 lbin_method = 1),
               "bin_vector must be numeric")
  expect_error(change_em_binning(dat_list = datalist,
                                 bin_vector = c(1,datalist$lbin_vector),
                                 lbin_method = 1),
               "The specified bin_vector is longer than the original")
  expect_error(change_em_binning(dat_list = datalist,
                                 bin_vector = seq(1, 6, length.out =
                                                  length(datalist$lbin_vector)),
                                 lbin_method = 2),
               "you must specify all pop bin parameters if using lbin_method=2")
  orig <- datalist$Ngenders
  datalist$Ngenders <- 2
  expect_error(change_em_binning(dat_list = datalist,
                                 bin_vector = seq(1, 6, length.out =                                                                                    length(datalist$lbin_vector)),
                                 lbin_method = 1),
               "_Ngenders is greater than 1 in the model.")
  expect_error(change_em_binning(dat_list = datalist,
                                 bin_vector = seq(1, 6, length.out =                                                                                    length(datalist$lbin_vector)),
                                 lbin_method = 1),
               "_Ngenders is greater than 1 in the model.")
  # todo: think about what these two tests really mean
  # datalist$Ngenders <- orig
  # tmp_lbin_max <- datalist$lbin_vector[length(datalist$lbin_vector)]
  # tmp_lbin_vec <- datalist$lbin_vector[-(length(datalist$lbin_vector))]
  # tmp_lbin_vec <- c(tmp_lbin_vec, (tmp_lbin_max+1))
  # expect_error(change_em_binning(dat_list    = datalist,
  #                                bin_vector  = tmp_lbin_vec,
  #                                lbin_method = 1),
  #   "The maximum value in the bin_vector is not equal to the original maximum length bin value.")
  # tmp_lbin_min <- datalist$lbin_vector[1]
  # tmp_lbin_vec <- datalist$lbin_vector[-1]
  # tmp_lbin_vec <- c((tmp_lbin_min+1), tmp_lbin_vec)
  # expect_error(change_em_binning(dat_list = datalist,
  #                                bin_vector = tmp_lbin_vec,
  #                                lbin_method = 1),
  #              "The minimum value in the bin_vector is not equal to the ")
})
