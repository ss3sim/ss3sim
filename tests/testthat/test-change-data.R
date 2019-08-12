# context("change_data() is working")

# temp_path <- file.path(tempdir(), "ss3sim-test")
# dir.create(temp_path, showWarnings = FALSE)
# wd <- getwd()
# setwd(temp_path)

# test_that("change_bin() throws an error or warning if bins aren't appropriate ", {
#    expect_error(change_bin(file_in = "", file_out = "", bin_vector = c("a", "b")))
#    expect_error(change_bin(file_in = "", file_out = "", bin_vector = 1))
# })

# test_that("change_data() works on the cod model for lengths", {
#   f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
#   change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "len")
#   temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
#   temp <- change_fltname(temp)
#   expect_equal(temp$lbin_vector, seq(2, 8, 2))
#   expect_equal(temp$N_lbins, 4)
#   expect_equal(ncol(temp$lencomp), 10)
# })
#
# test_that("change_bin() works on the sardine model for lengths", {
#   f <- system.file("extdata/models/sar-om/SardOM.dat", package = "ss3sim")
#   change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "len")
#   temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
#   temp <- change_fltname(temp)
#   expect_equal(temp$lbin_vector, seq(2, 8, 2))
#   expect_equal(temp$N_lbins, 4)
#   expect_equal(ncol(temp$lencomp), 10)
# })
#
# test_that("change_bin() works on the cod model for ages", {
#   f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
#   change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "age")
#   temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
#   temp <- change_fltname(temp)
#   expect_equal(temp$agebin_vector, seq(2, 8, 2))
#   expect_equal(temp$N_agebins, 4)
#   expect_equal(ncol(temp$agecomp), 13)
# })
#
# # test_that("change_bin() works on the cod model for mla type", {
# #   f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
# #   change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "mla")
# #   temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
# #   temp <- change_fltname(temp)
# #   expect_equal(temp$N_MeanSize_at_Age_obs, seq(2, 8, 2))
# #   expect_equal(temp$N_agebins, 4)
# #   expect_equal(ncol(temp$agecomp), 13)
# # })
#
# unlink("test.dat")
# setwd(wd)

# below: old tail compression tests that were in a separate script: ------------
# tail compression now done in change data

# temp_path <- file.path(tempdir(), "ss3sim-test")
# dir.create(temp_path, showWarnings = FALSE)
# wd <- getwd()
# setwd(temp_path)
#
# d <- system.file("extdata", package = "ss3sim")
# om <- paste0(d, "/models/cod-om")
# em <- paste0(d, "/models/cod-em")
# case_folder <- paste0(d, "/eg-cases")

# The following test also needs to be reworked, because change_fltname is
# no longer a function.
# test_that("change_tail_compression changes the tail compression value", {
#   dat_file <- system.file("extdata", "example-om", "data.ss_new",
#     package = "ss3sim")
#   dat_file <- r4ss::SS_readdat(dat_file, version = NULL, verbose = FALSE)
#   dat_file <- change_fltname(dat_file)
#   test_tc <- change_tail_compression(tail_compression = .1234, dat_file,
#     dat_file_out = "test.dat")
#   expect_equal(test_tc$comp_tail_compression, 0.1234)
# })

# The following test needs to be re-worked:

# test_that("change_tail_compression works with ss3sim_base", {
#
#   skip_on_cran()
# #   library("doParallel")
# #   library("foreach")
# #   registerDoParallel(cores = 2)
# #
# # change_tail_compression is now called from within change_data()
# # need to re-work this test:
# #
# #   # T0 = no tail compression, T1 = 0.3 tail compression:
# #   run_ss3sim(iterations = 1,
# #     scenarios = c("D0-F0-T0-cod", "D0-F0-T1-cod"),
# #     case_folder = case_folder, om_dir = om,
# #     em_dir = em, case_files = list(F = "F",
# #       D = c("index", "lcomp", "agecomp"), T = "tail_compression"),
# #     ss_mode = "optimized", parallel = TRUE)
# #
# #   # quickly grab results to see if any difference:
# #   get_results_all(user_scenarios =
# #       c("D0-F0-T0-cod", "D0-F0-T1-cod"))
# #
# #   results <- read.csv("ss3sim_scalar.csv")
# #
# #   expect_false(results$NLL_TOTAL_em[1] == results$NLL_TOTAL_em[2])
# #
# #   unlink(c("ss3sim_scalar.csv", "ss3sim_ts.csv"))
# #   unlink("D0-F0-T0-cod", TRUE)
# #   unlink("D0-F0-T1-cod", TRUE)
# })
#
# setwd(wd)

