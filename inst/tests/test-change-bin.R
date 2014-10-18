context("change_bin() is working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

test_that("change_bin() throws an error or warning if bins aren't appropriate ", {
   expect_error(change_bin(file_in = "", file_out = "", bin_vector = c("a", "b")))
   expect_error(change_bin(file_in = "", file_out = "", bin_vector = 1))
})

test_that("change_bin() works on the cod model for lengths", {
  f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
  change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "len")
  temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
  expect_equal(temp$lbin_vector, seq(2, 8, 2))
  expect_equal(temp$N_lbins, 4)
  expect_equal(ncol(temp$lencomp), 10)
})

test_that("change_bin() works on the sardine model for lengths", {
  f <- system.file("extdata/models/sar-om/SardOM.dat", package = "ss3sim")
  change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "len")
  temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
  expect_equal(temp$lbin_vector, seq(2, 8, 2))
  expect_equal(temp$N_lbins, 4)
  expect_equal(ncol(temp$lencomp), 10)
})

test_that("change_bin() works on the cod model for ages", {
  f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
  change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "age")
  temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
  expect_equal(temp$agebin_vector, seq(2, 8, 2))
  expect_equal(temp$N_agebins, 4)
  expect_equal(ncol(temp$agecomp), 13)
})

# test_that("change_bin() works on the cod model for mla type", {
#   f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
#   change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2), type = "mla")
#   temp <- r4ss::SS_readdat("test.dat", verbose = FALSE)
#   expect_equal(temp$N_MeanSize_at_Age_obs, seq(2, 8, 2))
#   expect_equal(temp$N_agebins, 4)
#   expect_equal(ncol(temp$agecomp), 13)
# })

unlink("test.dat")
setwd(wd)
