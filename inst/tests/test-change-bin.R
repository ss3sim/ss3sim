context("change_bin() is working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

test_that("change_bin() throws an error or warning if bins aren't appropriate ", {
   expect_error(change_bin(file_in = "", file_out = "", bin_vector = c("a", "b")))
   expect_error(change_bin(file_in = "", file_out = "", bin_vector = 1))
})

test_that("change_bin() works on the cod model", {
  f <- system.file("extdata/models/cod-om/codOM.dat", package = "ss3sim")
  change_bin(f, file_out = "test.dat", bin_vector = seq(2, 8, 2))
  fl <- readLines("test.dat")
  line <- fl[grep("N_Length_comp_observations", fl) + 1]
  expect_equal(substr_r(line, 11), "l2 l4 l6 l8")
})

unlink("test.dat")
setwd(wd)
