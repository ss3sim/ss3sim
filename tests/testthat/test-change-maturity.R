context("change_maturity() is working")

temp_path <- file.path(tempdir(), "ss3sim-test")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)

test_that("change_maturity() throws an error if maturity_option is not appropriate", {
  expect_error(change_maturity(maturity_option = 6L))
  expect_error(change_maturity(maturity_option = 1.5))
  expect_error(change_maturity(maturity_option = "abc"))
  expect_error(change_maturity(maturity_option = c(1L, 2L)))
})

test_that("change_maturity() changes the option from 1 to 5 for the cod model", {
  f <- system.file("extdata/models/cod-em/codEM.ctl", package = "ss3sim")
  change_maturity(f, "test.ctl", maturity_option = 5L)
  fl <- readLines("test.ctl")
  mat_value <- as.numeric(substr(fl[grep("maturity_option", fl)], 1, 1))
  expect_equal(mat_value, 5L)
 })

test_that("change_maturity() changes the option from 1 to 5 for the flatfish model", {
  f <- system.file("models/flatfish/em/ss3.ctl", package = "ss3models")
  change_maturity(f, "test.ctl", maturity_option = 5L)
  fl <- readLines("test.ctl")
  mat_value <- as.numeric(substr(fl[grep("maturity_option", fl)], 1, 1))
  expect_equal(mat_value, 5L)
 })

test_that("change_maturity() changes the option from 1 to 2 for the flatfish model", {
  f <- system.file("models/flatfish/em/ss3.ctl", package = "ss3models")
  change_maturity(f, "test.ctl", maturity_option = 2L)
  fl <- readLines("test.ctl")
  mat_value <- as.numeric(substr(fl[grep("maturity_option", fl)], 1, 1))
  expect_equal(mat_value, 2L)
})

unlink("test.ctl")
setwd(wd)
