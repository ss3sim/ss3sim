context("Q setup")

temp_path <- file.path(tempdir(), "q-setup")
dir.create(temp_path)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
dat <- r4ss::SS_readdat(dir(file.path(d, "models", "cod-om"),
  full.names = TRUE, pattern = "\\.dat"
), verbose = FALSE)
ctl <- r4ss::SS_readctl(dir(file.path(d, "models", "cod-om"),
  full.names = TRUE, pattern = "\\.ctl"
),
verbose = FALSE,
use_datlist = TRUE, datlist = dat
)

test_that("Automatic errors in change_q", {
  expect_error(
    expect_warning(change_q(string_add = "Fishery", ctl_file_in = "NotAFile.txt"))
  )
  expect_error(
    expect_warning(change_q(string_add = "Fishery", dat_file_in = "NotAFile.txt"))
  )
  expect_error(change_q(string_add = "Fishery"))
  expect_error(change_q(string_remove = 1:2, dat_list = dat))
  expect_error(change_q(string_remove = 1:2, ctl_list = ctl))
  expect_error(change_q())
})

test_that("Non existent fleets generate errors", {
  expect_warning(newctl <- change_q(
    string_add = "Vessel", ctl_list = ctl, dat_list = dat, verbose = TRUE
  ))
  expect_equal(NROW(newctl$Q_parms), 2)
  newctl <- change_q(string_add = "Vessel", ctl_list = ctl, dat_list = dat)
})

test_that("All q's are removed", {
  newctl <- change_q(
    string_remove = c("Fishery", 2),
    ctl_list = ctl, dat_list = dat, ctl_file_out = NULL
  )
  expect_equal(NROW(newctl$Q_options), 0)
  expect_equal(NROW(newctl$Q_options), NROW(newctl$Q_parms))
})

test_that("Character removal of fleet name works", {
  newctl <- change_q(
    string_remove = c("Survey"),
    ctl_list = ctl, dat_list = dat, ctl_file_out = NULL
  )
  expect_equal(length(grep("\\(2\\)", row.names(newctl$Q_parms))), 0)
})

test_that("File from change q is written to the disk", {
  newctl <- change_q(
    string_remove = c("Fishery", 2),
    ctl_list = ctl, dat_list = dat, ctl_file_out = file.path(temp_path, "test.ctl")
  )
  expect_true(file.exists(file.path(temp_path, "test.ctl")))
  time <- file.info(file.path(temp_path, "test.ctl"))$mtime
  expect_error(change_q(
    string_remove = c("Fishery", 2),
    ctl_list = ctl, dat_list = dat, ctl_file_out = file.path(temp_path, "test.ctl")
  ))
  expect_equal(file.info(file.path(temp_path, "test.ctl"))$mtime, time)
  ctlredo <- change_q(string_add = 1:2, ctl_list = newctl, dat_list = dat)
  expect_equal(NROW(ctlredo$Q_parms), 2)
})

rm(d, dat, ctl)
