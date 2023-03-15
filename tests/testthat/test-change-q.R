test_that("Automatic errors in change_q", {
  expect_error(change_q(string_add = "Fishery"))
  expect_error(change_q())
})

test_that("All q's are removed", {
  newctl <- change_q(
    string_remove = c("Fishery", 2),
    ctl_list = codomctl
  )
  expect_equal(NROW(newctl$Q_options), 0)
  expect_equal(NROW(newctl$Q_options), NROW(newctl$Q_parms))
})

test_that("Character removal of fleet name works", {
  newctl <- change_q(
    string_remove = c("Survey"),
    ctl_list = codomctl,
  )
  expect_equal(length(grep("\\(2\\)", row.names(newctl$Q_parms))), 0)
})

test_that("File from change q is written to the disk", {
  newctl <- change_q(
    string_remove = c("Fishery", 2),
    ctl_list = codomctl,
  )
  ctlredo <- change_q(string_add = 1:2, ctl_list = newctl)
  expect_equal(NROW(ctlredo$Q_parms), 2)
})
