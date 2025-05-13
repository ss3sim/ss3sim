test_that("change_em works and model is viable", {
  om_dir <- system.file(
    "extdata", "models", "cod-om",
    package = "ss3sim"
  )
  em_dir <- file.path(tempdir(), "ss3sim")
  dir.create(em_dir, showWarnings = FALSE, recursive = TRUE)
  create_em(om_dir, em_dir)
  expect_true(all(file.exists(
    file.path(em_dir, c("codEM.ctl", "forecast.ss", "starter.ss"))
  )))
  file.copy(
    file.path(om_dir, "codOM.dat"),
    file.path(em_dir, "ss3.dat"),
    overwrite = TRUE
  )
  file.copy(
    file.path(om_dir, "codOM.dat"),
    file.path(em_dir, "ss3.dat"),
    overwrite = TRUE
  )
  results <- r4ss::run(
    dir = em_dir,
    exe = get_bin(),
    skipfinished = FALSE,
    extras = "-nohess -stopph 0",
    verbose = FALSE,
    show_in_console = FALSE
  )
  expect_true(all(results == "ran model"))
})
