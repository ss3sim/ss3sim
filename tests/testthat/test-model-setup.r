wd.old <- getwd()
temp_path <- file.path(tempdir(), "verify_input")
dir.create(temp_path, showWarnings = FALSE)
setwd(temp_path)
d <- system.file("extdata", package = "ss3sim")
file.copy(file.path(d, "models", "cod-om"), ".", recursive = TRUE)
file.remove("cod-om/codOM.dat")
on.exit(setwd(wd.old), add = TRUE)

test_that("verify_input picks up a missing (data) file", {
  expect_error(verify_input(model_dir = "cod_om", type = "om"))
})

unlink("cod-om", recursive = TRUE)
