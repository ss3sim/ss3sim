context("Verify input")

test_that("verify_input picks up a missing (data) file", {
    d <- system.file("extdata", package = "ss3sim")
    om <- paste0(d, "/models/cod-om")
    file.copy(om, ".", recursive = TRUE)
    file.remove("cod-om/codOM.dat")
  expect_error({
    verify_input(model_dir = "cod_om", type = "om")
  }
  )
    unlink("cod-om", recursive = TRUE)
})
