context("Verify input")

test_that("verify_input picks up a missing (data) file", {
    d <- system.file("extdata", package = "ss3sim")
    om <- paste0(d, "/ss3sim_base_eg/cod_om")
    file.copy(om, ".", recursive = TRUE)
    file.remove("cod_om/simple_cod_om.dat")
  expect_error({
    verify_input(model_dir = "cod_om", type = "om")
  }
  )
    unlink("cod_om", recursive = TRUE)
})
