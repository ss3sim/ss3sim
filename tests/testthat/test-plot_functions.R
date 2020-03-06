context("functions in plot_functions work")

data("scalar_dat", package = "ss3sim")

test_that("plot_cummean works", {
  obj <- plot_cummean(scalar_dat,
                      "VonBert_K_Fem_GP_1_em",
                      group = "scenario",
                      use_facet = TRUE)
  expect_type(obj, "list")
  expect_true(length(obj) == 2)
  expect_s3_class(obj$plot, "ggplot")
  expect_s3_class(obj$data, "data.frame")
  expect_true(all(colnames(obj$data) == c("iteration", "scenario",
                                      "VonBert_K_Fem_GP_1_em", "cummean")))
  # group can also be left NULL if only plotting a single scenario.
  # it is recommended to set use_facet FALSE in this case.
  scen_to_use <- unique(scalar_dat$scenario)[1]
  scalar_dat_1_scen <- scalar_dat[scalar_dat$scanario == scen_to_use, ]
  obj2 <- plot_cummean(scalar_dat_1_scen,
                       var = "VonBert_K_Fem_GP_1_em",
                       group = NULL,
                       use_facet = FALSE)
  expect_type(obj2, "list")
  expect_true(length(obj2) == 2)
  expect_s3_class(obj2$plot, "ggplot")
  expect_s3_class(obj2$data, "data.frame")
  expect_true(all(colnames(obj2$data) == c("iteration","VonBert_K_Fem_GP_1_em",
                                           "cummean")))
})
