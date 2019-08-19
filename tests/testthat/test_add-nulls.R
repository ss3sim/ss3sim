test_that("add_nulls works", {

  output <- add_nulls(list(fleets = c(1,2), years = list(1:12, 32:36)),
                      c("fleets", "years", "stdev"))
  expect_equal(names(output), c("fleets", "years", "stdev"))
  expect_equal(output$stdev, NULL)
})
