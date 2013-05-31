context("Case parsing")

test_that("Single digit cases get picked up", {
  expect_equal(get_caseval("M1-D2", "M"), 1L)
  expect_equal(get_caseval("M2-D2", "M"), 2L)
})

test_that("Double and triple digit cases get picked up", {
  expect_equal(get_caseval("M10-D2", "M"), 10L)
  expect_equal(get_caseval("M100-D2", "M"), 100L)
})

test_that("Delimiters error", {
  expect_error(get_caseval("M1D2", "M"))
})

