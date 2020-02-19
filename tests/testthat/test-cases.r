context("Case parsing")

test_that("Single digit cases get picked up", {
  expect_equal(ss3sim:::get_caseval("M1-D2", "M"), 1L)
  expect_equal(ss3sim:::get_caseval("M2-D2", "M"), 2L)
})

test_that("Double and triple digit cases get picked up", {
  expect_equal(ss3sim:::get_caseval("M10-D2", "M"), 10L)
  expect_equal(ss3sim:::get_caseval("M100-D2", "M"), 100L)
})

test_that("Delimiters error", {
  expect_error(ss3sim:::get_caseval("M1D2", "M"))
})

d <- system.file("extdata", package = "ss3sim")
eg_cases  <- file.path(d, "eg-cases")

test_that("No time varying OK", {
    out <- get_caseargs(folder = eg_cases,
                      case_files = list(F = "F",
                                        D = c("index", "lcomp", "agecomp"),
                                        E = "E",
                                        O = "O"
                                        ),
                      scenario = "F1-D0-E100-O0-cod")
    expect_equal(out$tv_params, NULL)
})

test_that("Time varying parsed", {
    out <- get_caseargs(folder = eg_cases,
                        case_files = list(F = "F",
                                          D = c("index", "lcomp", "agecomp"),
                                          M = "M",
                                          E = "E",
                                          O = "O"),
                        scenario = "F1-D0-M1-E100-O0-cod")
    expect_length(out$tv_params, 1)
    expect_equal(names(out$tv_params)[1], "NatM_p_1_Fem_GP_1")
})

test_that("An undeclared case in a scenario ID gets stopped", {
  expect_error(get_caseargs(folder = eg_cases,
               case_files = list(F = "F",
                                 D = c("index", "lcomp", "agecomp"),
                                 M = "M",
                                 E = "E"
                                 ),
               scenario = "F1-D0-M1-E100-O0-cod"))
})

test_that("standardize_sampling_args works as expected", {
  val <- standardize_sampling_args(fleets = c(1,2),
                               years = list(1),
                               other_input = list(1),
                               return_val  = "both")
  expect_equivalent(val[[1]], list(1,1))
  expect_equivalent(val[[2]], list(1,1))

  val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(c(98, 99), c(100, 101)),
                                   other_input = list(1),
                                   return_val  = "both")
  expect_equivalent(val[[2]], list(c(1,1), c(1,1)))
  val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(c(98, 99), c(100, 101)),
                                   other_input = list(c(1,2)),
                                   return_val  = "both")
  expect_equivalent(val[[2]], list(c(1,2), c(1,2)))
  val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(c(98, 99)),
                                   other_input = list(c(1,2)),
                                   return_val  = "both")
  expect_equivalent(val[[1]], list(c(98, 99), c(98, 99)))
  expect_equivalent(val[[2]], list(c(1,2), c(1,2)))

  val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(1),
                                   other_input = list(5,6),
                                   return_val  = "both")
  expect_equivalent(val[[1]], list(1, 1))
  expect_equivalent(val[[2]], list(5,6))
  val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(seq(60, 100, 2), 40:100),
                                   other_input = list(100,200),
                                   return_val  = "both")
expect_equivalent(val[[2]], list(rep(100, length.out = length(seq(60, 100, 2))),
                                 rep(200, length.out = length(40:100))))
})

test_that("standardize_sampling_args errors when expected", {
  expect_error(val <- standardize_sampling_args(fleets = c(1,2),
                                   years = list(c(98, 99)),
                                   other_input = list(c(1,2,3)),
                                   return_val  = "both"),
               "did not have the correct dimensions")
  expect_error(val <- standardize_sampling_args(fleets = c(1),
                                                years = list(c(98, 99)),
                                                other_input = list(c(1,2,3)),
                                                return_val  = "both"),
               "did not have the correct dimensions")
# should this situation work properly or is giving an error sufficient?
  expect_error(val <- standardize_sampling_args(fleets = c(1,2),
                                   years = 1,
                                   other_input = list(1),
                                   return_val  = "both"),
               "Input\\(s\\) were not the correct type:")
  expect_error(val <- standardize_sampling_args(fleets = list(1,2),
                                                years = list(1),
                                                other_input = list(1),
                                                return_val  = "both"),
               "Input\\(s\\) were not the correct type:")
})
