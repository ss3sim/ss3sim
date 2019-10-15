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
    expect_equal(length(out$tv_params), 1)
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
