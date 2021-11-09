context("Make sure change_f works")

om <- system.file("extdata", "models", "cod-om", package = "ss3sim")
em <- system.file("extdata", "models", "cod-em", package = "ss3sim")
dat_list <- r4ss::SS_readdat(file.path(om, "codOM.dat"), verbose = FALSE)
ctl_list <- r4ss::SS_readctl(file.path(om, "codOM.ctl"), verbose = FALSE,
  use_datlist = TRUE, datlist = dat_list)

# set up input
input <- list(years = 1:100,
              fvals = rep(c(0, 0.1052), times = c(25, 75)),
              fleets = 1, ctl_list = ctl_list)

test_that("change_f works with F val for each year", {
  ctl <- do.call("change_f", input)
  expect_equal(NROW(ctl[["F_setup2"]]), length(input$years))
})

test_that("change_f works with a single F value", {
  input$fvals <- 0.1
  ctl <- do.call("change_f", input)
  expect_equal(NROW(ctl[["F_setup2"]]), length(input$years))
})

test_that("change_f gives expected error with invalid input", {
 input$fleets <- c(1,2)
 expect_error(do.call("change_f", input))
 input$fleets <- 1
 input$fvals <- c(0,0.1)
 expect_error(do.call("change_f", input))
 input$fvals <- 0.1
 input$ses  <- c(0.05, 0.06, 0.07)
 expect_error(do.call("change_f", input))
})

test_that("change_f provides error when F_method is 1 or 3 ", {
  input$ctl_list <- r4ss::SS_readctl(file.path(em, "codEM.ctl"), verbose = FALSE,
    use_datlist = TRUE, datlist = dat_list)
  expect_error(do.call("change_f", input),
    "== 2 is not TRUE")
})

test_that("change_f works with list-style inputs", {
  inlist <- list(
    years = list(1:10, 5:10), fvals = 0.2,
    fleets = 3:4,
    ctl_list = ctl_list
  )
  out <- do.call("change_f", inlist)
  expect_equal(
    length(unlist(inlist$years)),
    NROW(out[["F_setup2"]][, 2])
  )
  expect_equal(
    rep(formals(change_f)[["seasons"]], length(unlist(inlist$years))),
    out[["F_setup2"]][, 3]
  )
  expect_equal(
    rep(inlist[["fvals"]], length(unlist(inlist$years))),
    out[["F_setup2"]][, 4]
  )
  expect_equal(
    rep(formals(change_f)[["ses"]], length(unlist(inlist$years))),
    out[["F_setup2"]][, 5]
  )
  expect_equal(
    rep(1, length(unlist(inlist$years))),
    out[["F_setup2"]][, 6]
  )
  inlist$fleets <- 1
  expect_error(do.call("change_f", inlist))
  inlist$fleets <- 1:2
  inlist$ses <- list(1:9, 5:10)
  # Capture error message but don't print warning b/c it messes up
  # how the tests are printed to the screen.
  expect_equal(expected = "error", tryCatch({
    suppressMessages(utils::capture.output(
      do.call("change_f", inlist), type = "output"))
  },
      error = function(x) return("error")
  ))
})
