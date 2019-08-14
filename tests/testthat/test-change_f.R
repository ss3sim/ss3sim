context("Make sure change_f works")

temp_path <- file.path(tempdir(), "test-F")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")

# copy control file to temp_path.
file.copy(file.path(om, "codOM.ctl"), "codOM.ctl", overwrite = TRUE)

# set up input
input <- list(years = 1:100,
              fvals = rep(c(0, 0.1052), times = c(25, 75)),
              fisheries = 1)

test_that("change_f works with F val for each year", {
  ctl <- change_f(input$years, input$fisheries, input$fvals, ctl_file_in = "codOM.ctl",
             ctl_file_out = NULL)
  F_det_s_line <- grep("Fleet Yr Seas F_value se phase (for detailed setup of F_Method",
                       ctl, fixed = TRUE)+1
  F_det_e_line <- grep("Q_setup for fleets with cpue or survey data", ctl,
                       fixed = TRUE)-1
  expect_equal(F_det_e_line-F_det_s_line+1, length(input$years))
})

test_that("change_f works with a single F value", {
  input$fvals <- 0.1
  ctl <- change_f(input$years, input$fisheries, input$fvals, ctl_file_in = "codOM.ctl",
                  ctl_file_out = NULL)
  F_det_s_line <- grep("Fleet Yr Seas F_value se phase (for detailed setup of F_Method",
                       ctl, fixed = TRUE)+1
  F_det_e_line <- grep("Q_setup for fleets with cpue or survey data", ctl,
                       fixed = TRUE)-1
  expect_equal(F_det_e_line-F_det_s_line+1, length(input$years))
})

test_that("change_f gives expected error with invalid input", {
input$fisheries <- c(1,2)
expect_error(change_f(input$years,
                      input$fisheries,
                      input$fvals,
                      ctl_file_in = "codOM.ctl",
                      ctl_file_out = NULL),
             "The length of variable(s) fisheries is/are invalid", fixed = TRUE)
 input$fisheries <- 1
 input$fvals <- c(0,0.1)
 expect_error(change_f(input$years,
                       input$fisheries,
                       input$fvals,
                       ctl_file_in = "codOM.ctl",
                       ctl_file_out = NULL),
             "The length of variable(s) fvals is/are invalid", fixed = TRUE)
 input$fvals <- 0.1
 input$ses  <- c(0.05, 0.06, 0.07)
 expect_error(change_f(input$years,
                       input$fisheries,
                       input$fvals,
                       ses = input$ses,
                       ctl_file_in = "codOM.ctl",
                       ctl_file_out = NULL),
              "The length of variable(s) ses is/are invalid", fixed = TRUE)
})

# TODO maybe: change_F is currently not robust against all combinations of
# users using control files without standard ss_new commenting. May be worth
# changing in the future (but will be easier to do if can use r4ss::readctl
# instead of readLines and grep.) Can use these tests if want to do this:
# test_that("change_f give expected error with invalid control file", {
#   ctl <- readLines("codOM.ctl")
#   mod_ctl <- ctl
#   # modify ctl file so that it is missing an F_Method comment.
#   f_lines <- grep("F_Method", mod_ctl)
#   mod_ctl[f_lines[1]] <- "2"
#   writeLines(mod_ctl, "codOM_1_cmt_missing.ctl")
#   expect_error(ctl <-  change_f(input$years,
#                         input$fisheries,
#                         input$fvals,
#                         ctl_file_in = "codOM_1_cmt_missing.ctl",
#                         ctl_file_out = NULL),
#   "Phrase 'F_Method' should be found 3 times in the control file",
#   fixed = TRUE)
#   # remove Q setup comment
#   mod_ctl <- ctl
#   q_line <- grep("Q_setup", mod_ctl)
#   mod_ctl[q_line] <- "#"
#   writeLines(mod_ctl, "codOM_q_cmt_missing.ctl")
#   # TODO: modify change_f so the below tests passes (chainging the error message
#   # to something more informative.)
#   expect_error(
#     change_f(input$years,
#                         input$fisheries,
#                         input$fvals,
#                         ctl_file_in = "codOM_q_cmt_missing.ctl",
#                         ctl_file_out = NULL), "My error")
# })

test_that("change_f provides correct output w/o detailed F setup", {
  # modify codOM.ctl to not have detailed F setup.
  ctl <- readLines("codOM.ctl")
  mod_ctl <- ctl
  f_input_lines <- grep("overall start F value",mod_ctl, fixed = TRUE)
  det_s_line  <- grep("Fleet Yr Seas F_value", mod_ctl)
  if((det_s_line-1) %in% f_input_lines){
    mod_ctl[(det_s_line-1)] <- "0.2 1 0  # overall start F value; overall phase; N detailed inputs to read"
  } else {
    stop("Test not finding the correct ctl file spot to change.")
  }
  det_e_line <- grep("initial_F_parms", mod_ctl)
  mod_ctl <- mod_ctl[-(det_s_line:det_e_line)]
  writeLines(mod_ctl, "codOM_no_det.ctl")
  test_ctl <- change_f(input$years, input$fisheries, input$fvals,
                       ctl_file_in = "codOM_no_det.ctl", ctl_file_out = NULL)
  F_det_s_line <- grep("Fleet Yr Seas F_value se phase (for detailed setup of F_Method",
                       test_ctl, fixed = TRUE)+1
  F_det_e_line <- grep("Q_setup for fleets with cpue or survey data", test_ctl,
                       fixed = TRUE)-1
  expect_equal(F_det_e_line-F_det_s_line+1, length(input$years))

  #TODO: need to change F code so that can deal with this situation. Seems to
  # get inputs out of order as is in this scenario right now...
})

test_that("change_f provides correct output when F_method != 2 ", {
  #TODO develop test for this we think it is neceessary, or remove.
})
