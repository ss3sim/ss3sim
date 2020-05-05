context("Make sure change_f works")

temp_path <- file.path(tempdir(), "test-F")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")
em <- file.path(d, "models", "cod-em")

# copy control file to temp_path.
file.copy(file.path(om, "codOM.ctl"), "codOM.ctl", overwrite = TRUE)
file.copy(file.path(em, "codEM.ctl"), "codEM.ctl", overwrite = TRUE)

# set up input
input <- list(years = 1:100,
              fvals = rep(c(0, 0.1052), times = c(25, 75)),
              fisheries = 1)

test_that("change_f works with F val for each year", {
  ctl <- change_f(input$years, input$fisheries, input$fvals, ctl_file_in = "codOM.ctl",
             ctl_file_out = NULL)
  F_det_s_line <- 1 + max(grep(x = ctl,
    "overall start F value|Fleet Yr Seas F_value se phase \\(for detailed setup of F_Method"))
  F_det_e_line <- grep("Q_setup for fleets with cpue or survey data", ctl,
                       fixed = TRUE)-1
  expect_equal(F_det_e_line-F_det_s_line+1, length(input$years))
})

test_that("change_f works with a single F value", {
  input$fvals <- 0.1
  ctl <- change_f(input$years, input$fisheries, input$fvals, ctl_file_in = "codOM.ctl",
                  ctl_file_out = NULL)
  F_det_s_line <- 1 + max(grep(x = ctl,
    "overall start F value|Fleet Yr Seas F_value se phase \\(for detailed setup of F_Method"))
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
ctl <- readLines("codOM.ctl")
mod_ctl <- ctl
q_line <- grep("Q_setup", mod_ctl)
mod_ctl[q_line] <- "#"
writeLines(mod_ctl, "codOM_q_cmt_missing.ctl")
expect_error(change_f(input$years,
                      input$fisheries,
                      input$fvals,
                      ctl_file_in = "codOM_q_cmt_missing.ctl",
                      ctl_file_out = NULL),
            "Q_setup was not found in the ctl_file_in")
})

test_that("change_f provides correct output w/o detailed F setup", {
  # modify codOM.ctl to not have detailed F setup, following formatting in an
  # control.ss_new file.
  ctl <- readLines("codOM.ctl")
  mod_ctl <- ctl
  f_input_lines <- grep("overall start F value",mod_ctl, fixed = TRUE)
  det_s_line  <- grep("Fleet Yr Seas F_value", mod_ctl)
  if((det_s_line-1) %in% f_input_lines){
    mod_ctl[(det_s_line-1)] <- "0.2 1 0  # overall start F value; overall phase; N detailed inputs to read"
  } else {
    stop("Test not finding the correct ctl file spot to change.")
  }
  det_e_line <- grep("initial_F_parms", mod_ctl)-1
  mod_ctl <- mod_ctl[-((det_s_line+1):det_e_line)]
  writeLines(mod_ctl, "codOM_no_det.ctl")
  test_ctl <- change_f(input$years, input$fisheries, input$fvals,
                       ctl_file_in = "codOM_no_det.ctl", ctl_file_out = NULL)
  F_det_s_line <- 1 + max(grep(x = test_ctl,
    "overall start F value|Fleet Yr Seas F_value se phase \\(for detailed setup of F_Method"))
  F_det_e_line <- grep("Q_setup for fleets with cpue or survey data", test_ctl,
                       fixed = TRUE)-1
  expect_equal(F_det_e_line-F_det_s_line+1, length(input$years))
})

test_that("change_f provides error when F_method is 1 or 3 ", {
  expect_error(change_f(input$years, input$fisheries, input$fvals,
                   ctl_file_in = "codEM.ctl", ctl_file_out = NULL),
               "change_F only works with F_method = 2")
})
