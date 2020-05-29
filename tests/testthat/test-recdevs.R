context("Test user_recdevs, change_rec_devs()")
#.
#get_recdevs() is pretty simple, so does not need a test for now.

temp_path <- file.path(tempdir(), "test-recdevs")
dir.create(temp_path, showWarnings = FALSE)
wd <- getwd()
setwd(temp_path)
on.exit(setwd(wd), add = TRUE)
on.exit(unlink(temp_path, recursive = TRUE), add = TRUE)

d <- system.file("extdata", package = "ss3sim")
om <- file.path(d, "models", "cod-om")

# copy control file to temp_path.
file.copy(file.path(om, "codOM.ctl"), "codOM.ctl", overwrite = TRUE)

test_that("run_ss3sim fails if ncol(user_recdevs) is too small for iterations", {
  expect_error(run_ss3sim(iterations = 2,
    simdf = data.frame(setup_scenarios_defaults(),
      user_recdevs = "matrix(data = 0, ncol = 1, nrow = 100)")),
    "The number of columns in user_recdevs is less than the specified number of iterations")
})
test_that("change_rec_devs() works with vector of recdevs", {
  set.seed(123)
  recdevs <- rnorm(101, 0, 1) #don't worry about bias correction here.
  out_ctl <- change_rec_devs(recdevs, ctl_file_in = "codOM.ctl",
                             ctl_file_out = NULL)
  n_recdevs_line <- grep("#_read_recdevs", out_ctl, fixed = TRUE)
  n_recdevs <- as.numeric(strsplit(trimws(out_ctl[n_recdevs_line]), "\\s+")[[1]][1])
  f_recdev_line <-n_recdevs_line+1
  l_recdev_line <- grep("#Fishing Mortality info", out_ctl, fixed = TRUE)-1
  # check that the number of years corresponds with number of lines inserted.
  expect_equal(n_recdevs, l_recdev_line-f_recdev_line+1)
})

test_that("change_rec_devs works with single recdev value", {
  out_ctl <- change_rec_devs(0.01, "codOM.ctl", NULL)
  n_recdevs_line <- grep("#_read_recdevs", out_ctl, fixed = TRUE)
  n_recdevs <- as.numeric(strsplit(trimws(out_ctl[n_recdevs_line]), "\\s+")[[1]][1])
  f_recdev_line <-n_recdevs_line+1
  l_recdev_line <- grep("#Fishing Mortality info", out_ctl, fixed = TRUE)-1
  # check that the number of years corresponds with number of lines inserted.
  expect_equal(n_recdevs, l_recdev_line-f_recdev_line+1)
})

test_that("change_rec_devs works with named list", {
  rec_devs_yrs <- c(1, 5)
  devs <- c(0.05, 0.05)
  names(devs) <- rec_devs_yrs
  out_ctl <- change_rec_devs(devs, "codOM.ctl", NULL)
  n_recdevs_line <- grep("#_read_recdevs", out_ctl, fixed = TRUE)
  n_recdevs <- as.numeric(strsplit(trimws(out_ctl[n_recdevs_line]), "\\s+")[[1]][1])
  f_recdev_line <-n_recdevs_line+1
  l_recdev_line <- grep("#Fishing Mortality info", out_ctl, fixed = TRUE)-1
  # check that the number of years corresponds with number of lines inserted.
  expect_equal(n_recdevs, l_recdev_line-f_recdev_line+1)
  # check that these are for the correct years
  read_yrs <- out_ctl[(n_recdevs_line+1):(n_recdevs_line+length(rec_devs_yrs))]
  read_yrs <- unlist(lapply(strsplit(trimws(read_yrs), "\\s+"),
                            function(x) x[1]))
  expect_equivalent(as.integer(rec_devs_yrs), as.integer(read_yrs))
})

test_that("change_rec_devs gives error when provided too few recdevs", {
  set.seed(123)
  recdevs <- rnorm(2, 0, 1) # too short
  expect_error(change_rec_devs(recdevs, ctl_file_in = "codOM.ctl",
                  ctl_file_out = NULL),
               "The length of recdevs was")
})

test_that("change_rec_devs gives error when cannot find info on number and yrs for recdevs", {
  set.seed(123)
  recdevs <- rnorm(200, 0, 1)
  ctl <- readLines("codOM.ctl")
  #get rid of recrutiment dev lines that ss3sim uses to determine recdevs number
  tmp_start <- grep("# all recruitment deviations", ctl , fixed = TRUE)
  tmp_end <- grep("implementation error by year in forecast", ctl, fixed = TRUE)
  writeLines(ctl[-(tmp_start:tmp_end)], "codOM_mod.ctl")
  expect_error(change_rec_devs(recdevs, "codOM_mod.ctl", NULL),
               paste0("The number of recdevs and their associated years could",
                       " not be determined"))
})

test_that("change_rec_devs give error when advanced recdevs options are off",{
  #Note: would expect this test to fail if change_rec_devs is able to deal with
  # this scenario in the future.
  set.seed(123)
  ctl <- readLines("codOM.ctl")
  s_adv <- grep("to read 13 advanced options", ctl)
  e_adv <- grep("end of advanced SR options", ctl)
  ctl[s_adv] <- "0"
  ctl <- ctl[-((s_adv+1):e_adv)]
  writeLines(ctl, "codOM_no_adv.ctl")
  expect_error(change_rec_devs(rnorm(101), "codOM_no_adv.ctl", NULL),
              paste0("Currently ss3sim can only add recruitment deviations to ",
                     "control files that have advanced recruitment options ",
                     "turned on"))
})

test_that("change_rec_devs looks for spot to put recdevs as expected",{
  set.seed(123)
  recdevs <- rnorm(101)
  ctl <- readLines("codOM.ctl")
  # missing starting spot.
  recdev_line <- grep("_read_recdevs", ctl)
  mod_ctl <- ctl
  mod_ctl[recdev_line] <- "0" # removes comment.
  writeLines(mod_ctl, "codOM_no_recdevs_cmt.ctl")
  expect_error(change_rec_devs(recdevs, "codOM_no_recdevs_cmt.ctl", NULL),
               paste0("ss3sim could not find the line with comment ",
               "'#_read_recdevs' in the OM control file"), fixed = TRUE)
  # missing both F comments
  mod_ctl <- ctl
  F_line <- grep("Fishing Mortality info", mod_ctl)
  F_ball_line <- grep("F ballpark$", mod_ctl)
  mod_ctl <- mod_ctl[-c(F_line, F_ball_line)]
  writeLines(mod_ctl, "codOM_missing_2_F_cmt.ctl")
  expect_error(change_rec_devs(recdevs, "codOM_missing_2_F_cmt.ctl", NULL),
               "Could not find spot where Fishing Mortality info starts")
  #only missing 1 F comment (can run)
  mod_ctl <- ctl
  mod_ctl <- mod_ctl[-F_line]
  writeLines(mod_ctl, "CodOM_missing_1_F_cmt.ctl")
  out_ctl <- change_rec_devs(recdevs, "CodOM_missing_1_F_cmt.ctl", NULL)
  n_recdevs_line <- grep("#_read_recdevs", out_ctl, fixed = TRUE)
  n_recdevs <- as.numeric(strsplit(trimws(out_ctl[n_recdevs_line]), "\\s+")[[1]][1])
  f_recdev_line <-n_recdevs_line+1
  l_recdev_line <- grep("F ballpark$", out_ctl)-1
  expect_equal(n_recdevs, l_recdev_line-f_recdev_line+1)
})
