#' Verify and standardize Stock Synthesis input files
#'
#' Verify the contents of
#' operating model (`OM`) and
#' estimation model (`EM`) folders, i.e.,
#' check that the necessary SS input files are available.
#' If the contents are correct,
#' the `.ctl` and `.dat` files are renamed to standardized names and
#' the `starter.ss` file is updated to reflect these names.
#' If the contents are incorrect,
#' then a warning is issued and the simulation is aborted.
#'
#' @author Curry James Cunningham; modified by Sean Anderson
#' @return Nothing is returned from this function. Instead,
#' file are changed and saved to the disk.
#
#' @param model_dir Directory name for model. This folder should contain the
#'   `.ctl`, `.dat`, files etc.
#' @param type One of "om" or "em" for operating or estimating model.
#' @export
#'
#' @examples
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-verify-example")
#' dir.create(temp_path, showWarnings = FALSE)
#'
#' d <- system.file("extdata", "models", package = "ss3sim")
#'
#' om <- file.path(d, "cod-om")
#' em <- file.path(d, "cod-em")
#'
#' file.copy(om, temp_path, recursive = TRUE)
#' file.copy(em, temp_path, recursive = TRUE)
#'
#' # Verify the correct files exist and change file names:
#' verify_input(model_dir = file.path(temp_path, "cod-om"), type = "om")
#' verify_input(model_dir = file.path(temp_path, "cod-em"), type = "em")
#' unlink(temp_path, recursive = TRUE)
verify_input <- function(model_dir, type = c("om", "em")) {
  type <- match.arg(type, several.ok = FALSE)

  ctl_name <- paste0(type, ".ctl")

  # Ensure correct files are provided
  files <- list.files(model_dir)
  if (length(grep(".ctl", files, ignore.case = TRUE))) {
    f.ctl <- grep(".ctl", files, ignore.case = TRUE)
  } else {
    f.ctl <- NA
  }
  if (type == "om") {
    if (length(grep(".dat", files, ignore.case = TRUE))) {
      f.dat <- grep(".dat", files, ignore.case = TRUE)
    } else {
      f.dat <- NA
    }
  }

  if (length(grep("starter.ss", files, ignore.case = TRUE))) {
    f.starter <- grep("starter.ss$", files, ignore.case = TRUE)
  } else {
    f.starter <- NA
  }
  if (length(grep("forecast.ss", files, ignore.case = TRUE))) {
    f.forecast <- grep("forecast.ss$", files, ignore.case = TRUE)
  } else {
    f.forecast <- NA
  }
  if (type == "om") {
    file.loc <- data.frame(f.ctl, f.dat, f.starter, f.forecast) # f.par was removed
    file.types <- c(".ctl file", ".dat file", "starter.ss
      file", "forecast.ss file") # "ss.par file",  was removed
  }
  if (type == "em") {
    file.loc <- data.frame(f.ctl, f.starter, f.forecast)
    file.types <- c(".ctl file", "starter.ss file", "forecast.ss file")
  }
  missing.file <- which(is.na(file.loc)) # Which files are missing
  if (length(missing.file) > 0) {
    stop(paste("Missing Files in", type, ":", file.types[missing.file], "\n"))
  } else { # Change names
    file.rename(
      from = paste0(model_dir, "/", files[file.loc$f.ctl]), to =
        paste0(model_dir, "/", ctl_name)
    )
    if (type == "om") {
      file.rename(
        from = paste0(model_dir, "/", files[file.loc$f.dat]), to =
          paste0(model_dir, "/ss3.dat")
      )
    }
    # Alter the starter.ss file
    starter.ss <- r4ss::SS_readstarter(
      file = paste0(model_dir, "/starter.ss"),
      verbose = FALSE
    )
    starter.ss$datfile <- "ss3.dat"
    starter.ss$ctlfile <- ctl_name
    # Write new starter.ss
    r4ss::SS_writestarter(
      mylist = starter.ss, dir = model_dir,
      file = "starter.ss", overwrite = TRUE, verbose = FALSE, warn = FALSE
    )
    # Alter the .ctl file
    ctl <- readLines(paste0(model_dir, "/", ctl_name))
    ctl.line <- grep("data_and_control_files", ctl, ignore.case = TRUE)
    ctl[ctl.line] <- paste0("#_data_and_control_files: ss3.dat // ", ctl_name)
    file.ext <- file(paste0(model_dir, "/", ctl_name))
    writeLines(ctl, file.ext)
    close(file.ext)
  }
}
