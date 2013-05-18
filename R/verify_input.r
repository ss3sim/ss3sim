#' Function to verify and standardize SS3 input files
#'
#' @description
#' This function verifies the contents of operating model (om) and estimation model
#' (em) folders. IF the contents are correct, .ctl and .dat files are
#' renamed to standardized names, ELSE warning is issued and process
#' aborted.
#'
#' Steps:
#' a) Check to ensure correct files are specified and output warning if not - SANITY CHECK
#' b) Rename .ctl and .dat files accordingly from specified OM and EM files
#' c) Change input .ctl and .dat names in starter.SS file 
#'
#' @author Curry James Cunningham
#' @details This is a helper function to be used within the larger
#' SS3 simulation wrapper function.
#
#' @param model_dir directory name for model
#' @param type One of "om" or "em" for operating or estimating model
#' @examples \dontrun{
#' verify.input(model_dir = "cod_om", type = "om")
#' verify.input(model_dir = "cod_em", type = "em")
#' }

verify_input <- function(model_dir, type = c("om", "em")) {

  type <- type[1]

  if (type == "om") {
    #Ensure correct files are provided (.ctl, .dat, ss3.par, starter.ss, forecast.ss)
    files <- list.files(model_dir)
    if (length(grep(".ctl", files, ignore.case = TRUE))) {
      f.ctl <- grep(".ctl", files, ignore.case = TRUE)
    }
    else {
      f.ctl <- NA
    }
    if (length(grep(".dat", files, ignore.case = TRUE))) {
      f.dat <- grep(".dat", files, ignore.case = TRUE)
    }
    else {
      f.dat <- NA
    }
    if (length(grep("ss3.par", files, ignore.case = TRUE))) {
      f.par <- grep("ss3.par", files, ignore.case = TRUE)
    }
    else {
      f.par <- NA
    }
    if (length(grep("starter.ss", files, ignore.case = TRUE))) {
      f.starter <- grep("starter.ss", files, ignore.case = TRUE)
    }
    else {
      f.starter <- NA
    }
    if (length(grep("forecast.ss", files, ignore.case = TRUE))) {
      f.forecast <- grep("forecast.ss", files, ignore.case = TRUE)
    }
    else {
      f.forecast <- NA
    }
    file.loc <- data.frame(f.ctl, f.dat, f.par, f.starter, 
      f.forecast)
    file.types <- c(".ctl file", ".dat file", "ss3.par file", 
      "starter.ss file", "forecast.ss file")
    missing.file <- which(is.na(file.loc)) # Which files are missing 
    if (length(missing.file) > 0) {
      stop(paste("Missing Files in", type, "=", file.types[missing.file], 
        "\n", sep = " "))
    }
    else { # Change names
      file.rename(from = paste(model_dir, "/", files[file.loc$f.ctl], 
        sep = ""), to = paste(model_dir, "/om.ctl", 
        sep = ""))
      file.rename(from = paste(model_dir, "/", files[file.loc$f.dat], 
        sep = ""), to = paste(model_dir, "/data.dat", 
        sep = ""))
      # Alter the starter.ss file
      starter.ss <- readLines(paste(model_dir, "/starter.ss", sep = ""))
      line.dat <- grep(".dat", starter.ss, ignore.case = TRUE)[1]
      starter.ss[line.dat] <- "data.dat"
      line.ctl <- grep(".ctl", starter.ss, ignore.case = TRUE)[1]
      starter.ss[line.ctl] <- "om.ctl"
      #Write new starter.ss
      file.ext <- file(paste(model_dir, "/starter.ss", sep = ""))
      writeLines(starter.ss, file.ext)
      close(file.ext)
      # Alter the .ctl file
      ctl <- readLines(paste0(model_dir, "/om.ctl"))
      ctl.line <- grep("data_and_control_files", ctl, ignore.case = TRUE)
      ctl[ctl.line] <- "#_data_and_control_files: data.dat // om.ctl"
      file.ext <- file(paste(model_dir, "/om.ctl", sep = ""))
      writeLines(ctl, file.ext)
      close(file.ext)
    }
  }
  if (type == "em") {
    # Ensure correct files are provided (.ctl, .dat, starter.ss, forecast.ss)
    files <- list.files(model_dir)
    if (length(grep(".ctl", files, ignore.case = TRUE))) {
      f.ctl <- grep(".ctl", files, ignore.case = TRUE)
    }
    else {
      f.ctl <- NA
    }
    if (length(grep(".dat", files, ignore.case = TRUE))) {
      f.dat <- grep(".dat", files, ignore.case = TRUE)
    }
    else {
      f.dat <- NA
    }
    if (length(grep("starter.ss", files, ignore.case = TRUE))) {
      f.starter <- grep("starter.ss", files, ignore.case = TRUE)
    }
    else {
      f.starter <- NA
    }
    if (length(grep("forecast.ss", files, ignore.case = TRUE))) {
      f.forecast <- grep("forecast.ss", files, ignore.case = TRUE)
    }
    else {
      f.forecast <- NA
    }
    file.loc <- data.frame(f.ctl, f.dat, f.starter, f.forecast)
    file.types <- c(".ctl file", ".dat file", "starter.ss file", 
      "forecast.ss file")
    missing.file <- which(is.na(file.loc)) # Which files are missing 
    if (length(missing.file) > 0) {
      stop(paste("Missing Files in", type, "=", file.types[missing.file], 
        "\n", sep = " "))
    }
    else { # Change names
      file.rename(from = paste(model_dir, "/", files[file.loc$f.ctl], 
        sep = ""), to = paste(model_dir, "/em.ctl", 
        sep = ""))
      file.rename(from = paste(model_dir, "/", files[file.loc$f.dat], 
        sep = ""), to = paste(model_dir, "/data.dat", 
        sep = ""))
      # Alter the starter.ss file
      starter.ss <- readLines(paste(model_dir, "/starter.ss", 
        sep = ""))
      line.dat <- grep(".dat", starter.ss, ignore.case = TRUE)[1]
      starter.ss[line.dat] <- "data.dat"
      line.ctl <- grep(".ctl", starter.ss, ignore.case = TRUE)[1]
      starter.ss[line.ctl] <- "em.ctl"
      #Write new starter.ss
      file.ext <- file(paste(model_dir, "/starter.ss", sep = ""))
      writeLines(starter.ss, file.ext)
      close(file.ext)
      # Alter the .ctl file
      ctl <- readLines(paste0(model_dir, "/em.ctl"))
      ctl.line <- grep("data_and_control_files", ctl, ignore.case = TRUE)
      ctl[ctl.line] <- "#_data_and_control_files: data.dat // em.ctl"
      file.ext <- file(paste(model_dir, "/em.ctl", sep = ""))
      writeLines(ctl, file.ext)
      close(file.ext)
    }
  }
  if (type != "om" & type != "em") {
    stop(paste("Misspecification of \"type\", read as:", 
      type, "-should be either \"om\" or \"em\"", sep = " "))
  }
}
