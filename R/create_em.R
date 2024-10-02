#' Create an EM from an OM
#'
#' Create estimation model (EM) files from operating model (OM) files.
#' By making small changes to the OM rather than having two sets of files,
#' less files need to be maintained.
#' Differences between the OM and EM are mainly related to how the
#' OM takes input fishing mortality values rather than absolute catches.
#'
#' @section Control file:
#' Most changes to the EM control file relate to recruitment and fishing.
#' The phase in which recruitment deviations are estimated is checked to
#' ensure that it is positive. Though, this might be unnecessary because the OM
#' file can have negative or positive phases. Thus, users are encouraged to
#' just set the phase in which recruitment is estimated in the OM at the value
#' that they would like to use in the EM.
#' Additional changes are made to the bias adjustment procedure based on
#' the biology of the stock.
#'
#' The `F_Method` is set to 3 to allow the model to estimate fishing mortality
#' based on catches in the data file. Users might want to adjust the maximum
#' fishing mortality based on their scenarios.
#'
#' @section Data file:
#' No data file is needed for the EM.
#' The `data_expval.ss` file produced when executing the OM contains the
#' expected values of the OM population dynamics.
#' {ss3sim} provides three functions which carry out the random sampling
#' process and generate `.dat` files to be used in the EM.
#' See the Introduction vignette `vignette("introduction", package = "ss3sim")`
#' for more details.
#'
#' @section Forecast file:
#' Nothing is changed in the forecast file from the OM.
#'
#' @section Starter file:
#' The names of the data and control files are specified and
#' the maximum phase for estimation is set to 100.
#'
#' @param dir_in A file path to a directory that contains the following files:
#' `forecast.ss`, `starter.ss`, and a control file
#' (e.g., `xxxOM.ctl`). The default is to get the codOM within \pkg{ss3sim}.
#' @param dir_out A file path to a directory where the new files will be saved.
#' The default is to save the files in your current working directory in a
#' folder called `new-em`.
#'
#' @export
#' @author Kelli F. Johnson
#' @return Nothing is returned, but three files are saved to the disk in
#' the specified folder that may also be new.
#' @examples
#' create_em()
#' # The necessary files are in the following folder
#' dir(file.path(getwd(), "new-em"))
#' # Clean up your directory
#' unlink(file.path(getwd(), "new-em"), recursive = TRUE)
create_em <- function(dir_in = system.file("extdata", "models", "cod-om", package = "ss3sim"),
                      dir_out = file.path(getwd(), "new-em")) {
  dir.create(dir_out, showWarnings = FALSE, recursive = TRUE)
  inputs <- r4ss::SS_read(
    dir = dir_in,
    ss_new = FALSE,
    verbose = FALSE
  )

  # Control file
  ctl_name_out <- gsub(
    "OM",
    "EM",
    basename(inputs[["ctl"]][["sourcefile"]]),
    ignore.case = TRUE
  )
  dat_list <- inputs[["dat"]]
  ctl_list <- inputs[["ctl"]]
  ctl_list$MainRdevYrFirst <- dat_list$styr + dat_list$Nages
  ctl_list$recdev_early_start <- dat_list$styr + floor(dat_list$Nages * -0.5)
  ctl_list$recdev_early_phase <- abs(ctl_list$recdev_early_phase)
  ctl_list$recdev_phase <- abs(ctl_list$recdev_phase)
  ctl_list$last_early_yr_nobias_adj <- ifelse(
    dat_list$styr + dat_list$Nages < dat_list$endyr,
    dat_list$styr + dat_list$Nages,
    dat_list$styr
  )
  ctl_list$first_yr_fullbias_adj <- ctl_list$last_early_yr_nobias_adj + 1
  ctl_list$last_yr_fullbias_adj <- dat_list$endyr - dat_list$Nages / 4
  ctl_list$first_recent_yr_nobias_adj <- dat_list$endyr - 1
  ctl_list$max_bias_adj <- 0.9
  ctl_list$min_rec_dev <- -10
  ctl_list$max_rec_dev <- 10
  ctl_list$F_Method <- 3
  names(ctl_list)[which(names(ctl_list) == "F_setup")] <- "F_iter"
  ctl_list$F_iter <- 4
  ctl_list <- ctl_list[-which(names(ctl_list) == "F_setup2")]
  r4ss::SS_writectl(
    ctllist = ctl_list,
    outfile = file.path(dir_out, ctl_name_out),
    verbose = FALSE, overwrite = TRUE
  )

  # Forecast file
  r4ss::SS_writeforecast(
    inputs[["fore"]],
    dir = dir_out,
    writeAll = FALSE,
    overwrite = TRUE,
    verbose = FALSE
  )

  # Starter file
  starter_list <- inputs[["start"]]
  starter_list$datfile <- "ss3.dat"
  starter_list$ctlfile <- ctl_name_out
  starter_list$cumreport <- 1
  starter_list$last_estimation_phase <- 100
  starter_list$maxyr_sdreport <- -2
  r4ss::SS_writestarter(starter_list,
    dir = dir_out,
    overwrite = TRUE, verbose = FALSE
  )
}
