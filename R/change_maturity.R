#' Alter a control file to specify the SS3 maturity option
#'
#' Alter a control file to specify the maturity option in SS3. You could use
#' this function to, for example, tell SS3 to read empirical age-fecundity and
#' body weight-at-age data from \code{wtatage.ss}.
#'
#' @template ctl_file_in
#' @template ctl_file_out
#' @param maturity_option *An integer specifying \code{1} for length logistic,
#'   \code{2} for age logistic, \code{3} to read age-maturity for each female,
#'   \code{4} to read age-fecundity for each female growth patern, or \code{5}
#'   to read empirical age-fecundity and body weight-at-age from a separate
#'   file (\code{wtatage.ss}).
#' @template casefile-footnote
#' @author Sean C. Anderson
#' @return A modified SS3 control file.
#' @export
#' @family change functions
#' @examples
#' # Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-maturity-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#'
#' # Locate the package data:
#' ctlfile <- system.file("extdata", "models", "cod-em",
#'  "codEM.ctl", package = "ss3sim")
#'
#' # Change the maturity option from 1 to 5:
#' change_maturity(ctlfile, "test.ctl", maturity_option = 5L)
#'
#' unlink("test.ctl")
#' setwd(wd)

change_maturity <- function(ctl_file_in = "em.ctl", ctl_file_out = "em.ctl",
  maturity_option = 1L) {

  if(!maturity_option[1] %in% seq_len(5) | length(maturity_option) != 1) {
    stop("Maturity option must be one of 1, 2, 3, 4, or 5")
  }

  ctl <- readLines(ctl_file_in)
  line <- grep("maturity_option", ctl)
  x <- strsplit(ctl[line], " ")[[1]]
  x[1] <- maturity_option
  ctl[line] <- paste(x, collapse = " ")

  writeLines(ctl, ctl_file_out)
}
