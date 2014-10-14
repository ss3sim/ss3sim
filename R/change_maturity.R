#' Alter a control file to specify the SS3 maturity option
#'
#' Alter a control file to specify the maturity option in SS3. You could use
#' this function to, for example, tell SS3 to read empirical age-fecundity and
#' body weight-at-age data from \code{wtatage.ss}.
#'
#' @param file_in Input control file
#' @param file_out Output control file
#' @param maturity_option *An integer specifying \code{1} for length logistic,
#'   \code{2} for age logistic, \code{3} to read age-maturity for each female,
#'   \code{4} to read age-fecundity for each female growth patern, or \code{5}
#'   to read empirical age-fecundity and body weight-at-age from a separate
#'   file (\code{wtatage.ss}).
#' @template casefile-footnote
#' @author Sean C. Anderson
#' @return A modified SS3 control file.
# @examples
#' @export

change_maturity <- function(file_in = "em.ctl", file_out = "em.ctl",
  maturity_option = 1L) {

  if(!maturity_option[1] %in% seq_len(5) | length(maturity_option) != 1) {
    stop("Maturity option must be one of 1, 2, 3, 4, or 5")
  }

  ctl <- readLines(file_in)
  line <- grep("maturity_option", ctl)
  x <- strsplit(ctl[line], " ")[[1]]
  x[1] <- maturity_option
  ctl[line] <- paste(x, collapse = " ")

  writeLines(ctl, file_out)
}
