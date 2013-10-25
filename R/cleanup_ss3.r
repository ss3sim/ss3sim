#' Clean up after an SS3 run
#'
#' Removes all of the unwanted output files from the specified
#' directory.
#'
#' @export
#' @param dir_name The directory of interest, the function ignores
#' case (i.e. names can be specified as lower or upper case)
#' @param clean_vector A vector of characters specifying the unwanted
#' files to be removed. The function allows the use of wildcards (i.e.
#' "*").
#' @author Kelli Johnson

cleanup_ss3 <- function(dir_name, clean_vector = c("admodel.*", 
  "ss3.eva", "fmin.log", "*.rpt", "variance", "ss3.b0*", 
  "ss3.p0*", "ss3.r0*", "ss3.bar", "ss3.cor", "ss3.log", 
  "ss3.rep", "checkup.sso", "cumreport.sso", "derived_posteriors.sso ", 
  "echoinput.sso", "parmtrace.sso", "posterior_vectors.sso", 
  "posteriors.sso", "rebuild.sso", "sis_table.sso", "data.ss_new", 
  "forecast.ss_new", "control.ss_new", "starter.ss_new", 
  "wtatage.ss_new")) {
  if (!file.exists(dir_name)) 
    stop("Specified directory does not exist")
  if (!is.character(clean_vector)) 
    stop("File names must be specified as character values in clean_vector")
  filesInFolder <- dir(dir_name, full.names = TRUE)
  # The use of wildcards "*" can potentially cause some files to be
  # duplicated in cleanLocator
  # using unique(cleanLocator) accommodates duplicates
  cleanLocator <- unlist(sapply(clean_vector, grep, filesInFolder))
  sapply(filesInFolder[unique(cleanLocator)], file.remove)
}
