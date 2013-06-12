#' Get the folder location of a FISH600 model
#' @param folder_name The model folder name.
#' @export
#' @return A character object showing the location of appropriate
#' FISH600 folder in the package \code{extdata} folder.
#' @author Sean Anderson
#' @examples
#' \dontrun{
#' get_fish600_modelfolder("cod-em")
#' }
get_fish600_modelfolder <- function(folder_name) {
  d <- system.file("extdata", package = "ss3sim")
  f <- paste0(d, "/models/", folder_name)
  f
}


