#' Get the folder location of an included Stock Synthesis model configuration
#'
#' This function returns the location of one of the built-in model
#' configurations.
#'
#' @param folder_name The model folder name. One of `"cod-om"`, `"cod-em"`,
#' `"fla-om"`, `"fla-em"`, `"sar-om"`, `"sar-em"` representing cod, flatfish, and
#' sardine-like model configurations and operating (om) and estimating model
#' (em) varieties. See the \pkg{ss3sim} paper or vignette for further details.
#' @export
#' @return A character object showing the location of the appropriate model
#' configuration folder in the package `extdata` folder.
#' @examples
#' get_model_folder("cod-em")
get_model_folder <- function(folder_name) {
  d <- system.file("extdata", package = "ss3sim")
  f <- file.path(d, "models", folder_name)
  f
}
