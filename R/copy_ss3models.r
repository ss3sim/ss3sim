#' Copy operating and estimation models while creating folder
#' structure
#'
#' @param model_dir A directory containing the operating or estimation
#' models. Each folder should be named according to a scenario ID.
#' @param iterations The iterations to copy to. The function will
#' create the folders as needed.
#' @param scenarios Which scenarios to copy to. Supply a vector of
#' character elements.
#' @param type Are you copying operating or estimation models? This
#' affects whether the model folder gets named "om" or "em"
#' @author Sean Anderson, Kelli Johnson
#' @examples \dontrun{
#' dir.create("oms/blockm-cod")
#' dir.create("oms/blockm-fla")
#' copy_ss3models(model_dir = "oms", type = "om", iterations = 1:3)
#' }
#' @export

copy_ss3models <- function(model_dir, scenarios,
  iterations = 1:100, type = c("om", "em")) {

  type <- type[1]

  for(sc in scenarios) {
    for(it in iterations) {
      from <- pastef(model_dir)
      to <- pastef(sc, it)
      dir.create(to, showWarnings = FALSE, recursive = TRUE)
      if(file.exists(pastef(to, type))){
        stop(paste0(to, "/", type, " already exists. Have you already run this model?"))
      } else {
        file.copy(from, to, recursive = TRUE)
        orig_model_folder <- rev(strsplit(from, "/")[[1]])[1]
        file.rename(pastef(to, orig_model_folder), pastef(to, type))
        # sanity check and rename for consistency:
        verify_input(model_dir = pastef(to, type), type = type)
      }
    }
  }

}

