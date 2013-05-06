#' Create simulation directories
#'
#' Create a tree of file directories based on the specified scenarios
#' for the simulation analyses and copy the respective operating
#' models from the master location to the trial folders.
#'
#' @param home_name The base working directory for the simulation. 
#' @param scen_list A list of vectors, where each vector specifies folder
#' names for the scenario options. Enter vectors in order of their
#' hierarchy. Names in the vector specifying the operating models
#' should match the folder names that house the operating models in
#' the master location. See the example below.
#' @param beg A numeric value for the first iteration of the simulation.
#' @param nsets A numeric value for the number of iterations to run.
#' @param copy_om A binary value indicating whether or not to copy the
#' necessary operating model from the master location to each
#' iteration folder.
#' @author Kelli Johnson
#' @export
#' @examples \dontrun{
#' # Change the home_name directory based on where you'd like the
#' example folders created:
#' create_dirs(home_name = "~/Desktop/", scen_list = list(main = "ss3sim",
#' papers = "m", species = c("cod","flat","sardine"), fishing =
#' c("down","up","contrast")), nsets = 10, copy_om = FALSE)
#' }

create_dirs <- function(home_name, scen_list, beg = 1, nsets = 100,
  copy_om = TRUE){

  require("reshape")

  checkPathName <- rev(unlist(strsplit(home_name,"")))[1]

# add a slash to the end of home_name if it isn't already a slash
  if(!(checkPathName == "\\" | checkPathName == "/")){
    home_name <- paste(home_name, "/", sep = "")
  }

  scen_list$iterations <- seq(beg,(beg+nsets-1), by = 1)
# add a slash to the end of home_name if it isn't already a slasp
  pathMatrix <- expand.grid(scen_list)
  pathVector <- apply(pathMatrix, 1, paste, collapse = "/")
  pathVector <- paste(home_name, pathVector, "/", sep = "")
  pathVector <- gsub(" ", "", pathVector)
  sapply(pathVector, dir.create, recursive = TRUE, showWarnings = FALSE)

  if(copy_om == TRUE){
    om_names <- list.files(file.path(home_name, "om_master"))
    om_paths <- gsub("\\/", "\\", list.files(file.path(home_name,
          "om_master"), full.names = TRUE), fixed = TRUE)
    om_files <- lapply(om_paths, list.files, full.names = TRUE)
    ## a list where each OM needs to go
    omDestIndex <- sapply(om_names, grep, x = pathVector, ignore.case
      = TRUE, simplify = FALSE)
    omDestLocat <- lapply(omDestIndex, function(x)pathVector[c(x)])

    for(i in seq_along(om_names)){
      sapply(omDestLocat[[i]], function(x)sapply(om_files[i],
          function(y)file.copy(from = y, to = x)))
    }
  }
}

