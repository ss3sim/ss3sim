#' Setup operating models
#'
#' Copy operating models from their home locations and place them in a
#' master location for later use in the simulation. 
#'
#' @param home_name The base working directory for the simulation.
#' @param om_location A vector of paths to folder locations for each
#' of the necessary operating models. Folder names for the operating
#' models should match how the scenarios label each of the operating
#' models.
#' @param ss_path Path to the Stock Synthesis executable file (e.g. in
#' Windows: "c:\\SSFolder\\ss3.exe" or in Unix: "~/SSFolder/ss3")
#' @author Kelli Johnson
#' @export
#' @examples \dontrun{
#'# A Windows example:
#' setup_om(home_name = "c:\\ss", om_location = c(
#' "c:\\users\\kelli\\sardine", "c:\\users\\kelli\\cod"), ss_path =
#' "c:\\ss\\ss3.exe" )
#'
#'# A Unix example:
#' setup_om(home_name = "~/Desktop/sim", om_location = c(
#' "~/Desktop/sardine", "~/Desktop/cod"), ss_path =
#' "~/Desktop/ss3" )
#'}

setup_om <- function(home_name, om_location, ss_path){  

  #if(!(tolower(basename(ss_path)) == tolower("SS3.exe")))stop (
  #"Path to Stock Synthesis must end in SS3.exe" )

  if(!file.exists(ss_path))stop("Provide a correct path for Stock
    Synthesis, e.g. c:\\SSFolder\\ss3.exe or /SSfolder/ss3")

    omMasterLocation <- gsub("\\/", "\\", file.path(home_name,
        "om_master" ), fixed = TRUE) 

    dir.create(omMasterLocation, recursive = TRUE, showWarnings = FALSE)

    sapply(om_location, function(x) file.copy(from = x, to =
        omMasterLocation, recursive = TRUE))
    sapply(list.dirs(omMasterLocation, full.names = TRUE, recursive
        = FALSE ), function (x) file.copy(from = ss_path, to = x))
}
