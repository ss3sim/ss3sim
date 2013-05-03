#' Extract recruitment deviations from the par file
#' 
#' @param working_folder The folder with SS3 output.
#' @author Kotaro Ono
#' @export

get_rec_devs <- function(working_folder) {
  SS_par <- scan(file=paste(working_folder, "/ss3.par", sep=""), 
    what="character") 
  rec.dev.find <- find_item(SS_par,
    what_vec=c("recdev1","F"))
  Rec_dev <- as.numeric(SS_par[rec.dev.find[1]:rec.dev.find[2]])
  return(Rec_dev)
}

