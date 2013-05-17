#' Create template argument input files
#'
#' Creates template input files based on the argument lists for
#' specified functions. Look in your working directory for the
#' template files. Change the case ID number (defaults to \code{0})
#' and the species identifier to a three letter identifier. For the
#' FISH600 project use one of \code{cod}, \code{sar}, or \code{fla}
#' for cod, sardine, or flatfish.
#'
#' @param functions A named vector. The names correspond to the
#' filenames that will get written. The values correspond to the
#' functions to grab the arguments from.
#' @param ext The file extension to create the configuration files
#' with. Defaults to \code{".txt"}.
#' @param delim The delimiter. Defaults to \code{"; "}.
#' @param ... Anything else to pass to \code{write.table}.
#' @author Sean Anderson
#' @examples \dontrun{
#' create_argfiles()
#' }
#' @export

create_argfiles <- function(functions = c("lcomp0-spp" =
    "change_lcomp", "agecomp0-spp" = "change_agecomp", "index0-spp" =
    "change_index", "M0-spp" = "change_m", "F0-spp" = "change_f"), ext
  = ".txt", delim = "; ", ...) {
  if(!is.character(functions)) 
    stop("Functions must be a vector of character.")
  for(i in 1:length(functions)) {
    x <- formals(functions[i])
    d <- data.frame(args = names(x), vals = as.character(x))
    write.table(d, file = paste0(names(functions)[i], ext), sep = delim,
      row.names = FALSE, col.names = FALSE, quote = FALSE, ...)
  }
  print(paste("Created the template file", paste0(names(functions), ext)))
}
