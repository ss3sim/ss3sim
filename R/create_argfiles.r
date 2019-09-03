#' Create template argument input files
#'
#' Creates template input files based on the argument lists for specified
#' functions. Look in your working directory for the template files. Change
#' the case ID number (defaults to \code{0}) and the species identifier to a
#' three letter identifier. To use one of the built-in model setups, use one
#' of \code{cod}, \code{sar}, or \code{fla} for cod, sardine, or flatfish. An
#' example filename would be \code{M1-sar.txt} or \code{lcomp2-fla.txt}.
#'
#' @param functions A named vector. The names correspond to the filenames that
#'   will get written. The values correspond to the functions to grab the
#'   arguments from.
#' @param ext The file extension to create the configuration files with.
#'   Defaults to \code{".txt"}.
#' @param delim The delimiter. Defaults to \code{"; "}.
#' @param ignore A vector of character object of arguments to ignore in the
#'   arguments. Found via \code{grep} so can be part of an argument name.
#' @param ... Anything else to pass to \code{write.table}.
#' @author Sean Anderson
#' @examples \dontrun{
#' create_argfiles()
#' # Some example input lines:
#' #
#' # year1; 1990
#' # years; 1990:2000
#' # years; c(1980, 1990, 1995)
#' # survey_type; fishery
#' }
#' @export
#' @details
#' The first column in the text files denotes the argument to be passed to a
#' function. The second argument denotes the value to be passed. You can use
#' any simple \R syntax. For example: \code{c(1, 2, 4)}, or \code{seq(1, 100)}
#' or \code{1:100} or \code{matrix()}. Character objects don't need to be
#' quoted. However, be careful not to use your delimiter (set up as a
#' semicolon) anywhere else in the file besides to denote columns.
#'
#' The function \code{\link{change_tv}} is a special case. To pass arguments
#' to \code{\link{change_tv}} through a \code{\link{run_ss3sim}}: (1) create a
#' case file with an arbitrary letter not used elsewhere (anything but D, E,
#' F, or R) and include the line \code{function_type; change_tv} in your case
#' file. For example, you might want to use M for natural mortality, S for
#' selectivity, or G for growth.
#'
#' This function (\code{create_argfiles}) automatically adds a line
#' \code{function_type; change_tv} to the top of a case file \code{X0-spp.txt}
#' as a starting point for \code{change_tv}.

create_argfiles <- function(functions = c("lcomp0-spp" =
    "sample_lcomp", "agecomp0-spp" = "sample_agecomp", "index0-spp" =
    "sample_index", "F0-spp" = "change_f",
    "R0-spp" = "change_retro", "E0-spp" = "change_e",
    "X0-spp" = "change_tv"), ext = ".txt",
    delim = "; ", ignore = c("file", "dir", "make_plot"), ...) {
  if(!is.character(functions))
    stop("Functions must be a vector of character.")
  for(i in 1:length(functions)) {
    x <- formals(functions[i])
    args_ignore <- as.numeric(unlist(sapply(ignore,
          function(z) grep(z, names(x)))))
    message(paste("Ignoring", names(x)[args_ignore], "in", functions[i]))
    x <- x[-args_ignore]
    d <- data.frame(args = names(x), vals = as.character(x))
    if(functions[[i]] == "change_tv") {
      d <- rbind(data.frame(args = "function_type", vals = "change_tv"), d)
    }
    write.table(d, file = paste0(names(functions)[i], ext), sep = delim,
      row.names = FALSE, col.names = FALSE, quote = FALSE, ...)
  }
  message(paste("Created the template file", paste0(names(functions), ext)))
}
