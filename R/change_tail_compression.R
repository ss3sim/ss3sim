#' Replace tail compression value for length composition data.
#'
#' This function replaces the tail compression value for length composition
#' data in a \code{dat} file (\code{file_in}) with those specified in
#' \code{tail_compression}. It then writes a new file with name
#' \code{file_out} into the working directory.
#'
#' @param tail_compression The new tail_compression value to be used. Must
#' be a numeric value, as a proportion. For example 0.1 means 10
#' percent. See the SS3 manual for further information. A NULL value
#' indicates no action, a negative value indicates to SS3 to ignore it (not
#' use that feature).
#' @param file_in Input SS3 dat file.
#' @param file_out Output SS3 dat file. Typically the same as \code{file_in}.
#' @return A modified SS3 \code{.dat} file, and that file returned
#' invisibly (for testing) as a vector of character lines.
#' @author Cole Monnahan
#' @export

#' @examples
#' ## Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-tail-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' ## Run the function with built-in data file.
#' dat_file <- system.file("extdata", "example-om", "data.ss_new",
#'   package = "ss3sim")
#' test <- change_tail_compression(tail_compression = .1234, file_in = dat_file,
#'   file_out = paste0(temp_path, "/test.dat"))
#' ## Look at the changes
#' test[grep("_tail_compression", test)[1]]
#' ## Clean up the temp files
#' unlink(temp_path)
change_tail_compression <- function(tail_compression, file_in, file_out){
    ## Check inputs
    if(is.null(tail_compression)) return(invisible(NULL))
    if(!file.exists(file_in))
        stop(paste(file_in, "not found in change_tail_compression function"))
    stopifnot(is.numeric(tail_compression))
    dat <- readLines(file_in)
    ## The data sections are repeated in the data.ss_new files, so only use
    ## first one
    tail.line <- grep("#_comp_tail_compression", x=dat)[1]
    current <- as.numeric(strsplit(dat[tail.line], split=" ")[[1]][1])
    dat[tail.line] <-
        paste0(tail_compression,
               " #_comp_tail_compression; changed from: ",
               current)
    ## Write it back to file
    writeLines(dat, con=file_out)
    return(invisible(dat))
}
