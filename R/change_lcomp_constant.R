#' Set the robustification constant for length composition data.
#'
#' This function replaces the robustification value for length composition
#' data in a \code{dat} file (\code{file_in}) with those specified in
#' \code{lcomp_constant}. It then writes a new file with name
#' \code{file_out} into the working directory.
#'
#' @details The robustification constant is added to both the observed and
#' expected proportions of length composition data, before being normalized
#' internally. It is designed to help stabilize the model, but is unclear
#' how and when to use it for optimal effect. The same value is used for
#' all length data.
#' @param lcomp_constant The new value to be used. Must be a numeric value,
#' as a proportion. For example 0.1 means 10 percent. See the SS3 manual
#' for further information. A NULL value indicates no action resulting in
#' using the current value, and a value of 0 will effectively turn off the
#' feature.
#' @param file_in Input SS3 data file.
#' @param file_out Output SS3 dat file. Typically the same as \code{file_in}.
#' @return A modified SS3 \code{.dat} file, and that file returned
#' invisibly (for testing) as a vector of character lines.
#' @author Cole Monnahan
#' @export

#' @examples
#' ## Create a temporary folder for the output:
#' temp_path <- file.path(tempdir(), "ss3sim-lcomp-constant-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' ## Run the function with built-in data file.
#' dat_file <- system.file("extdata", "example-om", "data.ss_new",
#'   package = "ss3sim")
#' test <- change_lcomp_constant(lcomp_constant = .1234, file_in = dat_file,
#'   file_out = paste0(temp_path, "/test.dat"))
#' ## Look at the changes
#' test[grep("#_add_to_comp", test)[1]]
#' ## Clean up the temp files
#' unlink(temp_path)
change_lcomp_constant <- function(lcomp_constant, file_in, file_out){
    ## Check inputs
    if(is.null(lcomp_constant)) return(invisible(NULL))
    if(!file.exists(file_in))
        stop(paste(file_in, "not found in change_lcomp_constant function"))
    stopifnot(is.numeric(lcomp_constant))
    dat <- readLines(file_in)
    ## The data sections are repeated in the data.ss_new files, so only use
    ## first one
    const.line <- grep("#_add_to_comp", x=dat)[1]
    current <- as.numeric(strsplit(dat[const.line], split=" ")[[1]][1])
    dat[const.line] <-
        paste0(lcomp_constant, " #_add_to_comp; changed from: ", current)
    ## Write it back to file
    writeLines(dat, con=file_out)
    return(invisible(dat))
}
