#' Create matching column names across a list of data frames
#'
#' Add missing columns to each data frame in the list allowing
#' for the use [rbind()] to create a single data frame.
#' The code is based on `rbind.fill` from the `plyr` package.
#'
#' @param dfs A list of data frames, where the length can be one.
#' @param bind A logical value specifying if the data frame(s)
#' should be returned as a single data frame. The default is
#' `FALSE`, which returns a list of data frames same as what
#' was provided in `dfs`.
#' @param fillwith A single value that will be used to populate all
#' of the missing columns.
#' @author Kelli F. Johnson
#' @return Depending on the input to `bind` you can either
#' return the same structure, i.e., a list of data frames, or
#' a data frame with all rows from each original data frame.
#' Missing values will be filled with the entry in `fillwith`.
#' @examples
#' x <- data.frame("a" = 1:10, "b" = 21:30)
#' y <- data.frame("a" = 11:15, "y" = letters[1:5])
#' alist <- ss3sim:::add_colnames(list(x, y), bind = FALSE)
#' adataframe <- ss3sim:::add_colnames(list(x, y), bind = TRUE)
#' # clean up
#' rm(x, y, alist, adataframe)
add_colnames <- function(dfs, bind = FALSE, fillwith = NA) {
  vars <- unique(unlist(lapply(dfs, base::names)))
  newdfs <- lapply(dfs, function(x) {
    missing <- setdiff(vars, names(x))
    x[, missing] <- fillwith
    return(x)
  })
  if (bind) newdfs <- do.call("rbind", newdfs)
  return(newdfs)
}
