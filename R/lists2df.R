#' Combine multiple lists element wise to a single data frame
#'
#' Take the `ith` element from each list and combine into a data frame
#' and then `rbind` the resulting data frames to form a single data frame.
#'
#' @param ... Lists of the same length
#' @keywords internal
#' @return A data frame. With one column for each vector present in
#' the lists, e.g., if the lists have two vectors then the resulting
#' data frame will have two columns. An additional column will be added
#' called `"index"` that specifies which vector the information
#' was taken from within each list. For example, if each list has two
#' vectors and the first vector is of length 3 and the second is of
#' length 4, then the index column will be `c(rep(1,3), rep(2,4))`.
#' @author Kelli F. Johnson
#' @examples
#' ss3sim:::lists2df(list(1:2, 5:10), list(3:4, 5:10))
#' ss3sim:::lists2df(years = list(1:2, 5:10), fvals = list(3:4, 5:10))
#' ss3sim:::lists2df(years = list(1:2, 5:10))
#' \dontshow{
#' testthat::expect_error(
#'   ss3sim:::lists2df(years = list(1:10, 5:10), fisheries = 1:2)
#' )
#' }
#'
lists2df <- function(...) {
  lists <- list(...)
  if (length(unique(sapply(lists, length))) != 1) {
    stop("Lists are not all the same length.")
  }
  lvecs <- do.call("rbind", rapply(lists, length, how = "list"))
  if (all(sapply(apply(lvecs, 2, unique), length) != 1)) {
    stop(
      "Vectors within each list are not the same length for each",
      " ith element of the input lists."
    )
  }
  do.call("rbind", mapply(data.frame,
    "index" = seq_along(lists[[1]]),
    ..., SIMPLIFY = FALSE, MoreArgs = list(fix.empty.names = FALSE, stringsAsFactors = FALSE)
  ))
}

#' Replicate an object a number of times to return a list
#'
#' Replicate the input scalar argument the provided number of times,
#' where `length` can be a vector, and thus, the returned object
#' will be a list with potentially more than one entry in the list.
#' This is helpful when an input argument needs to be repeated to match
#' other input arguments that are lists of vectors when creating data frames.
#'
#' @param scalar A single numeric value.
#' @param length A vector of integer values specifying how many times `scalar`
#' should be repeated in each list element.
#' @keywords internal
#' @return A list of the same length as `length`. If, `scalar` is not
#' a list but a vector, then it will be returned as a list.
scalar2list <- function(scalar, length) {
  if (all(mapply(length, scalar) == 1) && length(scalar) == length(length)) {
    out <- mapply(rep, x = scalar, times = length, SIMPLIFY = FALSE)
  }
  if (length(unlist(scalar)) != 1 && length(length) == 1) {
    if (is.list(scalar)) {
      out <- scalar
    } else {
      out <- list(scalar)
    }
  }
  if (length(unlist(scalar)) == 1 && length(length) > 1) {
    out <- lapply(length, function(x) rep(scalar, length = x))
    nm <- deparse(substitute(scalar))
    names(out) <- gsub(
      "(.+)\\..+\\.(.+)",
      paste0("\\1\\.", nm, "\\.\\2"), names(out)
    )
  }
  if (length(scalar) == length(length) &&
    all(mapply(length, scalar) == length)) {
    # Good to go
    out <- scalar
  }
  if (!exists("out")) {
    message(
      "Sorry, scalar2list did not work properly.",
      "\nSee the data structures below."
    )
    message("scalar is")
    utils::capture.output(type = "message", scalar)
    message("length is")
    utils::capture.output(type = "message", length)
    stop("scalar2list was terminated.", call. = FALSE)
  }

  testthat::expect_equivalent(length, sapply(out, length),
    label = "\nscalar2list: specified 'length' is\n"
  )
  return(out)
}
