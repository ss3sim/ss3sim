#' Replace a \code{NULL} value with \code{NA} in a list
#'
#' Replace items with a zero length in a list with the value supplied in
#' the argument \code{replacement}. Useful for scenarios where \code{NULL}
#' is sometimes a valid input from legacy code. Nested  lists can behave
#' badly when they have a \code{NULL} entry, for example when converting
#' to a \code{\link[tibble]{tibble}}, it will be unnamed.
#'
#' @param x An object that potentially has a length of zero and you
#' wish it to be an actual value.
#' @param replacement The value you would like to use to replace items
#' with a length of zero. For example, the default \code{NA_integer_}
#' will replace all \code{NULL} values with {NA}. Other options for this
#' argument could be \code{NA_character_}.
#' @return The object \code{x} is returned with some items replaced.
#' If the input object was of zero length, then the \code{replacement}
#' parameter will  be returned instead.
#' @author Amanda from [stack overflow](https://stackoverflow.com/questions/22870198/is-there-a-more-efficient-way-to-replace-null-with-na-in-a-list)
#' @examples
#' employees <- list(
#'   list(id = 1,
#'     dept = "IT",
#'     age = 29,
#'     sportsteam = "softball"),
#'   list(id = 2,
#'     dept = "IT",
#'     age = 30,
#'     sportsteam = NULL),
#'   list(id = 3,
#'     dept = "IT",
#'     age = 29,
#'     sportsteam = "hockey"),
#'   list(id = 4,
#'     dept = NULL,
#'     age = 29,
#'     sportsteam = "softball"))
#' # Meat of the example here!
#' \dontrun{
#' # need %>% from [magrittr] package
#'  do.call(rbind, lapply(employees, rbind)) magrittr::%>% data.frame() %>%
#' purrr::modify_depth(2, replace_x)
#' }
#'
replace_x <- function(x, replacement = NA_integer_) {
  if (length(x) == 0 || length(x[[1]]) == 0) {
    return(replacement)
  } else {
    return(x)
  }
}