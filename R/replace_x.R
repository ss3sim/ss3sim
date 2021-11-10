#' Replace a `NULL` value with `NA` in a list
#'
#' Replace items with a zero length in a list with the value supplied in
#' the argument `replacement`. Useful for scenarios where `NULL`
#' is sometimes a valid input from legacy code. Nested  lists can behave
#' badly when they have a `NULL` entry, for example when converting
#' to a [tibble::tibble()], it will be unnamed.
#'
#' @param x An object that potentially has a length of zero and you
#' wish it to be an actual value.
#' @param replacement The value you would like to use to replace items
#' with a length of zero. For example, the default `NA_integer_`
#' will replace all `NULL` values with {NA}. Other options for this
#' argument could be `NA_character_`.
#' @return The object `x` is returned with some items replaced.
#' If the input object was of zero length, then the `replacement`
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
