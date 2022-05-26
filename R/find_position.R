#' Find integer reference to fleet names
#'
#' Find the position of each desired value, i.e., `x`,
#' in a vector of strings.
#' Builds on [match()] by allowing `x` to be a combination of
#' strings to be matched and known positions.
#'
#' @param x A vector of strings and/or integers to be matched.
#' @param table A vector of strings. If the character strings includes
#' values that can be coerced to integers, they must be in a matching position
#' in the vector. For example, `table = c("3","4","1")` will confuse the
#' function because `x = 1` will produce a return value of 3 rather than 1.
#' @export
#' @return An integer vector indicating the positions of `x` in `table`.
#' Same as [match()], if `x[i]` is found to be equal to `table[j]`, then
#' the value returned in the `ith` position of the integer vector is `j`.
#' The smallest value of `j`, i.e., the first match, is always returned.
#'
#' `find_position()` differs from [match()] in three ways.
#' First, values of `x` that are not found are removed. Thus, the length of
#' the integer vector has the potential to be shorter than the length of `x`.
#' Second, `x` can contain a mix of integer positions that are already known
#' and strings to be found.
#' Third, `table` cannot include integers that do not match their position.
#' See the specifications for `table` for more details.
#' @seealso See [match()] for a more formal version of `find_position()`
#' that returns an integer vector the same length as `x`.
#' @author Kelli F. Johnson
#' @examples
#' # Standard use
#' find_position(c("sad", 1), c("happy", "sad"))
#' # Incorrect use
#' find_position(c("sad", 2), c("happy", "sad", "2"))
find_position <- function(x, table) {
  # todo: write a check for `table` that ensures integers are in their correct
  # position, e.g., c("yes", 2, "no") not c("yes", 3, "no"), if present.
  characterint <- ifelse(
    test = is.na(match(x, table)),
    yes = match(x, seq_along(table)),
    no = match(x, table)
  )
  nonas <- characterint[!is.na(characterint)]
  return(as.integer(nonas))
}
