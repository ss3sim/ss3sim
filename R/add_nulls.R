#' Add NULL values to non-existent list elements
#'
#' @param param_list A list in which the names correspond to parameter names
#'   and the values correspond to the values to be passed.
#' @param desired_params A character vector of desired list elements.
#' @return A list with the desired elements as described by the
#'   \code{desired_params} argument. Any values that were missing in
#'   \code{param_list} will be returned with values of \code{NULL}.
#' @author Sean C. Anderson
# @examples
# add_nulls(list(a = 1, b = 2), c("a", "b", "d"))

add_nulls <- function(param_list, desired_params) {
  out_param_list <- vector(mode = "list", length = length(desired_params))
  names(out_param_list) <- desired_params
  names_param_list <- names(param_list)
  sapply(desired_params, function(x) {
    if(x %in% names_param_list)
      param_list[[seq_along(names_param_list)[x == names_param_list]]]
    },
    simplify = FALSE)
}
