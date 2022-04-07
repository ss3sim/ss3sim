#' Set up bin structure for composition data
#'
#' Set up the bin structure needed for composition data.
#'
#' @param bins A vector of integer values, either lengths
#' or ages. Do not repeat them if you are using a two-sex model,
#' the function will do that for you.
#' @param nsex A single integer of one or two specifying the number
#' of sexes in the model.
#' @param leader Most users will not need to change the leader
#' character from the default unless you are working with age data,
#' then just use \`"a"` rather than the default of `"l"`.
#' If you have a two-sex model, i.e., `nsex = 2`, then the
#' function will take care of the naming for you, which is forced
#' to `"f"` and `"m"`. You can change them afterwards if you
#' want.
#' @seealso This is a helper function used to create the bins before
#' sampling takes place, see [ss3sim_base()].
#' @export
#' @examples
#' ex <- setup_bins(bins = 1:10, nsex = 2, leader = "a")
#' test <- length(ex) == 20 & all(grep("m", ex) == 11:20)
#' \dontshow{
#' testthat::expect_true(test)
#' }
#' ex <- setup_bins(bins = 1:5, nsex = 1)
#' test <- ex[4] == "l4"
#' \dontshow{
#' testthat::expect_true(test)
#' }
setup_bins <- function(bins, nsex = 1, leader = c("l", "a")) {
  leader <- match.arg(leader, several.ok = FALSE)
  leaders <- c(ifelse(nsex == 1, leader, "f"), "m")[1:nsex]
  out <- as.vector(t(outer(leaders, bins, FUN = paste0)))
  return(out)
}
