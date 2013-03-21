#' Find a vector of values from an object between 2 values
#' 
#' @param object The object (a vector) in which to search
#' @param what_vec A vector of length 2. The function will return the values of object between what_vec[1] + 1 and what_vec[2] - 2.
#' @examples
#' find_item(seq_len(10), c(3, 7))

find_item <- function(object, what_vec) {
   first <- grep(what_vec[1], object)
   last <- grep(what_vec[2], object)
   use_vec <- c((first+1), (last-2))
   return(use_vec)
}
