#' Break apart composition data
#' 
#' Composition data typically includes meta data in the beginning columns followed
#' by a matrix of values by age, length, etc.
#' 
#' @template data
#' @return A list of four data frames and two integer values. The latter specify
#' the number of columns that contain meta data and the number of rows of data.
#' @author Kelli Faye Johnson
#'
split_comp <- function(data) {
  meta <- grep("^[a-zA-Z]+$", colnames(data))
  # match age0, a0, or 0 b/c * is any number of characters in bracket
  ages <- grep("^[age]*[0-9]+$", colnames(data))
  samp <- grep("^N_", colnames(data))
  list(
    "meta" = data[, meta],
    "data" = data[, c(meta, ages)],
    "raw" = type.convert(data[, ages]),
    "samp" = data[, c(meta, samp)], "nmeta" = max(meta),
    "n" = NROW(data))
}
