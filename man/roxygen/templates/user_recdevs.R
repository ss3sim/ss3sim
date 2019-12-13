#' @param user_recdevs An optional matrix of recruitment deviations to replace
#'   the recruitment deviations built into the package. The columns represent
#'   run iterations and the rows represent years. \code{user_recdevs} can be a
#'   matrix of 0s for deterministic model checking. For traditional stochastic
#'   simulations these would be independent and normally distributed deviations
#'   with a standard deviation equal to the desired sigma R. Note that these
#'   recruitment deviations will be used verbatim (after exponentiation).
#'   \code{user_recdevs} will *not* be multiplied by sigma R and they will *not*
#'   be log-normal bias corrected. If \code{user_recdevs} are specified as
#'   anything besides \code{NULL} the package will issue a warning about this.
#'   Biased recruitment deviations can lead to biased model results.
