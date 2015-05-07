#' Setup parallel processing
setup_parallel <- function() {
  if(!requireNamespace("foreach", quietly = TRUE)) {
    stop("The foreach package is required for parallel scenario simulations.",
      call. = FALSE)
  }
  # to satisfy R CMD check:
  getDoParWorkers <- NULL

  cores <- foreach::getDoParWorkers()
  if(cores == 1) {
    warning(paste("You have only registered one core. Reverting to non-parallel",
      "processing. You need to register multiple cores first. For example:",
      "library(doParallel); registerDoParallel(cores = 2).",
      "See the help for ?run_ss3sim for an example."))
  }
  cores
}
