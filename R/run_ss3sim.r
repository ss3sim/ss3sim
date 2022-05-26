#' Master function to run simulations using Stock Synthesis
#'
#' This is the main high-level wrapper function for running \pkg{ss3sim}
#' simulations. First, this function separates the data frame of simulation
#' settings by row for each scenario into a list format. These lists are then
#' passed to [ss3sim_base()] to run each simulation. Alternatively, you
#' can run [ss3sim_base()] directly using your own lists.
#'
#' @param iterations Which iterations to run. A numeric vector. For example
#'   `1:100`. The same number of iterations will be run for each scenario.
#'   If any iterations already exist, then ss3sim will skip over them.
#' @param simdf A data frame of instructions with one row per scenario.
#'   See [setup_scenarios_defaults()] for default values that will be
#'   passed to ss3sim for a generic simulation to get you started. These
#'   values will be used if `simdf` is left at its default value of `NULL`.
#' @param parallel A logical argument that controls whether the scenarios are
#'   run in parallel. You will need to register multiple cores first with a
#'   package such as \pkg{doParallel} and have the \pkg{foreach} package
#'   installed. See the example below.
#' @param parallel_iterations Logical. By default `parallel = TRUE` will
#'   run scenarios in parallel. If you set `parallel = TRUE` and
#'   `parallel_iterations = TRUE` then the iterations will be run in
#'   parallel. This would be useful if you were only running one scenario
#'   but you wanted to run it faster.
#' @param ... Anything else to pass to [ss3sim_base()]. This could
#'   include `bias_adjust`. Also, you can pass
#'   additional options to the executable through the argument
#'   `admb_options`.

#' @author Sean C. Anderson
#'
#' @details The operating model folder should contain: `forecast.ss`,
#' `yourmodel.ctl`, `yourmodel.dat`, `ss.par`, and
#' `starter.ss`. The files should be the versions that are returned from an
#' Stock Synthesis run as `.ss_new` files. This is important because it creates
#' consistent formatting which many of the functions in this package depend on.
#' Rename the `.ss_new` files as listed above (and in all lowercase). The
#' estimation model folder should contain all the same files listed above except
#' the `ss.par` and `yourmodel.dat` files, which are unnecessary but
#' can be included if desired. See the vignette for details on modifying an
#' existing Stock Synthesis model to run with \pkg{ss3sim}. Alternatively, you might
#' consider modifying one of the built-in model configurations.
#'
#' Due to the way that Stock Synthesis is being used as an OM, you may see
#' the following ADMB error may appear in the console:
#' `Error -- base = 0 in function prevariable& pow(const prevariable& v1, CGNU_DOUBLE u)`
#' However, this is not a problem because ADMB is not used to optimize the OM,
#' and thus, the error can safely be ignored.
#'
#' @importFrom foreach %dopar%
#'
#' @return
#' The output will appear in your current \R working directory
#' Folders will be named based on the `"scenario"` column
#' of `simdf` or based on the date-time stamp
#' (i.e., `mmddhhmmss`) generated automatically at the start of the simulation.
#' The resulting folders will look like the following if you run
#' your simulation at noon on January 01:
#' * `0101120000/1/om`
#' * `0101120000/1/em`
#' * `0101120000/2/om`
#' * ...
#'
# An illustration of the input and output file structure
# of an \pkg{ss3sim} simulation:
# \figure{filestructure.png}{An illustration of the input and output file
# structure for an ss3sim simulation.}
#'
#' @seealso
#' [ss3sim_base()] can be called directly by passing lists of arguments,
#' rather than using the data frame approach with `run_ss3sim`.
#' The lists correspond to each function called by [ss3sim_base()].
#' Each element is itself a list of arguments for the given function.
#' Either way allows users to pass arguments to each of the
#' `change_*()` or `sample_*()` functions.
#' Note that if you do not include an argument,
#' then [ss3sim_base()] will assume the value of that argument is `NULL`.
#'
#' [run_ss3model()] is called by [ss3sim_base()].
#'
#' @export
#' @import lifecycle
#' @examples
#' \dontrun{
#' # A run with deterministic process error for model checking
#' # by passing user_recdevs to ss3sim_base through run_ss3sim:
#' recdevs_det <- matrix(0, nrow = 101, ncol = 2)
#' df <- data.frame(setup_scenarios_defaults(),
#'   "scenarios" = "determinate"
#' )
#' run_ss3sim(
#'   iterations = 1:2, simdf = df,
#'   bias_adjust = FALSE, user_recdevs = recdevs_det
#' )
#' get_results_all(user_scenarios = "determinate", overwrite = TRUE)
#' ts <- utils::read.csv("ss3sim_ts.csv")
#' expect_equivalent(
#'   unlist(ts$rec_dev[ts$year %in% 1:10 & ts$iteration == 2]),
#'   recdevs_det[1:10, 2]
#' )
#' }
#'
run_ss3sim <- function(iterations,
                       simdf = NULL,
                       parallel = FALSE,
                       parallel_iterations = FALSE,
                       ...) {
  if (parallel) {
    cores <- setup_parallel()
    if (cores == 1) parallel <- FALSE
  }
  arg_list <- setup_scenarios(simdf)
  # Note that inside a foreach loop you pop out of your current
  # environment until you go back into an exported function
  # therefore we need to add subst_r to the .export list
  # for foreach to work on Windows:

  # to satisfy R CMD check in the foreach() call below
  x <- NULL
  it_ <- NULL

  if (parallel) {
    if (parallel_iterations) {
      ignore <- lapply(arg_list, function(x) {
        dots <- list(...)
        message("Running iterations in parallel.")
        foreach::foreach(
          it_ = iterations, .packages = "ss3sim",
          .verbose = TRUE, .export = "substr_r"
        ) %dopar%
          do.call("ss3sim_base", c(x, list(iterations = it_), dots))
      })
    } else {
      message("Running scenarios in parallel.")
      ignore <- foreach::foreach(
        x = arg_list, .packages = "ss3sim",
        .verbose = FALSE, .export = "substr_r"
      ) %dopar%
        do.call("ss3sim_base", c(x, list(iterations = iterations, ...)))
    }
  } else {
    message("Running scenarios and iterations sequentially.")
    ignore <- lapply(arg_list, function(x) {
      do.call("ss3sim_base", c(x, list(iterations = iterations, ...)))
    })
  }

  message(
    "Completed iterations: ", paste(iterations, collapse = ", "),
    " for scenarios: ", paste(unlist(ignore), collapse = ", ")
  )
  return(unlist(ignore))
}
