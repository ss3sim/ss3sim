#' Master function to run a set of simulations using \pkg{ss3sim}
#'
#' This is the main high-level wrapper function used to run
#' a set of \pkg{ss3sim} simulations.
#' The data frame passed to `simdf` is parsed into a list and used to control
#' [ss3sim_base()]. Alternatively, you can call [ss3sim_base()] directly
#' with your own lists.
#'
#' @param iterations A numeric vector specifying which iterations are desired.
#'   For example `1:100`.
#'   The same number of iterations will be ran for each scenario.
#'   If any iterations already have a folder from a previous run, they will be
#'   skipped even if they do not contain viable results.
#' @param simdf A data frame of instructions with one row per scenario.
#'   See [setup_scenarios_defaults()] for default values that will be
#'   used for a generic simulation to get you started.
#'   These default values will only work with the stored cod model because
#'   some of the columns in `simdf` need to have values that
#'   match the fleet structure of the operating model. If you are not using the
#'   default cod model, please remember to add `om_dir` and `em_dir` columns
#'   to `simdf` with file paths to the locations of your operating model and
#'   estimation method. Essentially, `simdf` is a way to pass scenario-specific
#'   information to the arguments of [ss3sim_base()], whereas the `...` method
#'   will only work for things like `seed` that are universal to all scenarios
#'   in a simulation.
#' @param parallel A logical argument that controls whether the scenarios are
#'   run in parallel. You will need to register multiple cores first with a
#'   package such as \pkg{doParallel} and have the \pkg{foreach} package
#'   installed. For example, the following code will register two cores
#'   and must be called before running `run_ss3sim()`:
#'
#'   ```
#'   library(doParallel)
#'   cl <- makeCluster(2)
#'   registerDoParallel(cl)
#'   ```
#' @param parallel_iterations A logical argument specifying if you wish to
#'   run iterations in parallel. If you set `parallel = TRUE` and
#'   `parallel_iterations = TRUE` then iterations for a given scenario will
#'   be sent to multiple processors. All iterations for a given scenario must
#'   finish before the next scenario is started.
#'   This will be useful if you want to run one scenario fast or if you want
#'   to be able to look at the results for each scenario as they finish in
#'   another instance of \R. The argument will be ignored if `parallel = FALSE`.
#' @param ... Anything else to pass to [ss3sim_base()]. This could
#'   include `bias_adjust`. Also, you can pass
#'   additional options to the executable through the argument
#'   `admb_options`.

#' @author Sean C. Anderson
#'
#' @details The operating model folder, which is passed as a file path
#' using `simdf[["om_dir"]]`, should contain the following files:
#' * `forecast.ss`,
#' * `yourmodel.ctl`,
#' * `yourmodel.dat`,
#' * `ss.par`, and
#' * `starter.ss`.
#' The files should be the formatted versions that are returned from
#' Stock Synthesis after the model is optimized, i.e., `.ss_new` files.
#' It is important to use these formatted files because many functions in
#' \pkg{ss3sim} and \pkg{r4ss} depend on the location of keywords present
#' in the comments and other standardized formatting. Once you have these
#' files from a successfully optimized model, rename the `.ss_new` files
#' to match the names listed above, though you can change `yourmodel` to
#' whatever name is listed for the control and data files in `starter.ss`.
#' The estimation model folder should also contain these files, except
#' `ss.par` and `yourmodel.dat` files, which are unnecessary.
#' See the vignette titled modifying-models for details on modifying an
#' existing Stock Synthesis model to run with \pkg{ss3sim}. Alternatively,
#' consider modifying the built-in model configuration based on north sea cod.
#'
#' Note that due to the way that Stock Synthesis is being used as an OM,
#' you may see the following error from ADMB in the console:
#' Error -- base = 0 in function prevariable& pow(const prevariable& v1, CGNU_DOUBLE u)
#' However, this is not a problem because ADMB is not used to optimize the OM,
#' and thus, the error can safely be ignored.
#'
#' @importFrom foreach %dopar%
#'
#' @return
#' The output will appear in your current \R working directory.
#' Folders will be named based on the `"scenario"` column
#' of `simdf` or based on the date-time stamp
#' (i.e., mmddhhmmss) generated automatically at the start of the simulation.
#' The resulting folders will look like the following if you run
#' your simulation at noon on January 01:
#' * `0101120000/1/om`
#' * `0101120000/1/em`
#' * `0101120000/2/om`
#' * ...
#'
#' @seealso
#' [ss3sim_base()] can be called directly by passing lists to each
#' individual argument rather than using the data-frame approach
#' of `run_ss3sim(simdf = )`.
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
          .verbose = TRUE,
        ) %dopar%
          do.call("ss3sim_base", c(x, list(iterations = it_), dots))
      })
    } else {
      message("Running scenarios in parallel.")
      ignore <- foreach::foreach(
        x = arg_list, .packages = "ss3sim",
        .verbose = FALSE,
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
