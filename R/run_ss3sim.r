#' Master function to run simulations using Stock Synthesis
#'
#' This is the main high-level wrapper function for running \pkg{ss3sim}
#' simulations. First, this function separates the data frame of simulation
#' settings by row for each scenario into a list format. These lists are then
#' passed to \code{\link{ss3sim_base}} to run each simulation. Alternatively, you
#' can run \code{\link{ss3sim_base}} directly using your own lists.
#'
#' @param iterations Which iterations to run. A numeric vector. For example
#'   \code{1:100}. The same number of iterations will be run for each scenario.
#'   If any iterations already exist, then ss3sim will skip over them.
#' @param simdf A data frame of instructions with one row per scenario.
#'   The data frame replaces the old method of using case files.
#'   See \code{\link{setup_scenarios_defaults}} for default values that will be
#'   passed to ss3sim for a generic simulation to get you started. These
#'   values will be used if \code{simdf} is left at its default value of \code{NULL}.
#' @param scenarios Which scenarios to run. A vector of character objects. For
#'   example \code{c("D0-F0-cod", "D1-F1-cod")}. Also, see
#'   \code{\link{expand_scenarios}} for a shortcut to specifying the scenarios.
#'   See \code{\link{get_caseargs}} and the vignette for details on specifying
#'   the scenarios.
#' @param case_folder The folder containing the plain-text case files.
#' @param om_dir The folder containing the SS3 operating model
#'   configuration files.
#' @param em_dir The folder containing the SS3 estimation model
#'   configuration files.
#' @param case_files A named list that relates the case IDs to the files to
#'   return. \bold{The default list specifies only the required fishing
#'   mortality and data scenarios. To specify other cases you will need to
#'   extend this named list}. This argument is passed to
#'   \code{\link{get_caseargs}}. See that function for details and examples of
#'   how to specify this. The introduction vignette also explains how to specify
#'   the case files.
#' @param parallel A logical argument that controls whether the scenarios are
#'   run in parallel. You will need to register multiple cores first with a
#'   package such as \pkg{doParallel} and have the \pkg{foreach} package
#'   installed. See the example below.
#' @param parallel_iterations Logical. By default \code{parallel = TRUE} will
#'   run scenarios in parallel. If you set \code{parallel = TRUE} and
#'   \code{parallel_iterations = TRUE} then the iterations will be run in
#'   parallel. This would be useful if you were only running one scenario
#'   but you wanted to run it faster.
#' @param ... Anything else to pass to \code{\link{ss3sim_base}}. This could
#'   include \code{bias_adjust}. Also, you can pass
#'   additional options to the \code{SS3} command through the argument
#'   \code{admb_options}.

#' @author Sean C. Anderson
#'
#' @details The operating model folder should contain: \code{forecast.ss},
#' \code{yourmodel.ctl}, \code{yourmodel.dat}, \code{ss.par}, and
#' \code{starter.ss}. The files should be the versions that are returned from an
#' SS run as \code{.ss_new} files. This is important because it creates
#' consistent formatting which many of the functions in this package depend on.
#' Rename the \code{.ss_new} files as listed above (and in all lowercase). The
#' estimation model folder should contain all the same files listed above except
#' the \code{ss.par} and \code{yourmodel.dat} files, which are unnecessary but
#' can be included if desired. See the vignette for details on modifying an
#' existing \code{SS3} model to run with \pkg{ss3sim}. Alternatively, you might
#' consider modifying one of the built-in model configurations.
#' 
#' Note that due to the way that SS is being used as an OM, you may see 
#' the following ADMB error may appear in the console:
#' Error -- base = 0 in function prevariable& pow(const prevariable& v1, CGNU_DOUBLE u)
#' However, this is not a problem because ADMB is not used to optimize the OM,
#' and thus, the error can safely be ignored.
#'
#' @importFrom foreach %dopar%
#'
#' @return
#' The output will appear in your current \R working directory
#' Folders will be named based on the \code{"scenario"} column
#' of \code{simdf} or based on the date-time stamp
#' (i.e., mmddhhmmss) generated automatically at the start of the simulation.
#' The resulting folders will look like the following if you run
#' your simulation at noon on January 01:
#' \itemize{
#' \item \code{0101120000/1/om}
#' \item \code{0101120000/1/em}
#' \item \code{0101120000/2/om}
#' \item ...
#' }
#'
# An illustration of the input and output file structure
# of an \pkg{ss3sim} simulation:
# \figure{filestructure.png}{An illustration of the input and output file
# structure for an ss3sim simulation.}
#'
#' @seealso \code{\link{ss3sim_base}}, \code{\link{run_ss3model}},
#' \code{\link{get_caseargs}},
#' \code{\link{expand_scenarios}}
#' @export
#'
#' @examples
#'  \dontrun{
#' # A run with deterministic process error for model checking
#' # by passing user_recdevs to ss3sim_base through run_ss3sim:
#' recdevs_det <- matrix(0, nrow = 101, ncol = 2)
#' df <- data.frame(setup_scenarios_defaults(),
#'   "scenarios" = "determinate")
#' run_ss3sim(iterations = 1:2, simdf = df,
#'   bias_adjust = FALSE, user_recdevs = recdevs_det)
#' get_results_all(user_scenarios = "determinate", overwrite = TRUE)
#' ts <- read.csv("ss3sim_ts.csv")
#' expect_equivalent(unlist(ts$rec_dev[ts$year %in% 1:10 & ts$iteration == 2]),
#'   recdevs_det[1:10, 2])
#' }
#'
run_ss3sim <- function(iterations, simdf = NULL,
  scenarios = NULL, case_folder, om_dir = NULL, em_dir = NULL,
  case_files = list(F = "F", D = c("index", "lcomp", "agecomp")),
  parallel = FALSE, parallel_iterations = FALSE,
  ...) {

  if (!rlang::is_missing(case_folder)) {
    warning("The use of cases is deprecated, please use simdf to",
      "\nspecify the simulation parameters instead.")
  } else {case_folder <- NULL}
  if(parallel) {
    cores <- setup_parallel()
    if(cores == 1) parallel <- FALSE
  }

  # Get arguments for each scenario:
  if (!is.null(case_folder)) {
    arg_list <- lapply(scenarios, function(scenario) {
      a <- get_caseargs(folder = case_folder, scenario = scenario,
                        case_files = case_files)
      w <- get_weight_comps_args(a) # get the weight_comps args
      list(
        scenarios         = scenario,
        em_dir            = em_dir,
        om_dir            = om_dir,
        tv_params         = a$tv_params,
        operat_params     = a$O,
        f_params          = a$F,
        index_params      = a$index,
        data_params       = a$data,
        lcomp_params      = a$lcomp,
        agecomp_params    = a$agecomp,
        calcomp_params    = a$calcomp,
        wtatage_params    = a$wtatage,
        mlacomp_params    = a$mlacomp,
        em_binning_params = a$em_binning,
        retro_params      = a$retro,
        estim_params      = a$E,
        weight_comps_params = w)
    })
  } else {
    arg_list <- setup_scenarios(simdf)
  }
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
        foreach::foreach(it_ = iterations, .packages = "ss3sim",
          .verbose = TRUE, .export = "substr_r") %dopar%
            do.call("ss3sim_base",  c(x, list(iterations = it_), dots))
      })
    } else {
      message("Running scenarios in parallel.")
      ignore <- foreach::foreach(x = arg_list, .packages = "ss3sim",
        .verbose = FALSE, .export = "substr_r") %dopar%
          do.call("ss3sim_base", c(x, list(iterations = iterations, ...)))
    }
  } else {
    message("Running scenarios and iterations sequentially.")
    ignore <- lapply(arg_list, function(x) {
      do.call("ss3sim_base", c(x, list(iterations = iterations, ...)))
    })
  }

  message("Completed iterations: ", paste(iterations, collapse = ", "),
    " for scenarios: ", paste(scenarios, collapse = ", "))
  return(unlist(ignore))
}
