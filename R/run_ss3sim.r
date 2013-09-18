#' Master function to run SS3 simulations
#'
#' This function first deals with parsing the case input files and
#' then passes these arguments on to \code{\link{ss3sim_base}} to run
#' the simulation.
#'
#' @param iterations Which iterations to run. A numeric vector. For
#' example \code{1:100}.
#' @param scenarios Which scenarios to run. A vector of character
#' objects. For example \code{c("D0-E0-F0-G0-R0-S0-M0-cod",
#' "D1-E0-F0-G0-R0-S0-M0-cod")}. Also, see
#' \code{\link{expand_scenarios}} for a shortcut to specifying the
#' scenarios.
#' @param seed If set to a numeric vector then \code{set.seed} will be
#' set to each successive value of the vector \code{seed} on each
#' iteration. This can be useful to make simulations reproducible. If
#' left set to \code{NULL} then the seed will not be set. See, for
#' example, how this is set in \code{\link{run_fish600}} using the
#' package data object \code{\link{fish600seeds}}. If set, there
#' should be a seed value for each iteration. This only affects the
#' observation error -- the process error (recruitment deviations) are
#' already fixed according to \code{\link{recdevs}} by default.
#' @param ... Anything else to pass to \code{\link{ss3sim_base}}. This
#' includes \code{bias_adjust} and \code{bias_nsim}. Also, you can
#' pass additional options to \code{SS3} through the argument
#' \code{admb_options}.
#' @author Sean C. Anderson
#'
#' @details
#' The operating model folder should contain: \code{forecast.ss},
#' \code{yourmodel.ctl}, \code{yourmodel.dat}, \code{ss3.par}, and
#' \code{starter.ss}. Nothing more and nothing less. The files should
#' be the versions that are returned from an SS run as \code{.ss_new}
#' files. This is important because it creates consistent formatting
#' which many of the functions in this package depend on. Rename the
#' \code{.ss_new} files as listed above (and in all lowercase). The
#' estimation model folder should contain all the same files listed
#' above except the \code{ss3.par} and \code{yourmodel.dat} files,
#' which are unnecessary but can be included if desired.
#' 
#' @return
#' The output will appear in whatever your current R working directory
#' is. There will be folders named after your scenarios. They will
#' look like this:
#' \itemize{
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/bias/1/om}
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/bias/1/em}
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/bias/2/om}
#' \item ...
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/1/om}
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/1/em}
#' \item \code{D0-E0-F0-G0-M0-R0-S0-cod/2/om}
#' \item ...
#' }
#' @seealso \code{\link{ss3sim_base}}, \code{\link{run_ss3model}},
#' \code{\link{run_bias_ss3}}, \code{\link{run_fish600}}
#' @export
#'
#' @examples
#' \dontrun{
#' # TODO this example needs to be updated
#' # Without bias adjustment:
#' d <- system.file("extdata", package = "ss3sim")
#' case_folder <- paste0(d, "/eg-cases")
#' om <- paste0(d, "/models/cod-om")
#' em <- paste0(d, "/models/cod-em")
#' 
#' run_ss3sim(iterations = 1:1, scenarios = "D0-E0-F0-G0-R0-S0-M0-cod",
#'   case_folder = case_folder, om_model_dir = om, em_model_dir = em)
#' 
#' # With bias adjustment:
#' # (Note that bias_nsim should be bigger, say 10, but it is set to 2
#' # here so the example runs faster.)
#' run_ss3sim(iterations = 1:1, scenarios = "D1-E0-F0-G0-R0-S0-M0-cod",
#'   case_folder = case_folder, om_model_dir = om, em_model_dir = em,
#'   bias_adjust = TRUE, bias_nsim = 2)
#' 
#' # Restarting the previous run using the existing bias-adjustment
#' # output
#' run_ss3sim(iterations = 2:3, scenarios = "D1-E0-F0-G0-R0-S0-M0-cod",
#'   case_folder = case_folder, om_model_dir = om, em_model_dir = em,
#'   bias_adjust = FALSE, bias_already_run = TRUE)
#' 
#' # A deterministic run for model checking:
#' recdevs_det <- matrix(0, nrow = 100, ncol = 20)
#' run_ss3sim(iterations = 1:20, scenarios = "D0-E100-F0-G0-R0-S0-M0-cod",
#'   case_folder = case_folder, om_model_dir = om, em_model_dir = em,
#'   bias_adjust = TRUE, bias_nsim = 2, user_recdevs = recdevs_det)
#' }

run_ss3sim <- function(iterations, scenarios, case_folder,
  om_model_dir, em_model_dir, seed = NULL, ...) {

  output <- lapply(scenarios, function(x) {
    a <- get_caseargs(folder = case_folder, scenario = x) 
    sp <- substr_r(x, 3)
    ss3sim_base(iterations, scenarios = x,
      tv_params = a$tv_params,
      f_params = a$F, index_params =
      a$index, lcomp_params = a$lcomp, agecomp_params = a$agecomp, 
      retro_params = a$R, estim_params = a$E, om_model_dir = om_model_dir, 
      em_model_dir = em_model_dir, seed = seed, ...) 
  })

  print(paste("Completed iterations:", paste(iterations, collapse = ", "), 
      "for scenarios:", paste(scenarios, collapse = ", ")))

}
