#' Master function to run the FISH600-specific simulations
#'
#' This function first deals with parsing the case input files and
#' then passes these arguments on to \code{\link{ss3sim_base}}.
#'
#' @param iterations Which iterations to run. A numeric vector. For
#' example \code{1:100}.
#' @param scenarios Which scenarios to run. A vector of character
#' objects. For example \code{c("M1-F1-D1-R1-cod",
#' "M2-F1-D1-R1-cod")}. 
#' @param seed If set to a numeric vector then \code{set.seed} will be
#' set to each successive value of the vector \code{seed} on each
#' iteration. This can be useful to make simulations reproducible. If
#' left set to \code{NULL} then the seed will not be set. By default,
#' in \code{run_fish600}, this argument is passed a set of seed values
#' stored in the package data object \code{\link{fish600seeds}}. If
#' set, there should be a seed value for each iteration. Only affects
#' the observation error -- the process error (recruitment deviations)
#' are already fixed according to \code{\link{recdevs}} by default.
#' @param ... Anything else to pass to \code{\link{ss3sim_base}}. This
#' includes \code{bias_adjust} and \code{bias_nsim}. Also, you can
#' pass additional options to \code{SS3} through the argument
#' \code{admb_options}. Just don't pass the ADMB options \code{-noest}
#' or \code{-nohess} and enable bias adjustment.
#' @author Sean C. Anderson
#' @details
#' The operating model folder should contain: \code{forecast.ss},
#' \code{yourmodel.ctl}, \code{yourmodel.dat}, \code{ss3.par}, and
#' \code{starter.ss}. Nothing more and nothing less. The files should
#' be the versions returned as \code{.ss_new} files. This is important
#' because it creates consistent formatting which many of the
#' functions in this package depend on. Rename the \code{.ss_new}
#' files as listed above (and in all lowercase). The estimation model
#' folder should contain all the same files listed above except the
#' \code{ss3.par} and \code{yourmodel.dat} files, which are unnecessary.
#' 
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
#' @seealso \code{\link{run_ss3sim}}, \code{\link{ss3sim_base}},
#' \code{\link{run_ss3model}}, \code{\link{run_bias_ss3}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Without bias adjustment:
#' run_fish600(iterations = 1, scenarios = "D0-E0-F0-G0-M0-R0-S0-cod")
#'
#' # Run multiple scenarios (but only 1 iteration each):
#' run_fish600(iterations = 1, 
#' scenarios = c("D0-E0-F0-G0-M0-R0-S0-cod", "D0-E0-F0-G0-M1-R0-S0-cod"))
#'
#' # With bias adjustment:
#' # (Note that bias_nsim should be bigger, say 10, but it is set to 2
#' # here so the example runs faster.)
#' run_fish600(iterations = 1, scenarios = "M0-F0-D0-R0-S0-G0-E0-cod",
#' bias_adjust = TRUE, bias_nsim = 2)
#' 
#' # Restarting the previous run using the existing bias-adjustment
#' # output
#' run_fish600(iterations = 2:3, scenarios = "M0-F0-D0-R0-S0-G0-E0-cod",
#' bias_adjust = FALSE, bias_already_run = TRUE)
#' }

run_fish600 <- function(iterations, scenarios, seed = fish600seeds, ...) {

  case_folder <- get_fish600_casefolder()

  output <- lapply(scenarios, function(x) {
    a <- get_caseargs(folder = case_folder, scenario = x) 
    sp <- substr_r(x, 3)
    om_model_dir <- get_fish600_modelfolder(paste0(sp, "-om"))
    em_model_dir <- get_fish600_modelfolder(paste0(sp, "-em"))

    ss3sim_base(iterations, scenarios = x, m_params = a$M,
      sel_params = a$S, growth_params = a$G, f_params = a$F, index_params =
      a$index, lcomp_params = a$lcomp, agecomp_params = a$agecomp, 
      retro_params = a$R, estim_params = a$E, om_model_dir = om_model_dir, 
      em_model_dir = em_model_dir, seed = seed, ...) 
  })

  print(paste("Completed iterations:", paste(iterations, collapse = ", "), 
      "for scenarios:", paste(scenarios, collapse = ", ")))

}

