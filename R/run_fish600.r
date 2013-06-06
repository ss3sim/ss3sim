#' Master function to run the FISH600-specific simulations
#'
#' This function first deals with parsing the case input files and
#' then passes these arguments on to \code{\link{run_ss3sim}}.
#'
#' @param iterations Which iterations to run. A numeric vector. For
#' example \code{1:100}.
#' @param scenarios Which scenarios to run. A vector of character
#' objects. For example \code{c("M1-F1-D1-R1-cod", "M2-F1-D1-R1-cod")}.
#' @param case_folder The location of the folder containing the
#' case-specific argument control files. This is the folder that holds
#' files such as \code{M1-cod.txt} and \code{index1-cod.txt}.
#' @param om_model_dir The location of the folder containing the
#' operating model you want to use. 
#' @param em_model_dir The location of the folder containing the
#' estimation model you want to use. 
#' @param ... Anything else to pass to \code{\link{run_ss3sim}}. This
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
#' \item \code{M1-F1-D1-R1-cod/bias/1/om}
#' \item \code{M1-F1-D1-R1-cod/bias/1/em}
#' \item \code{M1-F1-D1-R1-cod/bias/2/om}
#' \item ...
#' \item \code{M1-F1-D1-R1-cod/1/om}
#' \item \code{M1-F1-D1-R1-cod/1/em}
#' \item \code{M1-F1-D1-R1-cod/2/om}
#' \item ...
#' }
#' @seealso \code{\link{run_ss3sim}}, \code{\link{run_ss3model}},
#' \code{\link{run_bias_ss3}}
#' @export
#'
#' @examples
#' \dontrun{
#' # Pull in file paths from the package example data:
#' d <- system.file("extdata", package = "ss3sim")
#' f <- paste0(d, "/run_ss3sim_eg/")
#' om_model_dir <- paste0(f, "cod_om")
#' em_model_dir <- paste0(f, "cod_em")
#' case_folder <- paste0(f, "case-arguments")
#' 
#' # Without bias adjustment:
#' run_fish600(iterations = 1, scenarios = c("M1-F1-D1-R1-S1-G1-cod"),
#' case_folder = case_folder, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir)
#'
#' # With bias adjustment:
#' # (Note that bias_nsim should be bigger, say 5, but it is set to 2
#' # here so the example runs faster.)
#' run_fish600(iterations = 1, scenarios = c("M1-F1-D1-R1-cod"),
#' case_folder = case_folder, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir, bias_adjust = TRUE,
#' bias_nsim = 2)
#' }

run_fish600 <- function(iterations, scenarios, case_folder,
  om_model_dir, em_model_dir, ...) {

  junk <- lapply(scenarios, function(x) {
    a <- get_caseargs(folder = case_folder, scenario = x) 

    run_ss3sim(iterations, scenarios = x, m_params = a$M,
      sel_params = a$S, growth_params = a$G, f_params = a$F, index_params =
      a$index, lcomp_params = a$lcomp, agecomp_params = a$agecomp, 
      retro_params = a$R, om_model_dir = om_model_dir, 
      em_model_dir = em_model_dir, ...) 
  })

  print(paste("Completed iterations:", paste(iterations, collapse = ", "), 
      "for scenarios:", paste(scenarios, collapse = ", ")))

}

