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
#' includes \code{bias_correct} and \code{bias_nsim}.
#' @author Sean C. Anderson
#' @details
#' The operating model folder should contain: forecast.ss,
#' yourmodel.ctl, yourmodel.dat, ss3.par, and starter.ss. Nothing more
#' and nothing less. The files should be the versions returned as
#' .ss_new files. This is important because it creates consistent
#' formatting which many of the functions in this package depend on.
#' Rename the .ss_new files as listed above The estimation model
#' folder should contain all the same files listed above except the
#' ss3.par file.
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
#' @seealso \code{\link{run_ss3sim}}
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
#' # Without bias correction:
#' run_fish600(iterations = 1, scenarios = c("M1-F1-D1-R1-cod"),
#' case_folder = case_folder, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir)
#'
#' # With bias correction:
#' # (Note that bias_nsim should be bigger, say 5, but it is set to 1
#' # here so the example runs faster.)
#' run_fish600(iterations = 1, scenarios = c("M1-F1-D1-R1-cod"),
#' case_folder = case_folder, om_model_dir = om_model_dir,
#' em_model_dir = em_model_dir, bias_correct = TRUE,
#' bias_nsim = 1)
#' }

run_fish600 <- function(iterations, scenarios, case_folder,
  om_model_dir, em_model_dir, ...) {

  lapply(scenarios, function(x) {
    a <- get_caseargs(folder = case_folder, scenario = x) 

    run_ss3sim(iterations, scenarios = x, m_params = a$M,
      f_params = a$F, index_params = a$index, lcomp_params = a$lcomp,
      agecomp_params = a$agecomp, om_model_dir = om_model_dir,
      em_model_dir = em_model_dir, ...) 
  })

}

