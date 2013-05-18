\name{run_fish600}
\alias{run_fish600}
\title{Master function to run the FISH600-specific simulations}
\usage{
  run_fish600(iterations, scenarios, case_folder,
    om_model_dir, em_model_dir, ...)
}
\description{
  This function first deals with parsing the case input
  files and then passes these arguments on to
  \code{\link{run_ss3sim}}.
}
\examples{
\dontrun{
d <- system.file("extdata", package = "ss3sim")
f <- paste0(d, "/run_ss3sim_eg/")
run_fish600(iterations = 1, scenarios = c("M1-F1-D1-R1-cod"),
case_folder = paste0(f, "case-arguments"), om_model_dir = paste0(f,
"cod_om"), em_model_dir = paste0(f, "cod_em"))
}
}

