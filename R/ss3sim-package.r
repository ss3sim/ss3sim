#' ss3sim: Fisheries stock assessment simulation testing with Stock Synthesis
#'
#' The \pkg{ss3sim} \R package is designed to
#' simplify the steps needed to generate beautiful simulation output from the
#' Stock Synthesis statistical catch-at-age stock assessment framework.
#'
#' An \pkg{ss3sim} simulation requires three types of input: (1) a base
#' model of the underlying truth (a Stock Synthesis operating model), (2) a base
#' model of how you will assess that truth (a Stock Synthesis estimation model),
#' (3) and a data frame specifying how you want to manipulate the
#' operating and estimation models.
#'
#' You can find example an operating and estimation model
#' within the package data (`inst/extdata/models/`).
#'
#' To carry out \pkg{ss3sim} simulations with the CRAN version of ss3sim,
#' you will need to have Stock Synthesis installed on your computer and
#' the binary needs to be in the path that \R sees.
#' See
#' [Stock Synthesis, Getting Started](https://nmfs-stock-synthesis.github.io/ss-documentation/Getting_Started_SS.html#the-path-approach) and
#' the [Introduction vignette](https://ss3sim.github.io/ss3sim/articles/introduction.html)
#' for instructions on
#' making sure Stock Synthesis will work from within \R.
#'
#' The main \pkg{ss3sim} functions are divided into three types:
#'
#' 1. `change_*()` and `sample_*()` functions that manipulate Stock Synthesis
#' configuration files. These manipulations generate an underlying "truth"
#' (operating models) and control our assessment of those models (estimation
#' models).
#' \itemize{
#' \item [change_f()]: Controls fishing mortality.
#'
#' \item [change_tv()]: Adds time-varying features. For
#' example, time-varying natural mortality, growth, or selectivity.
#'
#' \item [sample_lcomp()]: Controls how length-composition
#' data are sampled.
#'
#' \item [sample_agecomp()]: Controls how age-composition
#' data are sampled.
#'
#' \item [sample_index()]: Controls how the fishery and
#' survey indices are sampled.
#'
#' \item [change_e()]: Controls which and how parameters are
#' estimated.
#'
#' \item [change_retro()]: Controls the number of years to
#' discard for a retrospective analysis.
#'
#' \item [change_rec_devs()]: Substitutes recruitment
#' deviations.
#'
#' \item [change_lcomp_constant()]: Set the robustification constant
#' for length composition data.
#'
#' \item [change_tail_compression()]: Replace tail compression value
#' for length composition data.
#' }
#'
#' 2. `run_*()` functions that conduct simulations. These functions
#' generate a folder structure, call manipulation functions, run Stock Synthesis
#' as needed, and save the output.
#' \itemize{
#' \item [run_ss3sim()]: Main function to run \pkg{ss3sim}
#' simulations.
#'
#' \item [ss3sim_base()]: Underlying base simulation
#' function. Can also be called directly.
#' }
#'
#' 3. `get_*()` functions for synthesizing the output.
#' \itemize{
#' \item [get_results_scenario()]: Extract the results for a
#' single scenario.
#'
#' \item [get_results_all()]: Extract results from a series
#' of scenarios.
#' }
#'
#' See the introductory vignette `vignette("introduction", package = "ss3sim")`
#' for more extensive explanation of how to use the \pkg{ss3sim} \R package.
#'
#' \pkg{ss3sim} was developed by graduate students and post doctoral researchers
#' at the University of Washington (School of Aquatic and Fishery Sciences and
#' Quantitative Ecology and Resource Management departments) and Simon Fraser
#' University. The authors of individual functions are listed within the
#' function documentation and all contributors are listed in the
#' `DESCRIPTION` file.
#'
#' If you use \pkg{ss3sim} in a publication, please cite the package as
#' indicated by running `citation("ss3sim")` in the \R console.
#'
#' @docType package
#' @keywords internal
"_PACKAGE"

globalVariables(
  c(
    "Area",
    "Discard",
    "Flt",
    "ID",
    "Lbin_lo",
    "Seas",
    "Std_in",
    "Yr",
    "arg",
    "catch", "catch_se",
    "cummean",
    "data",
    "fleet",
    "label",
    "ncalc",
    "rowname",
    "seas",
    "type",
    "useESS_var",
    "value"
  )
)
