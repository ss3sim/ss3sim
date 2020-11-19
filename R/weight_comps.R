#' Weight the composition data in the EM
#'
#' Weight age- or length-composition data in the estimation model (EM) using a
#' tuning method (MacAllister-Ianelli or Francis) or a distribution
#' (Dirichlet multinomial). This is essentially a wrapper around
#' r4ss::SS_tune_comps
#'
#' @param method  Options for the weighting method are
#'  "MI" (for MacAllister-Ianelli), "Francis", or "DM" (for Dirichlet
#'  multinomial). For now, only 1 option can be used.
#' @param init_run A logical value specifying if the model should be run before
#'  iterative re-weighting. Only relevant to \code{method = c("MI", "Francis")}
#'  because they require initial estimates that are compared to a theoretical
#'  distribution external to SS.
#' @param main_run A logical value specifying if the model should be run to
#'  weight the composition data after the appropriate weightings are found.
#'  If \code{FALSE}, the final values found for the weightings will only
#'  be put into the control file and not used to generate parameter estimates.
#' @param niters_weighting The number of times you want to tune the model.
#' @template dir
#' @param bias_adjust Run bias adjustment first?
#' @param hess_always If \code{TRUE}, the Hessian will always be calculated.
#'  If \code{FALSE}, the Hessian will only be calculated for
#'  bias-adjustment runs thereby saving time.
#' @param fleets A vector of numeric values specifying which fleets you want to
#'  tune.
#' @param ... Anything else to pass to \code{\link{run_ss3model}}.
weight_comps <- function(method = c("MI", "Francis", "DM"),
                         init_run = FALSE,
                         niters_weighting = 1,
                         dir,
                         bias_adjust = FALSE,
                         hess_always = FALSE,
                         fleets,
                         ...) {
  if (hess_always | bias_adjust) {
    extras <- ""
  } else {
    extras <- "-nohess"
  }

  bin <- get_bin(bin_name = "ss")
  out <- r4ss::SS_tune_comps(replist = NULL, fleets = fleets, option = method,
                      init_run = init_run, niters_tuning = niters_weighting,
                      dir = dir, model = bin, extras = extras,
                      allow_up_tuning = FALSE,
                      verbose = FALSE)
  invisible(out)
}
