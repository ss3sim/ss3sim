#' Sample length compositions from a Stock Synthesis data file
#'
#' Extract length-composition data from a `.ss_new` data file and sample
#' the data. It is assumed that the composition data will be expected values
#' as written by Stock Synthesis in the second section of the data file, but
#' one can also sample input data. The resulting length-composition
#' data are assumed to represent observed length composition and will overwrite
#' the length data in `dat_list`, which is returned invisibly.
#' The data file can also be written to the disk, if a file path is provided to
#' `outfile`, and used as simulated data by an estimation model.
#'
#' @author Cole Monnahan and Kotaro Ono
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @template dat_list
#' @template outfile
#' @template Nsamp
#' @template sampledots
#' @template sampling-return
#'
#' @examples
#' dat_list <- r4ss::SS_readdat(
#'   verbose = FALSE,
#'   file = system.file(file.path("extdata", "models", "cod-om", "codOM.dat"),
#'     package = "ss3sim"
#'   )
#' )
#' ## Generate with constant sample size across years
#' ex1 <- sample_lcomp(
#'   dat_list = dat_list, outfile = NULL,
#'   fleets = 1:2, Nsamp = list(100, 50),
#'   years = list(seq(26, 100, by = 2), 80:100)
#' )
#' \dontshow{
#' testthat::expect_equal(dim(ex1[["lencomp"]]), c(59, 51))
#' }
#'
#' @export
#' @family sampling functions
#' @seealso [sample_agecomp()] for more examples.

sample_lcomp <- function(dat_list, outfile = NULL, fleets, Nsamp,
                         years, cpar = 1, ESS = NULL, ...) {
  check_data(dat_list)
  dat_list$lencomp <- sample_comp(dat_list$lencomp,
    Nsamp = Nsamp, fleets = fleets, years = years,
    cpar = cpar, ESS = ESS, ...
  )
  dat_list$N_lencomp <- nrow(dat_list$lencomp)

  ## Write the modified file
  if (!is.null(outfile)) {
    r4ss::SS_writedat(
      datlist = dat_list,
      outfile = outfile,
      overwrite = TRUE,
      verbose = FALSE
    )
  }
  invisible(dat_list)
}
