#' Sample age compositions from a Stock Synthesis data file
#'
#' Extract age-composition data from a `.ss_new` data file and sample
#' the data. It is assumed that the composition data will be expected values
#' as written by Stock Synthesis in the second section of the data file, but
#' one can also sample input data. The resulting age-composition
#' data are assumed to represent observed age composition and will overwrite
#' the age data in `dat_list`, which is returned invisibly.
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
#' @param keep_conditional A logical if conditional age-at-length data
#'   should be kept or removed entirely from the data file.
#'   `sample_agecomp` only works on the age-composition data
#'   and not on the conditional age-at-length data. To sample the
#'   conditional data, set `keep_conditional` to `TRUE`
#'   and use [sample_calcomp()].
#' @template sampledots
#' @template sampling-return
#'
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- file.path(d, "models", "cod-om", "codOM.dat")
#' dat_list <- r4ss::SS_readdat(f_in, version = NULL, verbose = FALSE)
#'
#' ## Turn off age comps by specifying fleets=NULL
#' test <- sample_agecomp(dat_list = dat_list, fleets = NULL)
#'
#' ## Generate with a smaller number of fleet taking samples
#' ex1 <- sample_agecomp(dat_list = dat_list, outfile = NULL,
#'   fleets = 2, Nsamp = list(c(10, 50)), years = list(c(26, 27)))
#' NROW(ex1$agecomp) == 2
#'
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- sample_agecomp(dat_list = dat_list, outfile = NULL,
#'   fleets = c(1, 2),
#'   Nsamp = list(c(rep(50, 5), rep(100, 5)), 50),
#'   years = list(seq(26, 44, 2), c(26:100)))
#'
#' ## Run three  cases showing Multinomial, Dirichlet(1), and over-dispersed
#' ## Dirichlet for different levels of sample sizes
#' op <- graphics::par(mfrow = c(1, 3))
#' set.seed(1)
#' true <- prop.table(dat_list$agecomp[
#'   dat_list$agecomp$FltSvy == 1 & dat_list$agecomp$Yr == 50, -(1:9)])
#' cpars <- c(NA, 1, 4)
#' for (samplesize in c(30, 100, 1000)) {
#'   if (samplesize > 30) graphics::par(mar = c(5.1, 1, 4.1, 2.1))
#'   graphics::plot(dat_list$agebin_vector, true, type = "b", ylim = c(0, 1),
#'     col = 4, lwd = 2, xlab = "Age",
#'     ylab = ifelse(samplesize == 30, "Proportion", ""),
#'     main = paste("Sample size =", samplesize))
#'   if (samplesize == 30) {
#'     graphics::legend("topright", lty = 1, col = 1:4, bty = "n",
#'       legend = c("Multinomial", "Dirichlet(1)", "Dirichlet(4)", "Truth"))
#'   }
#'   for (i in seq_along(cpars)) {
#'     ex <- sample_agecomp(dat_list = dat_list, outfile = NULL, fleets = 1,
#'       Nsamp = list(samplesize), years = list(50), cpar = cpars[i])$agecomp
#'     lines(dat_list$agebin_vector, prop.table(ex[1, -(1:9)]),
#'       col = i, type = "b")
#'   }
#' }
#' graphics::par(op)
#' @family sampling functions
#' @export
sample_agecomp <- function(dat_list, outfile = NULL, fleets, Nsamp,
  years, cpar = 1, ESS = NULL, keep_conditional = TRUE, ...) {

    check_data(dat_list)
    agecomp <- dat_list$agecomp
    ## Split the conditional data from the age data
    if (keep_conditional) {
      conditional_data <- dat_list$agecomp[dat_list$agecomp$Lbin_lo >= 0, ]
    } else {
      conditional_data <- dat_list$agecomp[0, ]
    }
    dat_list$agecomp <- rbind(
      sample_comp(
        data = dat_list$agecomp[dat_list$agecomp$Lbin_lo < 0, ],
        Nsamp = Nsamp, fleets = fleets, years = years,
      cpar = cpar, ESS = ESS, ...),
      conditional_data)
    dat_list$N_agecomp <- nrow(dat_list$agecomp)

    ## Write the modified file
    if (!is.null(outfile)){
      r4ss::SS_writedat(datlist = dat_list, outfile = outfile, overwrite = TRUE,
                  version = get_ss_ver_dl(dat_list),
                  verbose = FALSE)
    }
    invisible(dat_list)
}
