#' Sample the biomass with observation error
#'
#' This function creates an index of abundance sampled from the expected
#' available biomass for given fleets in given years. Let \code{B_y} be the biomass
#' from the operating model for year y. Then the sampled value is calculated as:
#' \code{B_y*exp(rnorm(1, 0, sds_obs)-sds_obs^2/2)}. The second term
#' adjusts the random samples so that their expected value is \code{B_y} (i.e. the
#' log-normal bias correction).
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{index}. A suggested (default) case letter is \code{D} for data.
#'
#' @template lcomp-agecomp-index
#' @template dat_list
#' @template outfile
#' @param sds_obs *A list the same length as \code{fleets}. The list should
#'   contain either single values or numeric vectors of the same length as the
#'   number of years which represent the standard deviation of the observation
#'   error. Single values are repeated for all years.
#' @param make_plot A logical switch for whether to make a crude plot showing
#'   the results. Useful for testing and exploring the function.
#'
#' @template sampling-return
#' @template casefile-footnote
#' @importFrom r4ss SS_writedat
#'
#' @export
#' @author Cole Monnahan, Kotaro Ono
#' @examples \donttest{
#' # Find the example data location:
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- file.path(d, "example-om", "data.ss_new")
#' dat_list <- r4ss::SS_readdat(f_in, section = 2, version = NULL,
#'                              verbose = FALSE)
#' ex1 <- sample_index(dat_list, outfile = NULL, fleets=c(2,3),
#'                     years=list(1938:2012, 1938:2012) ,
#'                     sds_obs=list(1e-6, 1e-6),
#'                     make_plot = TRUE)
#' ex2 <- sample_index(dat_list, outfile = NULL, fleets=c(2,3),
#'                     years=list(1938:2012, 1938:2012) ,
#'                     sds_obs=list(.05, .05),
#'                     make_plot = TRUE)
#' library(ggplot2)
#' ggplot(ex1$CPUE, aes(x=year, y=obs, group=index, ymin=0,
#'   colour = as.factor(index))) +
#'   geom_line() +
#'   geom_point(data=ex2$CPUE,
#'     aes(x=year, y=obs, colour=as.factor(index), group=index))
#' ## Exclude a fleet and have varying sds_obs by year
#' ex3 <- sample_index(dat_list, outfile = NULL, fleets=c(2),
#'                     years=list(1938:2012),
#'                     sds_obs=list(seq(.001, .1, len=75)))
#' ggplot(ex3$CPUE, aes(x=year, y=obs, group=index, ymin=0,
#'                 colour=as.factor(index)))+geom_point()
#' }
#' @family sampling functions

sample_index <- function(dat_list, outfile = NULL, fleets, years, sds_obs,
                         make_plot = FALSE) {
  check_data(dat_list)
  cpue <- dat_list$CPUE # CPUE expected values.
  ## Check inputs for errors
  Nfleets <- length(fleets)
  if(FALSE %in% (fleets %in% unique(cpue$index))) {
    stop("The specified fleet numbers do not match input file")
  }
  if(Nfleets!= 0 & class(sds_obs) != "list" | length(sds_obs) != Nfleets) {
    stop("sds_obs needs to be a list of same length as fleets")
  }
  if(Nfleets!= 0 & class(years) != "list" | length(years) != Nfleets) {
    stop("years needs to be a list of same length as fleets")
  }
  for(i in 1:Nfleets) {
    if(length(sds_obs[[i]]) > 1 & length(sds_obs[[i]]) != length(years[[i]])) {
      stop("Length of sds_obs does not match length of years for fleet ",
           fleets[i])
    }
  }
  ## End input checks

  ## Start of sampling from the indices.  The general approach
  ## here is to loop through each row to keep (depends on years
  ## input) and resample depending on sds_obs All these rows are
  ## then combined back together to form the final CPUE.
  newcpue.list <- list()
  k <- 1
  if (Nfleets!= 0) {
    for(i in 1:Nfleets) {
      fl <- fleets[i]
      ## If only one sds given, extend it for all years
      if(length(sds_obs[[i]]) == 1) {
        sds_obs[[i]] <- rep(sds_obs[[i]], length.out = length(years[[i]]))
      }
      if(!is.na(fl)) {
        cpue.fl <- cpue[cpue$index == fl & cpue$year %in% years[[i]], ]
        if(length(years[[i]]) != nrow(cpue.fl)) {
          stop("A year specified in years was not found in the input ",
               "file for fleet", fl)
        }
        ## Now loop through each year and resample that row
        for(yr in years[[i]]) {
          xx <- cpue.fl[cpue.fl$year == yr, ]
          if(nrow(xx) == 1) {
            ## Sample from this year and fleet and recombine
            ## with the original data
            sds.new <- sds_obs[[i]][which(yr == years[[i]])]
            newcpue.df <- xx
            newcpue.df[1,4] <- xx$obs * exp(rnorm(n = 1, mean = 0, sd = sds.new) - sds.new ^ 2/2)
            newcpue.df[1,5] <- sds.new
            newcpue.list[[k]] <- newcpue.df
            k <- k + 1
          } else {
            stop(nrow(xx), " rows found for fleet ", fl, " in year ", yr,
                 " when should be 1")
          }
        }
      }
    }
  }
  ## Bind all the rows together to form the new index
  if(Nfleets > 0) cpue.new <- do.call(rbind, newcpue.list)
  if(Nfleets == 0) cpue.new <- data.frame("#")

  ## Crude plots:
  if(make_plot & Nfleets > 0) {
    plot(cpue.new$year, cpue.new$obs, ylim = c(0, max(cpue.new$obs) * 1.05),
        type = "p", xlab = "Year", ylab = "Observed", pch = cpue.new$index)
    legend("bottomright", pch = unique(cpue.new$index),
           legend = unique(cpue.new$index), title = "Index", bty = "n")
  }

  ## Open the .dat file for the assessment model and find the right lines to
  ## overwrite
  newfile <- dat_list
  newfile$CPUE <- cpue.new
  if(Nfleets > 0) newfile$N_cpue <- nrow(cpue.new)
  if(Nfleets == 0 ) newfile$N_cpue <- 0

  if (!is.null(outfile)) {
    SS_writedat(datlist = newfile, outfile = outfile, overwrite = TRUE,
                version = newfile$ReadVersion, verbose = FALSE)
  }

  invisible(newfile)
}
