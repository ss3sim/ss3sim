#' Sample the biomass with observation error to generate indices of abundance.
#'
#' This function creates an index of abundance sampled from the expected
#' available biomass for given fleets in given years. Let B_y be the biomass
#' from the OM. Then the sampled value is calculated as: B_y*exp(rnorm(1, 0,
#' \code{sds_obs})-\code{sds_obs}^2/2). The second term adjusts the random
#' samples so that their expected value is B_y (i.e. the log-normal bias correction).
#'
#' @param infile SS data object as read in from \code{SS_readdat} in the r4ss
#' package. Make sure you select option \code{section=2}.
#' @param outfile Character string of the name for the new file to be
#' created. Must end in \code{.dat}.
#' @param fleets Numeric vector giving the fleets to be used. This order also
#' pertains to other arguments. A value of \code{NA} or missing value excludes that
#' fleet from outfile (i.e. turn it off so no samples are written).
#' @param years A Numeric list the same length as \code{fleets} giving
#' the years.
#' @param sds_obs A Numeric list of the same length as \code{fleets}. Either
#' single values or vectors the same length as the number of years can be
#' passed. Single values are repeated for all years.
#' @param make_plot A switch for whether to make a crude ggplot showing the
#' results. Useful for testing and exploring the function.
#' @param write_file A switch for whether to write \code{outfile} to disk. Can
#' be turned off to speed up testing or exploration of the function (the new
#' indices are returned invisibly, as in the examples below)
#' @export
#' @author Cole Monnahan, Kotaro Ono
#' @examples \dontrun{
#' # Find the example data location:
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/example-om/data.ss_new")
#' infile <- r4ss::SS_readdat(f_in, section = 2, verbose = FALSE)
#' outfile <- "test.dat"
#' ex1 <- change_index(infile, outfile, fleets=c(2,3),
#'                     years=list(1938:2012, 1938:2012) ,
#'                     sds_obs=list(1e-6, 1e-6), write_file=FALSE,
#'                     make_plot = TRUE)
#' ex2 <- change_index(infile, outfile, fleets=c(2,3),
#'                     years=list(1938:2012, 1938:2012) ,
#'                     sds_obs=list(.05, .05), write_file=FALSE,
#'                     make_plot = TRUE)
#' library(ggplot2)
#' ggplot(ex1, aes(x=year, y=obs, group=index, ymin=0,
#'                 colour=as.factor(index)))+geom_line() + geom_point(data=ex2,
#'                 aes(x=year, y=obs, colour=as.factor(index), group=index))
#' ## Exclude a fleet and have varying sds_obs by year
#' ex3 <- change_index(infile, outfile, fleets=c(2,NA),
#'                     years=list(1938:2012, 1950),
#'                     sds_obs=list(seq(.001, .1, len=75), .1),
#'                     write_file=FALSE)
#' ggplot(ex3, aes(x=year, y=obs, group=index, ymin=0,
#'                 colour=as.factor(index)))+geom_point()
#' }

change_index <- function(infile, outfile, fleets, years, sds_obs,
                         make_plot = FALSE, write_file=TRUE){
    cpue <- infile$CPUE
    ## Check inputs for errors
    if(substr_r(outfile,4) != ".dat" & write_file)
        stop(paste0("outfile ", outfile, " needs to end in .dat"))
    Nfleets <- length(fleets)
    if(length(unique(cpue$index)) != Nfleets)
        stop(paste0("Number of fleets specified (",Nfleets,
                    ") does not match input file (",
                    length(unique(cpue$index)), ")"))
    if(FALSE %in% (fleets[!is.na(fleets)] %in% unique(cpue$index)))
        stop(paste0("The specified fleet number specified does not match input file"))
    if(class(sds_obs) != "list" | length(sds_obs) != Nfleets)
        stop("sds_obs needs to be a list of same length as fleets")
    if(class(years) != "list" | length(years) != Nfleets)
        stop("years needs to be a list of same length as fleets")
    for(i in 1:length(fleets)){
        if(length(sds_obs[[i]])>1 & length(sds_obs[[i]]) != length(years[[i]]))
            stop(paste0("Length of sds_obs does not match length of years for fleet ",fleets[i]))
    }
    ## End input checks

    ## Start of sampling from the indices.  The general approach
    ## here is to loop through each row to keep (depends on years
    ## input) and resample depending on sds_obs All these rows are
    ## then combined back together to form the final CPUE.
    newcpue.list <- list()
    k <- 1
    for(i in 1:Nfleets){
        fl <- fleets[i]
        ## If only one sds given, extend it for all years
        if(length(sds_obs[[i]])==1) sds_obs[[i]] <- rep(sds_obs[[i]], len=length(years[[i]]))
        if(!is.na(fl)){
            cpue.fl <- subset(cpue, index==fl & year %in% years[[i]])
            if(length(years[[i]]) != nrow(cpue.fl))
                stop(paste("A year specified in years was not found in the input file for fleet", fl))
            cpue.fl$sds_obs<- sds_obs[[i]]
            ## Now loop through each year and resample that row
            for(yr in years[[i]]) {
                xx <- subset(cpue.fl, year==yr)
                sds.new <- sds_obs[[i]][which(yr == years[[i]])]
                if(nrow(xx)==1){
                    ## Sample from this year and fleet and recombine
                    ## with the original data
                    newcpue <- xx$obs*exp(rnorm(n=1, mean=0,
                                                sd=sds.new)-sds.new^2/2)
                    newcpue.df <- xx
                    newcpue.df$obs <- newcpue
                    newcpue.df$sds_obs <- sds.new
                    newcpue.list[[k]] <- newcpue.df
                    k <- k+1
                } else {
                    stop(paste0(nrow(xx), " rows found for fleet ", fl,
                                " in year ", yr, " when should be 1"))
                }
            }
        }
    }
    ## Bind all the rows together to form the new index
    cpue.new <- do.call(rbind, newcpue.list)
    if(make_plot) {
      p <- ggplot2::ggplot(cpue.new, ggplot2::aes(x=year, y=obs, ymin=0,
        colour=as.factor(index)))+ggplot2::geom_line()
      print(p)
    }
    ## Just for testing purposes. Create crude plots.

    ## Open the .dat file for the assessment model and find the right lines to
    ## overwrite
    newfile <- infile
    newfile$CPUE <- cpue.new
    if(write_file)
        r4ss::SS_writedat(datlist = newfile, outfile = outfile, overwrite = T)
    return(invisible(cpue.new))
}
