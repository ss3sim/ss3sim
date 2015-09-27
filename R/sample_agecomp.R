#' Sample age compositions from expected values
#'
#' Take a \code{data.SS_new} file containing expected values and sample to
#' create observed age compositions which are then written to file for use by
#' the estimation model.
#' If used with \code{\link{run_ss3sim}} the case file should be named
#' \code{agecomp}. A suggested (default) case letter is \code{D} for data.
#'
#' @author Cole Monnahan and Kotaro Ono; modified from a version by Roberto
#' Licandeo and Felipe Hurtado-Ferro
#'
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @template dat_list
#' @template Nsamp
#' @param keep_conditional A logical if conditional age-at-length data
#'   should be kept or removed entirely from the \code{.dat} file.
#'   \code{sample_agecomp} only works on the age composition data
#'   and not on the conditional age-at-length data. To sample the
#'   conditional data set \code{keep_conditional} to \code{TRUE}
#'   and use \code{\link{sample_calcomp}}.
#'
#' @template sampling-return
#' @template casefile-footnote
#' @importFrom r4ss SS_writedat
#'
#' @examples
#' d <- system.file("extdata", package = "ss3sim")
#' f_in <- paste0(d, "/models/cod-om/codOM.dat")
#' dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#' dat_list <- change_fltname(dat_list)
#'
#' ## Turn off age comps by specifying fleets=NULL
#' sample_agecomp(dat_list=dat_list, outfile="test1.dat",
#'                fleets=NULL, cpar=c(5,NA), Nsamp=list(100,100),
#'                years=list(1995, 1995), write_file=FALSE)
#'
#' ## Generate with a smaller number of fleet taking samples
#' ex1 <- sample_agecomp(dat_list=dat_list, outfile="test1.dat", fleets=c(2),
#'                       Nsamp=list(c(10,50)), years=list(c(26,27)),
#'                       write_file=FALSE)
#'
#' ## Generate with varying Nsamp by year for first fleet
#' ex2 <- sample_agecomp(dat_list=dat_list, outfile="test2.dat", fleets=c(1,2),
#'                       Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
#'                       years=list(seq(26, 44, 2),
#'                           c(26:100)), write_file=FALSE)
#'
#' \dontrun{
#' ## Run three  cases showing Multinomial, Dirichlet(1) and over-dispersed
#' ## Dirichlet for different levels of sample sizes
#' op <- par(mfrow = c(1,3))
#' for(samplesize in c(30, 100, 1000)){
#'     ex4 <- sample_agecomp(dat_list=dat_list, outfile="test4.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(NA, 1))
#'     ex5 <- sample_agecomp(dat_list=dat_list, outfile="test5.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(1, 1))
#'     ex6 <- sample_agecomp(dat_list=dat_list, outfile="test6.dat", fleets=c(1,2),
#'                           Nsamp=list(samplesize, samplesize),
#'                           write_file = FALSE,
#'                           years=list(2000,2000), cpar=c(5, 1))
#'     true <- subset(dat_list$agecomp, FltSvy==1 & Yr == 2000)[-(1:9)]
#'     true <- true/sum(true)
#'     plot(0:15, subset(ex4, FltSvy==1)[1,-(1:9)], type="b", ylim=c(0,1),
#'          col=1, xlab="Age", ylab="Proportion", main=paste("Sample size=",
#'          samplesize))
#'     legend("topright", legend=c("Multinomial", "Dirichlet(1)",
#'                                 "Dirichlet(5)", "Truth"),
#'            lty=1, col=1:4)
#'     lines((0:15), subset(ex5, FltSvy==1)[1,-(1:9)], type="b", col=2)
#'     lines((0:15), subset(ex6, FltSvy==1)[1,-(1:9)], type="b", col=3)
#'     lines((0:15), true, col=4, lwd=2)
#' }
#' par(op)
#' }
#' @family sampling functions
#' @export
sample_agecomp <- function(dat_list, outfile, fleets = c(1,2), Nsamp,
                           years, cpar=1, ESS=NULL, write_file=TRUE,
                           keep_conditional = TRUE){
    ## The new agecomp is mostly based on the old one so start with
    ## that
    check_data(dat_list)
    agecomp <- dat_list$agecomp

    ## Check inputs for errors
    Nfleets <- ifelse(is.null(fleets), 0, length(fleets))
    ## If not provided, use the sample size (true ESS except for Dirichlet case).
    if(is.null(ESS)) {
        ESS <- Nsamp
        useESS <- FALSE
    } else {
        useESS <- TRUE
    }
    if(Nfleets >0 & FALSE %in% (fleets %in% unique(agecomp$FltSvy)))
        stop(paste0("The specified fleet number does not match input file"))
    if(Nfleets!= 0 & (class(Nsamp) != "list" | length(Nsamp) != Nfleets))
        stop("Nsamp needs to be a list of same length as fleets")
    if(Nfleets!= 0 & (class(ESS) != "list" | length(ESS) != Nfleets))
        stop("ESS needs to be a list of same length as fleets")
    if(Nfleets!= 0 & (class(years) != "list" | length(years) != Nfleets))
        stop("years needs to be a list of same length as fleets")
    ## If no fleets are specified then skip these checks
    if (Nfleets>0){
        for(i in 1:Nfleets){
            if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
                stop(paste0("Length of Nsamp does not match length of years for ",
                  "fleet ",fleets[i]))
            if(length(ESS[[i]])>1 & length(ESS[[i]]) != length(years[[i]]))
                stop(paste0("Length of ESS does not match length of years for ",
                  "fleet ",fleets[i]))
        }
        if(length(cpar) == 1){
            ## If only 1 value provided, use it for all fleets
            cpar <- rep(cpar, times=Nfleets)
        } else if(length(cpar) != Nfleets){
            stop(paste0("Length of cpar (", length(cpar),
                        ") needs to be length of fleets (", Nfleets,
                        ") or 1"))
        }
    }
    ## End input checks

    ## Split the conditional data from the age data
    conditional_data <- agecomp[agecomp$Lbin_lo >= 0, ]
    agecomp <- agecomp[agecomp$Lbin_lo < 0, ]

    ## Resample from the age data
    ## The general approach here is to loop through each row to keep
    ## (depends on years input) and resample depending on Nsamp and
    ## cvar. All these rows are then combined back together to form
    ## the final agecomp.
    newcomp.list <- list() # temp storage for the new rows
    k <- 1
    ## Loop through each fleet
    if (Nfleets>0){
        for(i in 1:length(fleets)){
            fl <- fleets[i]
            if(!is.na(fl)){
                agecomp.fl <- agecomp[agecomp$FltSvy == fl &
                    agecomp$Yr %in% years[[i]], ]
                if(length(years[[i]]) != nrow(agecomp.fl))
                    stop(paste("A year specified in years was not found in the",
                               "input file for fleet", fl))
                ## Hack way to get ESS to work without restructuring the
                ## whole function. Need to be able to index it by year
                ## below
                if(length(ESS[[i]])==1)
                    ESS[[i]] <- rep(ESS[[i]], times=length(years[[i]]))
                agecomp.fl$Nsamp <- Nsamp[[i]]
                ## Now loop through each year and resample that row
                for(yr in years[[i]]) {
                    newcomp <- agecomp.fl[agecomp.fl$Yr==yr, ]
                    ## Replace expected values with sampled values
                    ## First 1-9 cols aren't age bins so skip them
                    probs <- as.numeric(newcomp[-(1:9)]/sum(newcomp[-(1:9)]))
                    ## If cpar is NA this signifies to use the multinomial
                    if(is.na(cpar[i])){
                        newcomp[-(1:9)] <-
                            rmultinom(1, size=newcomp$Nsamp, prob=probs)
                    } else { # use Dirichlet
                        lambda <- newcomp$Nsamp/cpar[i]^2 - 1
                        if(lambda < 0)
                            stop(paste("Invalid Dirichlet parameter: Lambda=", lambda))
                        newcomp[-(1:9)] <- gtools::rdirichlet(1, probs * lambda)
                        ## use the effective sample size when using Dirichlet
                        effectiveN <- newcomp$Nsamp/cpar[i]^2
                        newcomp$Nsamp <- effectiveN
                    }
                    ## newcomp will have the true ESS up until here. So
                    ## replace with supplied ESS if used
                    if(useESS)
                        newcomp$Nsamp <- ESS[[i]][which(years[[i]]==yr)]
                    newcomp.list[[k]] <- newcomp
                    k <- k+1
                }
            }
        }
    }

    ## Combine back together into final data frame with the different data
    ## types
    newfile <- dat_list
    if(Nfleets > 0){
        newcomp.final <- do.call(rbind, newcomp.list)
        ## Case with both types
        if(nrow(conditional_data) > 0){
            newcomp.final <- rbind(newcomp.final, conditional_data)
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)

        } else { ## case with only age data
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)
             }
    } else {
        ## Case with only conditional data
        if(nrow(conditional_data) > 0){
            newcomp.final <- conditional_data
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- nrow(newcomp.final)

        } else {
            ## case with no data of either type
            newcomp.final <- data.frame("#")
            newfile$agecomp <- newcomp.final
            newfile$N_agecomp <- 0
        }
    }

    ## Write the modified file
    if(write_file)
        SS_writedat(datlist = newfile, outfile = outfile, overwrite = TRUE,
                    verbose = FALSE)
    invisible(newfile)
}


#' (Depreciated) Sample age compositions from expected values
#'
#' \code{change_agecomp} is a depreciated function. Please use
#' \code{\link{sample_agecomp}} instead. \code{change_agecomp} will be removed
#' in the next major version.
#'
#' @param ... Arguments that get passed to \code{\link{sample_agecomp}}.
#'
#' @export

change_agecomp <- function(...) {
  warning(paste("change_agecomp is a depreciated function.",
    "Please use sample_agecomp instead. change_agecomp will",
    "be removed in the next major version."))
  sample_agecomp(...)
}
