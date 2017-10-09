#' @title Sample length compositions from expected values.
#'
#' @description Take a \code{data.SS_new} file containing expected values and sample to
#' create observed length compositions which are then written to file for use by
#' the estimation model. If used with \code{\link{run_ss3sim}} the case file
#' should be named \code{lcomp}. A suggested (default) case letter is \code{D}
#' for data.
#'
#' @author Gwladys Lambert; modified from a version by Cole Monnahan and Kotaro
#'   Ono
#'
#' @template lcomp-agecomp-index
#' @param cpar A numeric value or vector the same length as \code{fleets}
#'   controlling the variance of the Dirichlet distribution used for sampling. A
#'   value of 1 indicates the same standard deviation as a multinomial of the
#'   given Nsamp, 2 indicates twice, etc. Values greater than one indicate
#'   overdispersion, and less underdispersion. This is not compatible with the
#'   option to sample in schools, see \code{Nhauls}.
#' @template dat_list
#' @param Nsamp A numeric list of the same length as fleets. Either single
#'   values or vectors of the same length as the number of years can be passed
#'   through. Single values are repeated for all years. If no fleet collected
#'   samples, keep the value to Nsamp=NULL. When using the option to sample
#'   lengths from fish schools (see the \code{Nhaul} argument), \code{Nsamp} is
#'   the number of length samples to be taken per haul.
#' @param Nhauls A numeric list of the same length as fleets. Number of hauls
#'   to sample lengths data from. Default is NULL.
#'   Use only to activate the option to sample lengths data from fish schools,
#'   i.e., from aggregated data. It will call the \code{\link{sample_schools}}
#'   function.
#' @param empirical_aggs empirical data to create aggregation pattern, must include
#' 2 columns only, first mean lengths, then correponding SDs of hauls.
#' \code{\link{schooling_pattern}} is called to fit a gam model and resample standard
#' deviations from a new vector of mean lengths. Here this vector will be the empirical
#' one if \code{cl_factor} is NULL, else see \code{cl_factor}.
#' @param cl_factor this parameter determines the characteristics of the
#'  clusters (or fish schools) that form the expected distribution (combined by
#'  mixture distribution) year by year. \code{cl_factor} sets the maximum number of
#'  clusters that can be used to fit the expected length distribution which defines
#'  the mean length of these clusters. Used in combination with \code{empirical_aggs},
#'  it will overwrite the empirical vector of mean lengths. Each cluster is the
#'  gamma equivalent of a  normal distribution (gamma equivalent is used to avoid
#'  drawing negative length values). The number of clusters is defined by
#'    \code{cl_factor}*(max(\code{vec})-min(\code{vec})).
#'    A value of \code{cl_factor}=1 means that the number of clusters will be equal
#'    to the range of lengths, i.e. the mean of each distribution will be 1 cm apart.
#'    A value of 0.5 means that the number of clusters will be divided by 2, i.e.
#'    mean length of each distribution every 2cm etc.
#' @param vals_at_cutoff Value used to estimate the SD of the clusters (or fish
#'   schools) if \code{cl_factor} is derfined and \code{empirical_aggs} is not used.
#'   A value of 0.2 means that the SD of the normal distributions will
#'   be 20 percent of the full range of lengths.
#' @param ESS Define effective sample size for lengths data. Has to be a list of
#'   the same length as the number of fleets. Can be a single value or a vector
#'   of length equal to the number of years. Can also be a character, i.e.
#'   "hauls", in which case the number of hauls sampled from will be used as
#'   ESS, if using the option to sample in schools.
#' @param change_ess_now If ESS has to be changed at this stage or by the end of
#'   the sampling process in \code{\link{ss3sim_base}}. Default is NULL, meaning
#'   that it will be changed here (would do the same with TRUE). Set to FALSE to
#'   keep the original sample size until the end of the sampling process in
#'   \code{\link{ss3sim_base}}. This is important when sampling conditional
#'   age-at-length, using \code{\link{sample_calcomp}}, where the number of ages
#'   that can be sampled depends on the original number of lengths measured.
#' @param plot.schools Default is FALSE. Set to TRUE to visualise the schools
#'   fitting process and sampling outputs when the \code{Nhauls} argument is not
#'   set to NULL.
#' @param parallel_lgths If sampling several years, option to conduct the
#'   sampling in parallel (useful when using the schooling options). Default is
#'   FALSE. Note that the \code{plot.schools} option will not work in parallel.
#' @param Nsamp_max Maximum number of lengths measurements to take if
#'   numbers-at-length are randomly generated in the case file. Default is NULL.
#' @param random_gen If \code{years} are randomly generated, give the fleet
#'   number(s) for which they are. Default is NULL.
#' @template casefile-footnote
#' @template sampling-return
#' @return If the schooling option is turned on it will return a list of 2
#'   objects. The first one is the \code{.dat} file, the second one is a
#'   dataframe detailing the actual sampling, i.e. lengths measurements on a
#'   haul by haul basis.
#' @importFrom r4ss SS_writedat
#' @importFrom foreach foreach %dopar%
#' @importFrom graphics points
#' @importFrom reshape melt
#' @importFrom grDevices graphics.off
#' @export
#' @family sampling functions
#'
#' @examples
#'  d <- system.file("extdata", package = "ss3sim")
#'  f_in <- paste0(d, "/models/cod-om/codOM.dat")
#'  dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#'  dat_list <- change_fltname(dat_list)
#'
#'  ## Generate with constant sample size across years
#'  ex1 <- sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(1,2),
#'                      Nsamp=list(100,50), years=list(seq(26, 100, by=2),
#'                                              80:100), write_file = FALSE)
#'
#'  ## Generate with varying Nsamp by year for first fleet
#'  ex2 <- sample_lcomp(dat_list=dat_list, outfile="test2.dat", fleets=c(1,2),
#'                      Nsamp=list(c(rep(50, 5), rep(100, 5)), 50),
#'                      years=list(seq(26, 44, by=2),
#'                          80:100), write_file = FALSE)
#' \dontrun{
#'  # Plot distributions for a particular year to compare multinomial
#'  # vs. overdispersed Dirichlet
#'  temp.list <- temp.list2 <- list()
#'  for(i in 1:40){
#'      temp.list[[i]] <-
#'        sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(2),
#'                       cpar=c(3), years=list(95),
#'                       Nsamp=list(100), write_file=FALSE)$lencomp
#'      temp.list2[[i]] <-
#'          sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(2),
#'                       cpar=c(NA), Nsamp=list(100), years=list(95),
#'                       write_file=FALSE)$lencomp
#'  }
#'  ## Organize the data for plotting
#'  x1 <- reshape2::melt(do.call(rbind, temp.list)[,-(1:6)[-3]], id.vars="FltSvy")
#'  x2 <- reshape2::melt(do.call(rbind, temp.list2)[,-(1:6)[-3]], id.vars="FltSvy")
#'  Make proportional data
#'  x2 <- reshape2::melt(do.call(rbind, tapply(x2$value, list(x2$FltSvy, x2$variable), prop.table)))
#'  op <- par(mfrow=c(2,1))
#'  with(x1, boxplot(value~variable, las=2, ylim=c(0,.6), ylab="Proportion",
#'                   main="Overdispersed (cpar=3)",  xlab="length bin"))
#'  temp <- as.numeric(subset(dat_list$lencomp, Yr==95 & FltSvy == 2)[-(1:6)])
#'  points(temp/sum(temp), pch="-", col="red")
#'  with(x2, boxplot(value~Var1, las=2, ylab="Proportion",
#'                   main="Multinomial", xlab="length bin", xaxt = "n"))
#'  axis(side = 1, at = unique(x2$Var1), labels = unique(x1$variable), las = 2)
#'  temp <- as.numeric(subset(dat_list$lencomp, Yr==95 & FltSvy == 2)[-(1:6)])
#'  points(temp/sum(temp), pch="-", col="red")
#'  par(op)
#'
#'
#'  ## Option to sample in schools (length aggregated fish)
#'
#'  # Generate with constant sample size across years
#'  # If plot.schools = T, need to set the directory where the plot pdf will be
#'  # saved and can be accessed and visualised
#'  getwd()
#'  ex1 <- sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(1,2),
#'                      Nsamp=list(100,50), years=list(seq(96, 100, by=2),
#'                      98:100), Nhauls= list(100,100), cl_factor = 0.5,
#'                      vals_at_cutoff = 0.1, plot.schools = T, write_file = F)
#'  # dat file
#'  ex1a <- ex1[[1]]
#'  # hauls file
#'  ex1b <- ex1[[2]]
#'
#'  # Generate with varying Nsamp by year for first fleet
#'  ex2 <- sample_lcomp(dat_list=dat_list, outfile="test2.dat", fleets=c(1,2),
#'                      Nsamp=list(c(80,90,100), 50),
#'                      years=list(seq(40, 44, by=2), 98:100),
#'                      Nhauls= list(100,100), cl_factor = 0.5, vals_at_cutoff
#'                      = 0.1, plot.schools = TRUE,  write_file = FALSE)
#'  # dat file
#'  ex2a <- ex2[[1]]
#' hauls file
#'  ex2b <- ex2[[2]]
#'
#'  # Testing the parallel version - runs years in parallel (not useful if
#'  # running one year at a time)
#'
#'  library(doParallel)
#'  library(compiler)
#'  cores=7
#'  assign(".lib.loc", .libPaths()[1], envir = environment(.libPaths))
#'  cl<-makeCluster(cores)
#'  clusterCall(cl, function(x) .libPaths(x), .libPaths())
#'  registerDoParallel(cl)
#'
#'  ex3 <- sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(1,2),
#'                               Nsamp=list(100,50), years=list(seq(96, 100,
#'                               by=2), 98:100), Nhauls= list(100,100),
#'                               cl_factor = 0.5, vals_at_cutoff = 0.1,
#'                               plot.schools = TRUE,  write_file = FALSE,
#'                               parallel_lgths = T)
#'
#'
#'  # Compare multinomial and dirichlet to schools sampling
#'  temp.list <- temp.list2 <- temp.list3 <- list()
#'  for(i in 1:10){
#'    temp.list[[i]] <-
#'      sample_lcomp(dat_list=dat_list, outfile="test1.dat", fleets=c(2), cpar=c(3),
#'                   Nsamp=list(100), years=list(95),
#'                   write_file=FALSE)$lencomp
#'    temp.list2[[i]] <-
#'      sample_lcomp(dat_list=dat_list, outfile="test2.dat", fleets=c(2), cpar=c(NA),
#'                   Nsamp=list(100), years=list(95),
#'                   write_file=FALSE)$lencomp
#'    temp.list3[[i]] <-
#'      sample_lcomp(dat_list=dat_list, outfile="test3.dat", fleets=c(2), cpar=c(NA),
#'                   Nhauls= list(10),  Nsamp=list(10), years=list(95),
#'                   cl_factor = 0.5, vals_at_cutoff = 0.1,
#'                   plot.schools = F, write_file=FALSE)[[1]]$lencomp
#'  # Note that the output of sample_lcomp is now a list of length 2.
#'  # [[1]] is the data file for SS, [[2]] is the actual samples, haul by haul
#'  }
#'  # Organize the data for plotting
#'  x1 <- reshape2::melt(do.call(rbind, temp.list)[,-(1:6)[-3]], id.vars="FltSvy")
#'  x2 <- reshape2::melt(do.call(rbind, temp.list2)[,-(1:6)[-3]], id.vars="FltSvy")
#'  x2$value <- x2$value/100
#'  x3 <- reshape2::melt(do.call(rbind, temp.list3)[,-(1:6)[-3]], id.vars="FltSvy")
#'  op <- par(mfrow=c(3,1))
#'  par(op)
#'  with(x1, boxplot(value~variable, las=2, ylab="Proportion",
#'                   main="Overdispersed (cpar=3)", ylim=c(0,.06), xlab="length bin"))
#'  with(x2, boxplot(value~variable, las=2, ylab="Proportion", ylim=c(0,.06),
#'                   main="Multinomial",  xlab="length bin"))#ylim=c(0,.6),
#'  with(x3, boxplot(value~variable, las=2, ylab="Proportion",
#'                   main="Schools", ylim=c(0,.06),  xlab="length bin"))#ylim=c(0,.6),
#'
#'
#'  }
#'

sample_lcomp <- function(dat_list, outfile, fleets = c(1,2), years, cpar = 1,
                         Nsamp, Nhauls=NULL, cl_factor=NULL,
                         vals_at_cutoff=0.1,
                         ESS=NULL, change_ess_now=NULL,
                         plot.schools = F, parallel_lgths = F,
                         Nsamp_max=NULL, random_gen=NULL,
                         empirical_aggs = NULL,
                         write_file = TRUE) {


  ## To satisfy R CMD checks
  x <- NULL

  check_data(dat_list)
  lcomp <- dat_list$lencomp

  ## Temporary option - if using the sampling-in-schools option and randomly generating the number samples and hauls, Nsamp_max can put a maximum limit to the number of samples taken per haul
  if (!is.null(Nhauls)){
    if (!is.null(Nsamp_max)){
      for (idx_temp in 1:length(Nsamp_max)) {
        Nsamp[[idx_temp]] <- ifelse(Nsamp[[idx_temp]]*Nhauls[[idx_temp]]<=Nsamp_max[[idx_temp]],Nsamp[[idx_temp]],
                                    floor(Nsamp_max[[idx_temp]]/Nhauls[[idx_temp]]))
      }
    }
  }

  ## Check inputs for errors
  Nfleets <- ifelse(is.null(fleets), 0, length(fleets))
  ## If not provided, use the sample size (true ESS only for multinomial case).
  if(is.null(ESS)) {
    ESS <- Nsamp
    useESS <- FALSE
  } else {
    if (is.null(change_ess_now)) {
      useESS <- TRUE
    } else if (!change_ess_now) {
      useESS <- FALSE
    }
  }

  if(FALSE %in% (fleets %in% unique(lcomp$FltSvy)))
    stop(paste0("The specified fleet number does not match input file"))
  if(Nfleets!= 0 & class(Nsamp) != "list" | length(Nsamp) != Nfleets)
    stop("Nsamp needs to be a list of same length as fleets")
  if(Nfleets!= 0 & class(ESS) != "list" | length(ESS) != Nfleets)
    stop("ESS needs to be a list of same length as fleets")
  if(Nfleets!= 0 & class(years) != "list" | length(years) != Nfleets)
    stop("years needs to be a list of same length as fleets")
  if (Nfleets>0){
    for(i in 1:Nfleets){
      if(length(Nsamp[[i]])>1 & length(Nsamp[[i]]) != length(years[[i]]))
        stop(paste0("Length of Nsamp does not match length of years for fleet ",
                    fleets[i]))
      if(length(ESS[[i]])>1 & length(ESS[[i]]) != length(years[[i]]))
        stop(paste0("Length of ESS does not match length of years for fleet ",
                    fleets[i]))
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

  # run schooling pattern to calculate shape and rate of gamma distribution if shooling option is to be used
  dat_gamma <- NULL
  if (!is.null(empirical_aggs))
  {
    if (!is.null(cl_factor)) {
    fac <- cl_factor*(max(dat_list$lbin_vector)-min(dat_list$lbin_vector))
    new_lengths <- seq(min(dat_list$lbin_vector),max(dat_list$lbin_vector), length=fac)
    } else {
      new_lengths <- sort(empirical_aggs[[1]])
    }
    dat_gamma <- schooling_pattern(empirical_aggs[[1]], empirical_aggs[[2]], new_lengths, plot_fit=F)
  }

  # Back to script

  hauls_res <- NULL

  ## Resample from the length data
  ## The general approach here is to loop through each row to keep
  ## (depends on years input) and resample depending on Nsamp and
  ## cvar. All these rows are then combined back together to form
  ## the final lcomp.
  newcomp.list <- list() # temp storlength for the new rows
  # kk <- 1
  ## Loop through each fleet
  if (Nfleets>0){
    for(ifl in 1:length(fleets)){
      fl <- fleets[[ifl]]
      if(!is.na(fl)){
        lcomp.fl <- lcomp[lcomp$FltSvy == fl & lcomp$Yr %in% years[[ifl]], ]
        if(length(years[[ifl]]) != nrow(lcomp.fl))
          stop(paste("A year specified in years was not found in the input",
                     "file for fleet", fl))
        ## Hack way to get ESS to work without restructuring the
        ## whole function. Need to be able to index it by year
        ## below
        if(length(ESS[[ifl]])==1 & !any(ESS== "hauls"))
          ESS[[ifl]] <- rep(ESS[[ifl]], times=length(years[[ifl]]))
        lcomp.fl$Nsamp <- Nsamp[[ifl]]

        ## Now loop through each year and resample that row
        ## GL - I turned this into a function so that I can use the parallel option useful when sampling in schools multiple years of data


        get_lengths <- function(x=1, ifl_temp=ifl, dat_list_temp =dat_list, lcomp.fl_temp=lcomp.fl, years_temp=years, Nhauls_temp=Nhauls, Nsamp_temp=Nsamp,
                                cpar_temp=cpar, cl_factor_temp=cl_factor, vals_at_cutoff_temp=vals_at_cutoff, dat_gamma_temp=dat_gamma,
                                plot.schools_temp = plot.schools, useESS_temp=useESS, ESS_temp=ESS, parallel_temp= parallel_lgths) {  # cutoff_temp=cutoff,


          # browser()

          hauls <- NULL
          yr = years_temp[[ifl]][x]
          newcomp <- lcomp.fl_temp[lcomp.fl_temp$Yr==yr, ]

          ## Replace expected values with sampled values
          ## First 1-9 cols aren't length bins so skip them
          probs <- as.numeric(newcomp[-(1:6)]/sum(newcomp[-(1:6)]))
          ## If cpar is NA this signifies to use the multinomial
          if (is.null(Nhauls_temp)) {
            if(is.na(cpar_temp[ifl])){
              newcomp[-(1:6)] <-
                rmultinom(1, size=newcomp$Nsamp, prob=probs)#/newcomp$Nsamp
            } else { # use Dirichlet
              lambda <- newcomp$Nsamp/cpar_temp[ifl]^2 - 1
              if(lambda<0)
                stop(paste("Invalid Dirichlet parameter: Lambda=", lambda))
              newcomp[-(1:6)] <- gtools::rdirichlet(1,probs * lambda)
              ## Use the effective sample size when using Dirichlet
              effectiveN <- newcomp$Nsamp/cpar[ifl]^2
              newcomp$Nsamp <- effectiveN
            }
            # Now if we use the sample_schools option, i.e. Nhauls !=NULL...
          } else {

            vec <- dat_list_temp$lbin_vector
            flt_idx <- ifl_temp

            return_hauls <- sample_schools(probs = probs, Nsamp= Nsamp_temp[[ifl]], Nhauls = Nhauls_temp[[ifl]], vec = vec,
                                           cl_factor=cl_factor_temp,  vals_at_cutoff= vals_at_cutoff_temp, #cutoff= cutoff_temp,
                                           gamma_sh = dat_gamma$shape, gamma_ra = dat_gamma$rate,
                                           plot.schools = plot.schools_temp, yr = yr)
            newcomp[,-c(1:6)] <- unlist(return_hauls[[1]])
            ifl_temp <- flt_idx
            yr.ind <- which(years_temp[[ifl_temp]]==yr)
            hauls <- return_hauls[[2]]
            newcomp$Nsamp <- sum(hauls$Nb)
            hauls <- suppressWarnings(cbind.data.frame(newcomp[,c(1:6)],hauls))
          }

          ## newcomp will have the true ESS up until here. So
          ## replace with supplied ESS if used
          if(useESS_temp) {
            if (ESS_temp[[ifl]] != "hauls") {
              newcomp$Nsamp <- ESS_temp[[ifl]][which(years_temp[[ifl]]==yr)]
            } else {
              newcomp$Nsamp <- Nhauls_temp[[ifl]]
            }
          }
          invisible(return(list(newcomp, hauls)))# ################################## TEMP FIX!!!!!
        }

        if (parallel_lgths) {
        cores <- setup_parallel()
        if(cores == 1) parallel_lgths <- FALSE
        }

        if (plot.schools)  {
          pdf(sprintf('plots_%03s.pdf', paste("fleet=", fl, sep="")))
        }

        if (parallel_lgths) {


          message(paste("Running length sampling in schools in parallel. Fleet ", ifl, sep=""))
      #    if (plot.schools) pdf(file = sprintf("plots_%03s.pdf", paste("fleet=", ifl, sep="")), onefile = T)

          newcomp.list.temp <- foreach::foreach(x = as.list(1:length(years[[ifl]])), .packages = c("ss3sim","parallel","doParallel","Hmisc","reshape"), #.libPaths(.libPaths()[1]),
                                                .verbose = FALSE, .export=c("get_lengths","sample_schools")) %dopar% {
                                                 do.call("get_lengths", list(x, ifl_temp=ifl, dat_list_temp =dat_list, lcomp.fl_temp=lcomp.fl, years_temp=years, Nhauls_temp=Nhauls,
                                                                              Nsamp_temp=Nsamp, cpar_temp=cpar, cl_factor_temp=cl_factor, dat_gamma=dat_gamma,
                                                                              vals_at_cutoff_temp=vals_at_cutoff, plot.schools_temp = plot.schools, useESS_temp=useESS,
                                                                              ESS_temp=ESS, parallel_temp=parallel_lgths)) #cutoff_temp=cutoff,
                                                          }

          ## Combine new rows together into one data.frame
          if(Nfleets>0) {
            hauls_res_temp  <-  do.call(rbind, sapply(newcomp.list.temp,"[", 2))
            hauls_res       <- rbind(hauls_res, hauls_res_temp)
            newcomp.list.temp <-  do.call(rbind, sapply(newcomp.list.temp,"[", 1))
            newcomp.list    <- rbind(newcomp.list, newcomp.list.temp)
            newcomp.final   <- newcomp.list
          }

          if(Nfleets==0) newcomp.final = data.frame("#")

        } else {

        #  pdf(sprintf('plots_%03s.pdf', paste("fleet=", fl, sep="")))

          for (i in 1:length(years[[ifl]])){
            newcomp.list.temp <- get_lengths(x=i)
            newcomp.list <- rbind(newcomp.list, newcomp.list.temp[[1]])
            hauls_res <- rbind(hauls_res, newcomp.list.temp[[2]]) ################################################ TEMP FIX
          }

         # graphics.off()
          ## Combine new rows together into one data.frame
          if(Nfleets>0) newcomp.final <- newcomp.list
          if(Nfleets==0) newcomp.final = data.frame("#")

        }

        if (plot.schools) dev.off()

        }
    }
  }

  ## Build the new dat file
  newfile <- dat_list
  newfile$lencomp <- newcomp.final
  if(Nfleets>0) newfile$N_lencomp <- nrow(newcomp.final)
  if(Nfleets==0) newfile$N_lencomp <- 0

  ## Write the modified file
  if(write_file)
    SS_writedat(datlist = newfile, outfile = outfile, overwrite = TRUE,
                verbose = FALSE)
  if (is.null(Nhauls)) res <- newfile
  if (!is.null(Nhauls)) res <- list(newfile,hauls_res)
  invisible(return(res))
}

#' (Depreciated) Sample length compositions from expected values
#'
#' \code{change_lcomp} is a depreciated function. Please use
#' \code{\link{sample_lcomp}} instead. \code{change_lcomp} will be removed
#' in the next major version.
#'
#' @param ... Arguments that get passed to \code{\link{sample_lcomp}}.
#'
#' @export

change_lcomp <- function(...) {
  warning(paste("change_lcomp is a depreciated function.",
                "Please use sample_lcomp instead. change_lcomp will",
                "be removed in the next major version."))
  sample_lcomp(...)
}
