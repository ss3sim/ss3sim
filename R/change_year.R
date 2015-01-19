#' Methods to change the years in an ss3sim model
#'
#' \code{change_years} takes SS3 \code{.ctl}, \code{.dat},  
#' \code{.par}, \code{starter.ss}, and \code{.forecast} files
#' and changes the start and end year of the model.
#' \code{change_years} is set up to work with \code{ss3sim}
#' operating model or estimation model.
#'
#' @param year_begin Desired start year
#' @param year_end Desired end year
#' @param burnin Length of burnin period. Default is zero for an OM.
#' Use burnin for EM models, to establish a period with no fishing.
#' @param ctl_file_in Input SS3 control file
#' @param ctl_file_out Output SS3 control file, if NULL the file will
#' be named the same as the \code{ctl_file_in}
#' @param dat_file_in Input SS3 data file
#' @param dat_file_out Output SS3 data file, if NULL the file will
#' be named the same as the \code{dat_file_in}
#' @param par_file_in Input SS3 parameter file
#' @param par_file_out Output SS3 parameter file, if NULL the file will
#' be named the same as the \code{par_file_in}
#' @param starter_file_in Input SS3 starter file
#' @param starter_file_out Output SS3 starter file, if NULL the file will
#' be named the same as the \code{starter_file_in}
#' @param forecast_file_in Input SS3 forecast file
#' @param forecast_file_out Output SS3 forecast file, if NULL the file will
#' be named the same as the \code{forecast_file_in}
#' @author Kelli Johnson
#' @return The function creates modified versions of the \code{.par},
#'   \code{.dat} , \code{.ctl}, \code{.starter}, and \code{.forecast} files.
#'
#' @details
#' Operating models and estimation models will not have all of the same files,
#' thus if the file does not exist change the \code{_file_in} to \code{NULL}.
#' The code will add data for all years specified, minus the burnin period,
#' if the data type is present in the \code{.dat} file.
#' Manipulation done to the \code{.dat} file is not complete and users will need
#' to specify data for years which are deleted. The function removes all composition
#' data except for the first year. 
#' To remove data use \code{\link{sample_index}}, \code{\link{sample_lcomp}},
#' or \code{\link{sample_agecomp}}. 
#' For models that use the \code{.forecast} file, all references to years
#' must be made with relative values (i.e., 0 or negative integers).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a temporary folder for the output and set the working directory:
#' wd.old <- getwd()
#' temp_path <- file.path(tempdir(), "change_year-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' setwd(temp_path)
#'
#' # Find the SS3 "Simple" model in the package data:
#' d <- system.file("extdata", package = "ss3sim")
#' simple <- paste0(d, "/Simple")
#' dir.create("Simple")
#' file.copy(simple, ".", recursive = TRUE)
#' setwd("Simple")
#'
#' # Run SS3 to create control.ss_new and Report.sso:
#' system("SS3_24o_safe starter.ss -noest")
#'
#' change_year(year_begin = 1, year_end = 100, burnin = 25,
#'  ctl_file_in = "control.ss_new", ctl_file_out = "change_year.ctl",
#'  dat_file_in = "simple.dat", dat_file_out = "change_year.dat",
#'  par_file_in = "ss3.par", par_file_out = "change_year.par",
#'  starter_file_in = "starter.ss", starter_file_out = "change_year_starter.ss",
#'  forecast_file_in = "forecast.ss", forecast_file_out = "change_year_forecast.ss")
#'
#' # Clean up:
#' setwd("../")
#' unlink("Simple")
#' setwd(wd.old)
#' }

change_year <- function(year_begin = 1, year_end = 100, burnin = 0,
  ctl_file_in = NULL, ctl_file_out = "new.ctl",
  dat_file_in = NULL, dat_file_out = "new.dat",
  par_file_in = NULL, par_file_out = "new.ss",
  starter_file_in = NULL, starter_file_out = "starter.ss",
  forecast_file_in = NULL, forecast_file_out = "forecast.ss",
  verbose = FALSE) {
  
  if (is.null(ctl_file_out)) {
    ctl_file_out <- ctl_file_in
  }
  if (is.null(dat_file_out)) {
    dat_file_out <- dat_file_in
  }
  if (is.null(par_file_out)) {
    par_file_out <- par_file_in
  }
  if (is.null(starter_file_out)) {
    starter_file_out <- starter_file_in
  }
  if (is.null(forecast_file_out)) {
    forecast_file_out <- forecast_file_in
  }

  year_span <- year_end - year_begin + 1
  years.use <- year_begin:year_end
  if (burnin > 0) {
    if (length(years.use) <= burnin) {
      stop("Burnin period is greater than or equal to\n
           total number of years.")
    }
    years.use <- years.use[-c(1:burnin)]
  }

  # Function to change a line split of the structure "value # comment"
  manipulate <- function(file, search_for, new) {
    line <- grep(search_for, file)
    input <- file[line]
    input.split <- strsplit(input, "#")[[1]]
    input.split[1] <- new
    output <- paste(input.split, collapse = " #")
    file[line] <- output
    return(file)
  }

  # Work with starter file
  if (!is.null(starter_file_in)) {
    ss3.starter <- readLines(con = starter_file_in)
    ss3.starter <- manipulate(ss3.starter, "min yr", -1) 
    ss3.starter <- manipulate(ss3.starter, "max yr", -2) 
    writeLines(ss3.starter, con = starter_file_out)
  }
  
  # Work with ctl file
  if (!is.null(ctl_file_in)) {
    ss3.ctl <- readLines(con = ctl_file_in)
    ss3.ctl <- manipulate(ss3.ctl, "first year of main recr_devs", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "last year of main recr_devs", year_end)
    ss3.ctl <- manipulate(ss3.ctl, "_last_early_yr_nobias_adj_in_MPD", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "_first_yr_fullbias_adj_in_MPD", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "_last_yr_fullbias_adj_in_MPD", year_end)
    ss3.ctl <- manipulate(ss3.ctl, "_first_recent_yr_nobias_adj_in_MPD", year_end)
    # Check F ballpark year
    ballpark.val <- strsplit(grep("F ballpark year", ss3.ctl, value = TRUE),
                             "#")[[1]]
    ballpark.val.1 <- as.numeric(sub(" ", "", ballpark.val[1]))
    if (ballpark.val.1 > 0) {
      stop("Currently the function only works if \n
            F ballpark year in the .ctl file is negative.")
    } else{
      ss3.ctl <- manipulate(ss3.ctl, "F ballpark year", year_end * -1)
    }
    fmethod <- strsplit(grep("F_Method", ss3.ctl, value = TRUE), "")[[1]]
    fmethod <- fmethod[-which(fmethod == " ")][1]    
    if (fmethod == 2) {
        F_value.start <- grep("F_value", ss3.ctl) + 1
        F_value.end <- grep("_initial_F_parms", ss3.ctl) - 1
        while(ss3.ctl[F_value.end] == "") {
            F_value.end <- F_value.end - 1
        }
        F.lines <- seq(F_value.start, F_value.end, by = 1)
        for(q in 1:length(F.lines)) {
            f.value <- ss3.ctl[F.lines[q]]
            f.value <- strsplit(f.value, " ")[[1]]
            if (any(f.value == "")) {
                f.value <- f.value[-which(f.value == "")]
            }
            f.value[2] <- year_begin + q - 1
            ss3.ctl[F.lines[q]] <- paste(f.value, collapse = " ")
        }
    }

    writeLines(ss3.ctl, con = ctl_file_out)
  }

  # Work with par file
  if (!is.null(par_file_in)) {
    ss3.par <- readLines(con = par_file_in)
    # Work with recdevs in par file
    recdev.line <- grep("recdev1", ss3.par) + 1
    ss3.par[recdev.line] <- paste(rep(0, year_span), collapse = " ")

    # Work with F values in par file
    F.words <- grep("F", ss3.par, value = TRUE)
      if (any(grepl("Fcast_recruitments", F.words))) {
        warning("right now the code does not work with\n
                Fcast_recruitments")
      }
      if (any(grepl("Fcast_impl_error", F.words))) {
        warning("#right now the code does not work with\n
                Fcast_impl_error")
      }
      if (any(grepl("F_rate", F.words))) {
            F.line <- grep("F_rate", ss3.par)
            F.init.line <- grep("init_F", ss3.par)
            F.template <- c(ss3.par[F.line[1]], 0.0)
            # Remove old F's
            ss3.par <- ss3.par[-seq(F.line[1], 
                                    tail(F.line, 1) + 1, by = 1)]
            counter <- F.init.line + 1
            for (f in seq(year_span)) {
              ss3.par <- append(ss3.par, F.template, 
                                after = counter)
              counter <- counter + 2
            }
      }
    writeLines(ss3.par, con = par_file_out)
  }
  
  # Work with dat file
  if (!is.null(dat_file_in)) {
    ss3.dat <- r4ss::SS_readdat(dat_file_in, verbose = verbose, 
                                echoall = FALSE, section = NULL)
    ss3.dat$styr <- year_begin
    ss3.dat$endyr <- year_end
    # Change catch
    n.catch <- ss3.dat$N_catch
    data.catch <- ss3.dat$catch
    data.catch.seasons <- unique(data.catch$seas)
    dim.fleets <- dim(data.catch)[2] - which(names(data.catch) == "year")
    dim.data <- c(length(data.catch.seasons) * length(years.use), dim.fleets + 2)
    data.catch.new <- matrix(NA, nrow = dim.data[1], ncol = dim.data[2])
  
    counter <- 1
    for(q in seq_along(years.use)) {
      for(s in seq_along(data.catch.seasons)) {
        data.catch.new[counter, ] <- c(rep(0, dim.fleets), years.use[q], 
                                       data.catch.seasons[s])
        counter <- counter + 1
      }
    }
    ss3.dat$catch <- as.data.frame(data.catch.new)
    colnames(ss3.dat$catch) <- NULL
    ss3.dat$N_catch <- dim(data.catch.new)[1]
    # Change CPUE data
    n.cpue  <- ss3.dat$N_cpue
    data.cpue <- ss3.dat$CPUE
    data.cpue.seasons <- unique(data.cpue$seas)
    data.cpue.index <- unique(data.cpue$index)
    num.row <- length(data.cpue.seasons) * length(years.use) * length(data.cpue.index)
    data.cpue.new <- matrix(NA, nrow = num.row, ncol = dim(data.cpue)[2])
    counter <- 1
    for(s in seq_along(data.cpue.seasons)) {
      for(i in seq_along(data.cpue.index)) {
        for(y in seq_along(years.use)) {
          se.value <- subset(data.cpue, index == data.cpue.index[i] & 
                             seas == data.cpue.seasons[s],
                             select = "se_log")[1, 1]
          data.cpue.new[counter, ] <- c(years.use[y], data.cpue.seasons[s], 
                                        data.cpue.index[i], 1, se.value)
          counter <- counter + 1
        }
      }
    }
    colnames(data.cpue.new) <- names(data.cpue)
    ss3.dat$CPUE <- as.data.frame(data.cpue.new)
    ss3.dat$N_cpue <- dim(data.cpue.new)[1]
    # Change discard
    n.discard <- ss3.dat$N_discard
    n.discard.fleets <- ss3.dat$N_discard_fleets
    if (n.discard > 0 & n.discard.fleets > 0) {
      data.discard <- ss3.dat$discard_data
      data.discard.new <- expand.grid(years.use,
                                      unique(data.discard$Seas),
                                      unique(data.discard$Flt), 
                                      0, 0)
      colnames(data.discard.new) <- colnames(data.discard)
      ss3.dat$discard_data <- data.discard.new
      ss3.dat$N_discard <- dim(data.discard.new)[1] 
    }
    # Change meanbodyweight if it exists
    n.meanbdwt <- ss3.dat$N_meanbodywt
    if (n.meanbdwt > 0) {
      ss3.dat$meanbodywt <- ss3.dat$meanbodywt[1, ]
      ss3.dat$meanbodywt$Year <- years.use[1]
      ss3.dat$N_meanbodywt <- dim(data.meanbdwt.new)[1]
    }

    # Change length comps
    n.lcomp <- ss3.dat$N_lencomp
    if (n.lcomp > 0) {
      ss3.dat$lencomp <- ss3.dat$lencomp[1, ]
        ss3.dat$lencomp$Yr[1] <- years.use[1]
        ss3.dat$N_lencomp <- dim(ss3.dat$lencomp)[1]
    }
    # Change age comps
    n.acomp <- ss3.dat$N_agecomp
    if (n.acomp > 0) {
        ss3.dat$agecomp <- ss3.dat$agecomp[1, ]
        ss3.dat$Yr[1] <- years.use[1]
        ss3.dat$N_agecomp <- dim(ss3.dat$agecomp)[1]
    }
    # Change length at age
    n.ccomp <- ss3.dat$N_MeanSize_at_Age_obs
    if (n.ccomp > 0) {
        ss3.dat$MeanSize_at_Age_obs <- ss3.dat$MeanSize_at_Age_obs[1, ]
        ss3.dat$MeanSize_at_Age_obs$Yr <- years.use[1]
        ss3.dat$N_MeanSize_at_Age_obs <- dim(ss3.dat$MeanSize_at_Age_obs)[1]
      }
    # environmental parameters
    num.env <- ss3.dat$N_environ_variables
    if (num.env > 0) {
        data.env <- ss3.dat$envdat
        data.env.variables <- unique(data.env$Variable)
        data.env.new <- data.frame("Yr" = rep(seq(year_begin, year_end), 
                                              num.env),
                                   "Variable" = rep(data.env.variables, each = year_span),
                                   "Value" = rep(0, num.env * year_span))
        ss3.dat$envdat <- data.env.new
    }
    # size frequency
        if (any(grepl("# N sizefreq methods to read", ss3.dat))) {
            stop("change_year does not accomodate dat files with
                  sizefreq methods. Please remove the sizefreq
                  data and run change_year again.")
        }
    SS_writedat(ss3.dat, dat_file_out, overwrite = TRUE, verbose = verbose)
  }
  if (!is.null(forecast_file_in)) {
    nareas <- 1; nfleets <- 1
    if ("ss3.dat" %in% ls()) {
      if (ss3.dat$N_areas > 1) {
        stop("change_year is only set up to work with forecast\n
              files that pertain to a model with 1 area, \n
              please change the forecast file yourself and \n
              set forecast_file_in = NULL")
      }
      nfleets <- ss3.dat$Nfleet
    }
    forecast <- SS_readforecast(forecast_file_in, nfleets, nareas,
                                verbose = verbose)
    forecastlines <- readLines(forecast_file_in)
      realyears <- grep("# after processing", forecastlines, value = TRUE)
      realyears <- strsplit(strsplit(realyears, "# ")[[1]][2], " ")[[1]]
      realyears <- as.numeric(realyears[!realyears == ""])
    if (all(forecast$Bmark_years == realyears)) {
      warning("Forecast biomass and fishing benchmark years\n
              must be set relative to the end year and not as\n
              absolute years. All benchmark years were set to \n
              the terminal year. Please manually change")
      forecast$Bmark_years <- rep(0, 6)
      forecast$Fcast_years <- rep(0, 4)
    }
    forecast$FirstYear_for_caps_and_allocations <- year_end + 
      (forecast$FirstYear_for_caps_and_allocations - max(realyears))
    forecast$Ydecl <- year_end - (max(realyears) - forecast$Ydecl)
    forecast$Yinit <- year_end + (forecast$Yinit - max(realyears))
    if (forecast$fleet_relative_F != 1 | 
        forecast$Ncatch != 0) {
      stop("change_year is not set up do change the relative F\n
            or the catch per year. This must be done manually,\n
            or change to use first-last-allocation year.")
    }
    SS_writeforecast(forecast, file = forecast_file_out,
                     overwrite = TRUE, verbose = verbose)
  }
  }

