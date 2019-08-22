#' Methods to change the years in an ss3sim model
#'
#' \code{change_year} takes SS3 \code{.ctl}, \code{.dat}, \code{.par},
#' \code{starter.ss}, and \code{.forecast} files and changes the start and end
#' year of the model. \code{change_year} works with \pkg{ss3sim} operating or
#' estimation models.
#'
#' @param year_begin Desired start year
#' @param year_end Desired end year
#' @param burnin Length of burnin period. Default is zero for an OM. Use burnin
#'   for EM models, to establish a period with no fishing.
#'   Note that the value should correspond to the number of years, not a year
#'   value.
#' @template ctl_file_in
#' @param ctl_file_out Output SS3 control file, if \code{NULL} the file will be
#'   named the same as the \code{ctl_file_in}
#' @template dat_file_in
#' @param dat_file_out Output SS3 data file, if \code{NULL} the file will be
#'   named the same as the \code{dat_file_in}
#' @template par_file_in
#' @param par_file_out Output SS3 parameter file, if \code{NULL} the file will
#'   be named the same as the \code{par_file_in}
#' @template str_file_in
#' @param str_file_out Output SS3 starter file, if \code{NULL} the file will
#'   be named the same as the \code{str_file_in}
#' @template for_file_in
#' @param for_file_out Output SS3 forecast file, if \code{NULL} the file
#'   will be named the same as the \code{for_file_in}
#' @param verbose Logical argument that is passed on to internal calls to r4ss
#'   functions for reading and writing SS3 files. Setting \code{verbose} to
#'   \code{TRUE} may be useful for troubleshooting.
#' @author Kelli Johnson
#' @return The function creates modified versions of the \code{.par},
#'   \code{.dat}, \code{.ctl}, \code{.starter}, and \code{.forecast} files.
#'
#' @details
#' Operating models and estimation models will not have all of the same files,
#' thus if the file does not exist change the \code{_file_in} to \code{NULL}.
#' The code will add data for all years specified, minus the burnin period,
#' if the data type is present in the \code{dat_file_in} file.
#' The manipulation performed on the \code{dat_file_in} file is not complete
#' and users will need
#' to specify data for years which are deleted. The function removes all composition
#' data except for the first year.
#' To remove data use \code{\link{sample_index}}, \code{\link{sample_lcomp}},
#' or \code{\link{sample_agecomp}}.
#' For models that use the \code{.forecast} file, all references to years
#' must be made with relative values (i.e., 0 or negative integers).
#' The code changes the begining and ending year for benchmark values to be the end
#' year of the model and \code{year_begin} is ignored for the forecast file.
#'
#' @importFrom r4ss SS_readdat SS_readforecast SS_readstarter
#'   SS_writedat SS_writeforecast SS_writestarter
#'
#' @export
#'
#' @examples
#' \donttest{
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
#' system("ss_safe starter.ss -noest")
#'
#' change_year(year_begin = 1, year_end = 100, burnin = 25,
#'  ctl_file_in = "simple.ctl", ctl_file_out = "change_year.ctl",
#'  dat_file_in = "simple.dat", dat_file_out = "change_year.dat",
#'  par_file_in = "ss.par", par_file_out = "change_year.par",
#'  str_file_in = "starter.ss", str_file_out = "change_year_starter.ss",
#'  for_file_in = "forecast.ss", for_file_out = "change_year_forecast.ss")
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
  str_file_in = NULL, str_file_out = "starter.ss",
  for_file_in = NULL, for_file_out = "forecast.ss",
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
  if (is.null(str_file_out)) {
    str_file_out <- str_file_in
  }
  if (is.null(for_file_out)) {
    for_file_out <- for_file_in
  }

  year_span <- year_end - year_begin + 1
  years.use <- year_begin:year_end
  if (burnin > 0) {
    if (length(years.use) <= burnin) {
      stop("Burnin period is greater than or equal to total number of years.")
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
    file
  }

  # Work with starter file
  if (!is.null(str_file_in)) {
    ss3.starter <- SS_readstarter(file = str_file_in, verbose = verbose)
    # -1 to make sdreport start year relative to model start year
    ss3.starter$minyr_sdreport <- -1
    # -2 to make sdreport end year relative to end year + N forecast years
    ss3.starter$maxyr_sdreport <- -2
    SS_writestarter(mylist = ss3.starter, file = str_file_out,
      overwrite = TRUE, verbose = verbose, warn = verbose)
  }

  # Work with ctl file
  if (!is.null(ctl_file_in)) {
    ss3.ctl <- readLines(con = ctl_file_in, warn = verbose)
    # Recruitment deviations: turn off bias adjustment
    ss3.ctl <- manipulate(ss3.ctl, "first year of main recr_devs", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "last year of main recr_devs", year_end)
    ss3.ctl <- manipulate(ss3.ctl, "_recdev_early_start", 0)
    ss3.ctl <- manipulate(ss3.ctl, "_last_yr_nobias_adj_in_MPD", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "_first_yr_fullbias_adj_in_MPD", year_begin)
    ss3.ctl <- manipulate(ss3.ctl, "_last_yr_fullbias_adj_in_MPD", year_end)
    ss3.ctl <- manipulate(ss3.ctl, "_end_yr_for_ramp_in_MPD", year_end)
    ss3.ctl <- manipulate(ss3.ctl, "_max_bias_adj_in_MPD", -1)
    # Check F ballpark year
    ballpark.val <- strsplit(grep("F ballpark year", ss3.ctl, value = TRUE),
                             "#")[[1]]
    ballpark.val.1 <- as.numeric(sub(" ", "", ballpark.val[1]))
    if (ballpark.val.1 > 0) {
      stop(paste("Currently the function only works if F ballpark year",
                  "in the .ctl file is negative."))
    } else{
      ss3.ctl <- manipulate(ss3.ctl, "F ballpark year", year_begin * -1)
    }
    fmethod <- substring(trimws(grep("F_Method:", ss3.ctl, value = TRUE)), 1, 1)
    if (fmethod == "2") {
      # Find number of input lines
      startFline <- grep("# overall start F value; overall phase;", ss3.ctl)
      startFvalue <- strsplit(ss3.ctl[startFline], " ")[[1]]
      startFvalue <- startFvalue[startFvalue != ""]
      nlines <- as.numeric(startFvalue[3])
      # Remove these lines from the ctl
      if (nlines > 0) {
        startFvalue[3] <- 0
        ss3.ctl[startFline] <- paste(startFvalue, collapse = " ")
        F_value.start <- grep("#Fleet Year Seas", ss3.ctl) + 1
        while(!grepl("#", ss3.ctl[F_value.start])) {
          ss3.ctl <- ss3.ctl[-F_value.start]
        }
      }
    }
    writeLines(ss3.ctl, con = ctl_file_out)
  }

  # Work with par file
  if (!is.null(par_file_in)) {
    ss.par <- readLines(con = par_file_in, warn = verbose)
    # Work with recdevs in par file
    recdev.line <- grep("recdev1", ss.par) + 1
    ss.par[recdev.line] <- paste(rep(0, year_span), collapse = " ")

    # Work with F values in par file
    F.words <- grep("F", ss.par, value = TRUE)
      if (any(grepl("Fcast_recruitments", F.words))) {
        warning("right now the code does not work with Fcast_recruitments")
      }
      if (any(grepl("Fcast_impl_error", F.words))) {
        warning("right now the code does not work with Fcast_impl_error")
      }
      if (any(grepl("F_rate", F.words))) {
            F.line <- grep("F_rate", ss.par)
            F.init.line <- grep("init_F", ss.par)
            # Remove old F's
            ss.par <- ss.par[-(F.line[1]:(max(F.line) + 1))]
            counter <- F.init.line + 1
            for (f in seq(year_span)) {
              F.template <- c(paste0("# F_rate[", f, "]:"), 0)
              ss.par <- append(ss.par, F.template,
                                after = counter)
              counter <- counter + 2
            }
      }
    writeLines(ss.par, con = par_file_out)
  }

  # Work with dat file
  if (!is.null(dat_file_in)) {
    ss3.dat <- SS_readdat(dat_file_in, version = NULL, verbose = verbose,
                                echoall = FALSE, section = NULL)
    oldsrtyear <- ss3.dat$styr
    ss3.dat$styr <- year_begin
    # Save old terminal year for the forecast file
    oldendyear <- ss3.dat$endyr
    ss3.dat$endyr <- year_end
    # Change catch
    seafleet.catch <- unique(ss3.dat$catch[, c("seas", "fleet")])
    if (!all(tapply(seafleet.catch$seas, seafleet.catch$fleet,
      function(x) length(unique(x))) == 1)) stop("ss3sim does not",
      " accomodate changing the year of a model with multiple seasons",
      " for a given fleet.")
    data.catch.new <- stats::setNames(do.call("rbind", apply(seafleet.catch, 1,
      function(x) expand.grid(
        c(-999, year_begin:year_end), x[1], x[2], 1, 0.01,
        stringsAsFactors = FALSE))),
      colnames(ss3.dat[["catch"]]))
    row.names(data.catch.new) <- NULL
    data.catch.new[data.catch.new[, "year"] < 0, "catch"] <- 0
    ss3.dat$catch <- data.catch.new

    # Change CPUE data
    seafleet.cpue <- unique(ss3.dat$CPUE[, c("seas", "index")])
    if (!all(tapply(seafleet.cpue$seas, seafleet.cpue$index,
      function(x) length(unique(x))) == 1)) stop("ss3sim does not",
      " accomodate changing the year of a model with multiple seasons",
      " for a given index.")
    data.cpue.new <- stats::setNames(do.call("rbind", apply(seafleet.cpue, 1,
      function(x) expand.grid(year_begin:year_end, x[1], x[2], 1, 0.01,
        stringsAsFactors = FALSE))),
      colnames(ss3.dat[["CPUE"]]))
    row.names(data.cpue.new) <- NULL
    ss3.dat$CPUE <- data.cpue.new
    # Change discard
    if (ss3.dat$N_discard_fleets > 0) {
      seafleet.disc <- unique(ss3.dat$discard_data[, c("Seas", "Flt")])
      if (!all(tapply(seafleet.disc$Seas, seafleet.disc$Flt,
        function(x) length(unique(x))) == 1)) stop("ss3sim does not",
        " accomodate changing the year of a model with multiple seasons",
        " for a given discard fleet.")
      data.disc.new <- stats::setNames(do.call("rbind", apply(seafleet.disc, 1,
        function(x) expand.grid(year_begin:year_end, x[1], x[2], 1, 0.01,
        stringsAsFactors = FALSE))),
        colnames(ss3.dat[["discard_data"]]))
      row.names(data.disc.new) <- NULL
      ss3.dat$discard_data <- data.disc.new
    }
    # Change meanbodyweight if it exists
    if (!is.null(ss3.dat$meanbodywt)) {
      seafleet.mbdwt <- unique(ss3.dat$meanbodywt[,
        c("Seas", "Fleet", "Partition", "Type")])
      if (!all(tapply(seafleet.mbdwt$Seas,
        INDEX = seafleet.mbdwt[, c("Fleet", "Partition", "Type")],
        function(x) length(unique(x)))[,1,] == 1)) stop("ss3sim does not",
        " accomodate changing the year of a model with multiple seasons",
        " for a given fleet with mean body weight data.")
      data.mbdwt.new <- stats::setNames(do.call("rbind", apply(seafleet.mbdwt, 1,
        function(x) expand.grid(year_begin:year_end, x[1], x[2], x[3], x[4], 1, 0.01,
        stringsAsFactors = FALSE))),
        colnames(ss3.dat[["meanbodywt"]]))
      row.names(data.mbdwt.new) <- NULL
      ss3.dat$meanbodywt <- data.mbdwt.new
    }
    # Change length comps
    if (!is.null(ss3.dat$lencomp)) {
      ss3.dat$lencomp <- ss3.dat$lencomp[1, ]
      ss3.dat$lencomp$Yr <- year_end
    }
    # Change age comps
    if (!is.null(ss3.dat$agecomp)) {
      ss3.dat$agecomp <- ss3.dat$agecomp[1, ]
      ss3.dat$agecomp$Yr <- year_end
    }
    # Change length at age
    if (!is.null(ss3.dat$MeanSize_at_Age_obs)) {
      ss3.dat$MeanSize_at_Age_obs <- ss3.dat$MeanSize_at_Age_obs[1, ]
      ss3.dat$MeanSize_at_Age_obs$Yr <- year_end
    }
    # environmental parameters
    if (ss3.dat$N_environ_variables > 0) {
      ss3.dat$envdat <- stats::setNames(expand.grid(year_begin:year_end,
        unique(ss3.dat$envdat$Variable), 0,
        stringsAsFactors = FALSE),
        colnames(ss3.dat$envdat))
    }
    # size frequency
    if (ss3.dat$N_sizefreq_methods > 0) {
      stop("change_year does not accomodate dat files with ",
        "sizefreq methods.\nPlease remove the sizefreq",
        "data and run change_year again.")
    }
    r4ss::SS_writedat(ss3.dat, dat_file_out, overwrite = TRUE,
      version = ss3.dat$ReadVersion, verbose = verbose)
  }

  # Work with forecast file
  if (!is.null(for_file_in)) {
    # todo: remove the hard coding of nseas to 1.
    for_ss_vers <- get_ss_ver_file(for_file_in) # get version from for_file_in
    forecast <- SS_readforecast(for_file_in, nfleets, nareas,
      version = for_ss_vers, verbose = verbose)
    # Mandatory changes
    forecast$Bmark_years <- rep(year_end, 10) # Change relative to end year
    forecast$Fcast_years <- rep(0, 6) # Change relative to end year
    forecast$Ydecl <- years.use[1]
    forecast$Yinit <- -1 # Change to endyear + 1
    if (!is.null(nrow(forecast$ForeCatch))) {
      if (verbose) message(paste("Removing all catches from forecast file."))
      forecast$ForeCatch <- NULL
    }
    if (forecast$FirstYear_for_caps_and_allocations != 0) {
      if (verbose) {
        message(paste0("Changing first year of caps and allocations in ",
        "forecast file from ", forecast$FirstYear_for_caps_and_allocations, " to ",
        year_end + 1, "."))
      }
        forecast$FirstYear_for_caps_and_allocations <- year_end + 1
    }
    if (all(forecast$max_totalcatch_by_fleet == 0)) {
      forecast$max_totalcatch_by_fleet <- NULL
    }
    if (all(forecast$max_totalcatch_by_area == 0)) {
      forecast$max_totalcatch_by_area <- NULL
    }
    # changing 0 in fleet_assignment_to_allocation_group to "#" will not work
    # with updated r4ss, so don't change.
    # if (all(forecast$fleet_assignment_to_allocation_group == 0)) {
    #   forecast$fleet_assignment_to_allocation_group <- "#"
    # }

    if (forecast$fleet_relative_F != 1) {
      stop(paste("change_year is not set up do change the relative F",
            "or the catch per year. This must be done manually,",
            "or change to use first-last-allocation year."))
    }
    r4ss::SS_writeforecast(forecast, file = for_file_out,
                     overwrite = TRUE, verbose = verbose)
  }
}
