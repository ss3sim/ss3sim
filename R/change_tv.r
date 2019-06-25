#' Methods to include time-varying parameters in an SS3 operating model
#'
#' \code{change_tv} takes SS3 \code{.ctl}, \code{.par}, and \code{.dat} files
#' and implements time-varying parameters using environmental variables.
#' \code{change_tv} is specifically set up to work with an operating model
#' \code{.ctl} file.
#'
#' @param change_tv_list *A list of named vectors. Names correspond to parameters
#' in the operating model that currently do not use environmental deviations and
#' the vectors correspond to deviations. See the section "Specifying the
#' \code{change_tv_list}" below for help on specifying this argument.
#' @template ctl_file_in
#' @template ctl_file_out
#' @template dat_file_in
#' @template dat_file_out
#' @template par_file_in
#' @template par_file_out
#' @template str_file_in
#' @template str_file_out
#' @author Kotaro Ono, Carey McGilliard, Kelli Johnson, and Kathryn Doering
#' @family change functions
#' @return The function creates modified versions of the \code{.par},
#'   \code{.starter}, \code{.ctl}, and \code{.dat} files.
#' @template casefile-footnote
#' @details
#' Although there are three ways to implement time-varying parameters within
#' SS3, \pkg{ss3sim} and \code{change_tv} only use the environmental variable
#' option. Within SS3, time-varying parameters work on an annual time-step.
#' Thus, for models with multiple seasons, the time-varying parameters will
#' remain constant for the entire year.
#'
#' The \code{ctl_file_in} argument needs to be a \code{.ss_new} file because
#' the documentation in \code{.ss_new} files are automated and standardized.
#' This function takes advantage of the standard documentation the
#' \code{.ss_new} files to determine which lines to manipulate and where to
#' add code in the \code{.ctl}, \code{.par}, and \code{.dat} files, code that
#' is necessary to implement time-varying parameters.
#'
#' \pkg{ss3sim} uses annual recruitment deviations and may not work with a
#' model that ties recruitment deviations to environmental covariates. If you
#' need to compare the environment to annual recruitment deviations, the
#' preferred option is to transform the environmental variable into an age 0
#' pre-recruit survey. See page 55 of the SS3 version 3.24f manual for more
#' information.
#'
#' @section Specifying the \code{change_tv_list}:
#' Parameters will change to vary with time according to the vectors of
#' deviations passed to \code{change_tv_list}. Vectors of deviations, also
#' referred to as environmental data, must have a length equal to \code{
#' endyr-startyr+1}, where \code{endyr} and \code{startyr} are specified the
#' \code{.dat} file. Specify years without deviations as zero.
#'
#' Parameter names must be unique and match the full parameter name in the
#' \code{.ctl} file. Names for stock recruit parameters must contain "devs",
#' "R0", or "steep", and only one stock recruit parameter can be time-varying
#' per model.
#'
#' This feature will include an *additive* functional linkage between
#' environmental data and the parameter where the link parameter is fixed at a
#' value of one and the par value is specified in the \code{.par} file:
#' \eqn{par'[y] = par + link * env[y]}.
#'
#' For catchability (\eqn{q}) the *additive* functional linkage is implemented
#' on the log scale: \eqn{ln(q'[y]) = ln(q) + link * env[y]}
#'
#' @section Passing arguments to \code{change_tv} through \code{\link{run_ss3sim}}:
#' (1) create a case file with an arbitrary letter
#' not used elsewhere (anything but D, E, F, or R) and (2) include the line
#' \code{function_type; change_tv} in your case file. For example, you might
#' want to use M for natural mortality, S for selectivity, or G for growth.
#'
#' @importFrom r4ss SS_readstarter SS_writestarter SS_readdat
#'
#' @export
#'
#' @examples
#' \donttest{
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-tv-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#'
#' # Find the SS3 "Simple" model in the package data:
#' d <- system.file("extdata", package = "ss3sim")
#' simple <- paste0(d, "/Simple")
#' dir.create("Simple")
#' file.copy(simple, ".", recursive = TRUE)
#' setwd("Simple")
#'
#' # Run SS3 to create control.ss_new and ss.par:
#' system("ss_safe starter.ss -nohess")
#'
#' change_tv(change_tv_list = list("NatM_p_1_Fem_GP_1" = c(rep(0, 20),
#'       rep(.1, 11)), "SR_BH_steep"=rnorm(31, 0, 0.05)), ctl_file_in =
#'   "simple.ctl", ctl_file_out = "example.ctl", dat_file_in =
#'   "simple.dat", dat_file_out = "example.dat")
#'
#' # Clean up:
#' setwd("../")
#' unlink("Simple")
#' setwd(wd)
#' }

change_tv <- function(change_tv_list,
  ctl_file_in = "control.ss_new", ctl_file_out = "om.ctl",
  dat_file_in = "ss3.dat", dat_file_out = "ss3.dat",
  par_file_in = "ss.par", par_file_out = "ss.par",
  str_file_in = "starter.ss", str_file_out = "starter.ss") {

  # Always use safe mode here:
  ss_bin <- "ss_safe"

  # read in necessary ss files.
  ss3.ctl     <- readLines(con = ctl_file_in)
  ss3.dat     <- SS_readdat(dat_file_in, verbose = FALSE)
  ss3.starter <- SS_readstarter(file = str_file_in, verbose = FALSE)
  ss3.ctl.parlines <- SS_parlines(ctl_file_in, verbose = FALSE)

  # Function requires that the directories associated with the out files are all
  #   the same, so check this.
  out_dirnames <- lapply(c(ctl_file_out, dat_file_out, par_file_out, str_file_out),
                        function(x) dirname(x))
  out_dirnames <- unique(unlist(out_dirnames))
  if (length(out_dirnames) > 1) {
    stop("Directories for all out files need to be the same in order for the",
         "function change_tv to work.")
  }

  # For all variables the following coding is used
   # mg = Natural mortality and growth parameters
   # sr = Stock recruit parameters
   # qs = Catchability paramaters
   # sx = Selectivity parameters

  # TODO:change so this script can be used with other time varying
  # parameters already in the model
  # TODO: Add in capabilities to change timevarying SR parameters. only 1 time
  # varying SR parameter allowed?
  # Need to check whether this is true or not for SS3.30 like it was for SS3.24.
  # TODO: if the variable being added is the same over years, then there is no
  # need to use timevarying. Check this, and change the init value in the ctl
  # file instead of time varying if this is the case.

  # check that the parameter we want to change is not already time varyingin the
  # model; if it is, stop code
  baseom_tv <-  ss3.ctl.parlines[(ss3.ctl.parlines$Label %in% names(change_tv_list)), ,
                                 drop = FALSE]
  if(any(baseom_tv[, c("env-var", "use_dev", "Block")] != 0)) {
    stop("One or more of the parameters listed in change_tv is already ",
         "time-varying in the base operating model. ss3sim cannot change ",
         "time-varying properties of parameters that are already specified as ",
         "time-varying.")
  }
  # For now, stop if there are any time varying parameters specified.
  # TODO: make this so the existing time varying structure can be retained., and
  # this check can be eliminated.
  if(any(ss3.ctl.parlines[ , c("env-var", "use_dev", "Block")] != 0)) {
    stop( "There are one or more environmental linkages specified already in",
          " the base operating model. At this time, ss3sim cannot change time-",
          "varying properties of parameters when there are already time-varying",
          "parameters specified in the base operating model.")
  }

  # Divide .dat file at the environmental variable table
  # If no environmental variables create an empty table to be filled later
  dat.varnum.counter <- ss3.dat$N_environ_variables

  if(is.null(ss3.dat$envdat)) {
    ss3.dat.tbl <- data.frame(array(dim=c(0,3),
                                    dimnames = list(NULL,
                                                    c("Yr", "Variable", "Value"))))
  } else {
    ss3.dat.tbl <- ss3dat$envdat
  }

  # Find where the params to change are in the control file, and stop the
  # script if it cannot be found.
  divider.a <- grep("#_Spawner-Recruitment", ss3.ctl, fixed = TRUE)[1]
  divider.b <- grep("#_Q_setup", ss3.ctl, fixed = TRUE)[1]
  divider.c <- grep("_size_selex_patterns", ss3.ctl, fixed = TRUE)[1]
  lab <- sapply(names(change_tv_list),
                function(x) {
                  val <- grep(pattern = x, x = ss3.ctl, fixed = TRUE)[1]
                  if(is.na(val)) {
                    stop( "Could not locate the parameter ", x, " in the ",
                          "operating model .ctl file. Check that the parameter",
                          " is spelled correctly and in the correct case. ",
                          "Have you standardized your .ctl file by running it ",
                          "through SS and used the control.ss_new file?")
                  }
                               if(val < divider.a) temp <- "mg"
                               if(val > divider.a & val < divider.b) temp <- "sr"
                               if(val > divider.b & val < divider.c) temp <- "qs"
                               if(val > divider.c) temp <- "sx"
                               if(x %in% ss3.dat$fleetnames) temp <- "qs"
                               temp
                }
               )
  tab <- as.data.frame.table(table(lab))
  # Check that there are no environmental links specified

# For MG parms or selectivity parameters, turn on the environmental link in the
# ctl file parameter line and add the values to the environmental data in the
# data file.
  temp.data <- change_tv_list#[lab == "mg" | lab == "sr"| lab == "sx" | lab == "qs"]
for(i in seq_along(temp.data)) {
  dat.varnum.counter <- dat.varnum.counter + 1
  par.ch <- grep(names(temp.data)[i], ss3.ctl, fixed = TRUE)[1]
  par.ex <- regexpr(names(temp.data)[i], ss3.ctl[par.ch])[1]
  val <- strsplit(substr(ss3.ctl[par.ch], start=1, stop=par.ex-1), " ")[[1]]
    # Check that the par line is the correct length.
    # values might include spaces or #, make them NA and remove
    # should result in a vector with length == 14
    val <- suppressWarnings(as.numeric(val))
    check <- is.na(val)
    if (sum(check) > 0) {
      val <- val[check == FALSE]
        if(length(val) < 14) {
          stop(paste("Please check", names(temp.data)[i], "in the control file,
               because the vector is less than 14 entries."))
        }
    }
    # Set the environmental link and environmental var (8th element)
    # where a 3 digit value is put in to specify link and env var. 2 is used
    # for an additive link. (e.g., to sepecify an additive link with env. var 3,
    # put 203 as the 8th element.)
    # link = 2 use an additive fxn of environmental variable (g)
    # value of g in year y (env(y,-g))
    # param`(y) = param + link*env(y,-g)
    val[8] <- as.numeric(paste0("2", "0", dat.varnum.counter))
    ss3.ctl[par.ch] <- paste(c(val, "#",  names(temp.data)[i]), collapse = " ")
  dat <- data.frame("Yr" = ss3.dat$styr:ss3.dat$endyr,
                    "Variable" = dat.varnum.counter,
                    "Value" = temp.data[i])
    ss3.dat.tbl <- rbind(ss3.dat.tbl, dat)
}


  # Set time varying autogeneration setting to all 1's to make sure SS will not
  # autogenerate them.
  autogen_line <- grep("# autogen: ", ss3.ctl)
  ss3.ctl[grep("# autogen: ", ss3.ctl)] <- paste0("1 1 1 1 1 # autogen: 1st ",
                                                  "element for biology, 2nd ",
                                                  "for SR, 3rd for Q, 4th",
                                                  "reserved, 5th for selex")

  # find comment for no time varying MG
  tv_cmt <- grep("#_no timevary", ss3.ctl) #unfortunately, does not exist for SR
  if (length(tv_cmt) < 3) {
  stop("ss3sim could not find all ctl file locations where time varying ",
       "short parameter lines should be specified for biology, catchability, ",
       "and selectivity. Please make sure you are using a standardized ctl ",
       "file generated from SS (i.e., was a control.ss_new file)"
       )
  }
  if (length(tv_cmt) > 3) {
    stop("There are more than three lines in the control file ", ctl_file_in,
         " that include the text '#_no timevary. Please make sure you are",
         "using a control file that has the default .ss_new formatting.")
  }

  # add short time varying parameter lines at their necessary sections.
  for(n in c("mg", "qs", "sx")) {
    n_string <- switch(n, "mg" = "MG", "qs" = "Q", "sx" = "selex")
    ss3.ctl <- add_tv_parlines(n, tab, n_string, ss3.ctl)
  }
  if("sr" %in% tab$lab) {
    #TODO:  allow SR params to be changed
    stop("Currently, ss3sim cannot manipulate stock recruitment time varying",
         "parameters. Please contact the developers if you are interested in",
         "using this feature.")

  }
  # Finish changing the data file based on the tv params.
  ss3.dat$N_environ_variables <- dat.varnum.counter
  ss3.dat$envdat <- ss3.dat.tbl

  #run SS with with no estimation and no hessian to get a new .par file and
  # a control ss_new file.
  #first change starter file option from use .par to .ctl
  ss3.starter$init_values_src <- 0 # dont use par
  ss3.starter$datfile <- basename(dat_file_out)
  ss3.starter$ctlfile <- basename(ctl_file_out)
  #write out files needed for this run.
  writeLines(ss3.ctl, ctl_file_out)
  SS_writedat(ss3.dat, dat_file_out, overwrite = TRUE, verbose = FALSE)
  SS_writestarter(mylist = ss3.starter, dir = dirname(str_file_out),
                  file = basename(str_file_out), overwrite = TRUE,
                  verbose = FALSE, warn = FALSE)

  # get rid of control.ss_new if it exists, as its presence will be used to
  # check that the model ran successfully

  if(file.exists(file.path(dirname(str_file_out), "control.ss_new"))) {
    file.remove(file.path(dirname(str_file_out), "control.ss_new"))
  }
  #In case putting the out files in a different base dir, change the wd and copy
  # over the forcast file
  if(dirname(ctl_file_in) != dirname(ctl_file_out)) {
    file.copy(file.path(dirname(str_file_in),"forecast.ss"),
              to = file.path(dirname(str_file_out), "forecast.ss"),
              overwrite = TRUE) #use the same forecast file.
    old_dir <- getwd()
    setwd(dirname(ctl_file_out))
    on.exit(setwd(old_dir), add = TRUE) # just in case exits on error.
  }
 #TODO: change to using run_ss3model instead, if possible. (but may not be
  # because of different file path name?)
  bin <- get_bin(ss_bin)
  #Call ss3 for a run that includes the environmental link
  message("Running OM to add timevarying parameters.")
  os <- .Platform$OS.type
  if(os == "unix") {
    system(paste(bin, "-nohess"), ignore.stdout = TRUE)
    } else {
      system(paste(bin, "-nohess"), show.output.on.console = FALSE,
             invisible = TRUE, ignore.stdout = TRUE)
    }

  # change wd back, if needed.
  if(dirname(ctl_file_in) != dirname(ctl_file_out)) {
    setwd(old_dir)
  }

  # Change starter file option back to using .par and write to file again.
  ss3.starter$init_values_src <- 1
  SS_writestarter(mylist = ss3.starter, dir = dirname(str_file_out),
                  file = basename(str_file_out), overwrite = TRUE,
                  verbose = FALSE, warn = FALSE)

  #Check that model ran successfully
  if(!file.exists(file.path(dirname(str_file_out), "control.ss_new"))) {
    stop("OM model run during change_tv() did not complete. Please check the",
         " model files in ", normalizePath(dirname(str_file_out)), ".")
  }
  # read in the new par and ctl file files. Note that
  # ss.par are the names created by SS when model runs are done.
  ss.par    <- readLines(file.path(dirname(str_file_out), "ss.par"))
  ss3.ctl_new <- readLines(file.path(dirname(str_file_out), "control.ss_new"))

  # Use the control.ss_new file from the model run so the formatting is better.
  file.copy(file.path(dirname(str_file_out), "control.ss_new"),
            to = ctl_file_out, overwrite = TRUE)
}

#' Add short time varying parameter lines. At time of writing, this method will
#' work for MG, selectivity, and catchability time varying, but not for SR
#' @param string The code representing the section the parameter is from.
#' @param tab As created in \code{change_tv()}
#' @param ctl_string The code as called in the .ss_new comment for time varying.
#' @param ss3.ctl A ss control file that has been read in using \code{readLines()}.
#' @return A modified version of ss3.ctl (a vector of strings), containing the
#'   new parameter line
add_tv_parlines <- function(string, tab, ctl_string, ss3.ctl) {
  if (string %in% tab$lab){
    line_num <- grep(paste0("#_no timevary ",ctl_string) , ss3.ctl)
    n_pars <- tab[tab$lab == string, "Freq"]
    # Don't bother labeling, because will run through to get ss_new control.
    if(n_pars > 0) {
      lines <- rep("-2 2 1 0 99 0 -5", times = n_pars)
      ss3.ctl <- append(ss3.ctl, lines, after = line_num)
    }
  }
  ss3.ctl
}
