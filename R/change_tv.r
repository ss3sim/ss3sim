#' Methods to include time-varying parameters in a Stock Synthesis operating model
#'
#' `change_tv` takes Stock Synthesis `.ctl`, `.par`, and `.dat` files
#' and implements time-varying parameters using environmental variables.
#' `change_tv` is specifically set up to work with an operating model
#' `.ctl` file.
#'
#' @param change_tv_list A list of named vectors. Names correspond to parameters
#' in the operating model that currently do not use environmental deviations and
#' the vectors correspond to deviations. See the section
#' "Specifying the `change_tv_list`" for help on specifying this argument.
#' @template ctl_file_in
#' @template ctl_file_out
#' @template dat_file_in
#' @template dat_file_out
#' @author Kotaro Ono, Carey McGilliard, Kelli Johnson, and Kathryn Doering
#' @family change functions
#' @return The function creates modified versions of the  `.ctl` and
#'   `.dat` files if ctl_file_out and dat_file_out are not NULL. The function
#'   also returns a list of the modified `.ctl` and `.dat` R objects
#'   invisibly.
#' @details
#' Although there are three ways to implement time-varying parameters within
#' Stock Synthesis, \pkg{ss3sim} and `change_tv` only use the environmental variable
#' option.
#' Within Stock Synthesis, time-varying parameters work on an annual time-step.
#' Thus, for models with multiple seasons, the time-varying parameters will
#' remain constant for the entire year.
#'
#' The `ctl_file_in` argument needs to be a `.ss_new` file because
#' the documentation in `.ss_new` files are automated and standardized.
#' This function takes advantage of the standard documentation the
#' `.ss_new` files to determine which lines to manipulate and where to
#' add code in the `.ctl`, `.par`, and `.dat` files, code that
#' is necessary to implement time-varying parameters.
#'
#' \pkg{ss3sim} uses annual recruitment deviations and may not work with a
#' model that ties recruitment deviations to environmental covariates. If you
#' need to compare the environment to annual recruitment deviations, the
#' preferred option is to transform the environmental variable into an age 0
#' pre-recruit survey. See page 55 of the Stock Synthesis version 3.24f manual for more
#' information.
#'
#' @section Specifying the `change_tv_list`:
#' Parameters will change to vary with time according to the vectors of
#' deviations passed to `change_tv_list`. Vectors of deviations, also
#' referred to as environmental data, must have a length equal to
#' `endyr-startyr+1`, where `endyr` and `startyr` are specified the
#' `.dat` file. Specify years without deviations as zero.
#'
#' Parameter names must be unique and match the full parameter name in the
#' `.ctl` file. Names for stock recruit parameters must contain "devs",
#' "R0", or "steep", and only one stock recruit parameter can be time-varying
#' per model.
#'
#' This feature will include an *additive* functional linkage between
#' environmental data and the parameter where the link parameter is fixed at a
#' value of one and the par value is specified in the `.par` file:
#' \eqn{par'[y] = par + link * env[y]}.
#'
#' For catchability (\eqn{q}) the *additive* functional linkage is implemented
#' on the log scale: \eqn{ln(q'[y]) = ln(q) + link * env[y]}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Create a temporary folder for the output and set the working directory:
#' temp_path <- file.path(tempdir(), "ss3sim-tv-example")
#' dir.create(temp_path, showWarnings = FALSE)
#' wd <- getwd()
#' setwd(temp_path)
#' on.exit(setwd(wd), add = TRUE)
#'
#' d <- system.file("extdata", package = "ss3sim")
#' om <- file.path(d, "models", "cod-om")
#' dir.create("cod-om")
#' file.copy(om, ".", recursive = TRUE)
#' setwd("cod-om")
#'
#' change_tv(
#'   change_tv_list =
#'     list(
#'       "NatM_p_1_Fem_GP_1" = c(rep(0, 20), rep(.1, 80)),
#'       "SR_BH_steep" = stats::rnorm(100, 0, 0.05)
#'     ),
#'   ctl_file_in = "codOM.ctl",
#'   ctl_file_out = "example.ctl",
#'   dat_file_in = "codOM.dat",
#'   dat_file_out = "example.dat"
#' )
#'
#' # Clean up:
#' unlink("cod-om", recursive = TRUE)
#' }
#'
change_tv <- function(change_tv_list,
                      ctl_file_in = "control.ss_new", ctl_file_out = "om.ctl",
                      dat_file_in = "ss3.dat", dat_file_out = "ss3.dat") {
  # read in necessary ss files.
  ss3.ctl <- readLines(con = ctl_file_in)
  ss3.dat <- r4ss::SS_readdat(dat_file_in, verbose = FALSE)
  ss3.ctl.parlines <- r4ss::SS_parlines(ctl_file_in, verbose = FALSE)

  # Function requires that the directories associated with the out files are all
  #   the same, so check this.
  if (!(is.null(ctl_file_out) | is.null(dat_file_out))) {
    out_dirnames <- lapply(c(ctl_file_out, dat_file_out), function(x) dirname(x))
    out_dirnames <- unique(unlist(out_dirnames))
    if (length(out_dirnames) > 1) {
      stop(
        "Directories for all out files need to be the same in order for the",
        "function change_tv to work."
      )
    }
  }

  # For all variables the following coding is used
  # mg = Natural mortality and growth parameters
  # sr = Stock recruit parameters
  # qs = Catchability paramaters
  # sx = Selectivity parameters

  # TODO:change so this script can be used with other time varying
  # parameters already in the model
  # TODO: if the variable being added is the same over years, then there is no
  # need to use timevarying. Check this, and change the init value in the ctl
  # file instead of time varying if this is the case. (might be best to do this
  # in a separate function.)

  # check that the parameter we want to change is not already time varyingin the
  # model; if it is, stop code
  baseom_tv <- ss3.ctl.parlines[(ss3.ctl.parlines$Label %in% names(change_tv_list)), ,
    drop = FALSE
  ]
  # get the par names
  env_var_name <- colnames(baseom_tv)[grep("^env[_-]var", colnames(baseom_tv))]
  use_dev_name <- colnames(baseom_tv)[grep("(^dev_link$)|(^use_dev$)", colnames(baseom_tv))]
  block_name <- colnames(baseom_tv)[grep("^Block$", colnames(baseom_tv))]
  tv_names <- c(env_var_name, use_dev_name, block_name)
  if (length(tv_names) != 3) {
    stop("ss3sim not reading time varying column names correctly.")
  }
  if (any(baseom_tv[, tv_names] != 0)) {
    stop(
      "One or more of the parameters listed in change_tv is already ",
      "time-varying in the base operating model. ss3sim cannot change ",
      "time-varying properties of parameters that are already specified as ",
      "time-varying."
    )
  }
  # For now, stop if there are any time varying parameters specified.
  # TODO: make this so the existing time varying structure can be retained., and
  # this check can be eliminated.
  if (any(ss3.ctl.parlines[, tv_names] != 0)) {
    stop(
      "There are one or more environmental linkages specified already in",
      " the base operating model. At this time, ss3sim cannot change time-",
      "varying properties of parameters when there are already time-varying",
      "parameters specified in the base operating model."
    )
  }

  # check the user is not requesting tv paramters that should not or cannot be
  # time varying
  invalid_pars <- c("CV_young", "CV_old", "SR_sigmaR", "SR_autocorr")
  any_invalid <- lapply(invalid_pars, function(x) grep(x, names(change_tv_list)))
  if (length(unique(unlist(any_invalid))) > 0) {
    stop(
      "Some requested time-varying parameters are not allowed. User ",
      "requested variable(s) ",
      paste0(invalid_pars[which(any_invalid > 0)], collapse = ", "),
      " should not or cannot be time varying in Stock Synthesis models."
    )
  }
  # Divide .dat file at the environmental variable table
  # If no environmental variables create an empty table to be filled later
  dat.varnum.counter <- ss3.dat$N_environ_variables

  if (is.null(ss3.dat$envdat)) {
    ss3.dat.tbl <- data.frame(array(
      dim = c(0, 3),
      dimnames = list(
        NULL,
        c("Yr", "Variable", "Value")
      )
    ))
  } else {
    ss3.dat.tbl <- ss3.dat$envdat
  }

  # Find where the params to change are in the control file, and stop the
  # script if it cannot be found.
  divider.a <- grep("#_Spawner-Recruitment", ss3.ctl, fixed = TRUE)[1]
  divider.b <- grep("#_Q_setup", ss3.ctl, fixed = TRUE)[1]
  divider.c <- grep("_size_selex_patterns", ss3.ctl, fixed = TRUE)[1]
  lab <- sapply(
    names(change_tv_list),
    function(x) {
      val <- grep(pattern = x, x = ss3.ctl, fixed = TRUE)[1]
      if (is.na(val)) {
        stop(
          "Could not locate the parameter ", x, " in the ",
          "operating model .ctl file. Check that the parameter",
          " is spelled correctly and in the correct case. ",
          "Have you standardized your .ctl file by running it ",
          "through Stock Synthesis and used the control.ss_new file?"
        )
      }
      if (val < divider.a) temp <- "mg"
      if (val > divider.a & val < divider.b) temp <- "sr"
      if (val > divider.b & val < divider.c) temp <- "qs"
      if (val > divider.c) temp <- "sx"
      if (x %in% ss3.dat$fleetnames) temp <- "qs"
      temp
    }
  )
  tab <- as.data.frame.table(table(lab))
  # For MG parms or selectivity parameters, turn on the environmental link in the
  # ctl file parameter line and add the values to the environmental data in the
  # data file.
  temp.data <- change_tv_list # [lab == "mg" | lab == "sr"| lab == "sx" | lab == "qs"]
  for (i in seq_along(temp.data)) {
    dat.varnum.counter <- dat.varnum.counter + 1
    par.ch <- grep(names(temp.data)[i], ss3.ctl, fixed = TRUE)[1]
    par.ex <- regexpr(names(temp.data)[i], ss3.ctl[par.ch], fixed = TRUE)[1]
    val <- strsplit(substr(ss3.ctl[par.ch], start = 1, stop = par.ex - 1), " ")[[1]]
    # Check that the par line is the correct length.
    # values might include spaces or #, make them NA and remove
    # should result in a vector with length == 14
    val <- suppressWarnings(as.numeric(val))
    check <- is.na(val)
    if (sum(check) > 0) {
      val <- val[check == FALSE]
      if (length(val) < 14) {
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
    ss3.ctl[par.ch] <- paste(c(val, "#", names(temp.data)[i]), collapse = " ")
    dat <- data.frame(
      "Yr" = ss3.dat$styr:ss3.dat$endyr,
      "Variable" = dat.varnum.counter,
      "Value" = temp.data[i]
    )
    colnames(dat) <- c("Yr", "Variable", "Value")
    ss3.dat.tbl <- rbind(ss3.dat.tbl, dat)
  }

  # Set time varying autogeneration setting to all 1's to make sure Stock Synthesis will not
  # autogenerate them.
  autogen_line <- grep("# autogen: ", ss3.ctl)
  ss3.ctl[grep("# autogen: ", ss3.ctl)] <- paste0(
    "1 1 1 1 1 # autogen: 1st ",
    "element for biology, 2nd ",
    "for SR, 3rd for Q, 4th",
    "reserved, 5th for selex"
  )
  # find comment for no time varying MG
  tv_cmt <- grep("#_no timevary", ss3.ctl) # unfortunately, does not exist for SR
  # look for each one.
  tv_cmt_lines <- ss3.ctl[tv_cmt]
  parnames <- paste0(c("MG", "SR", "Q", "selex"), " parameters")
  for (n in parnames) {
    tmp_line <- grep(n, tv_cmt_lines)
    if (is.null(tmp_line) & (n != "SR parameters")) {
      stop(
        "ss3sim could not find the ctl file location where time varying ",
        "short parameter lines should be specified for", n, ". Please make",
        "sure you are using a standardized ctl file generated from Stock Synthesis ",
        "(i.e., was a control.ss_new file"
      )
    }
  }
  if (length(tv_cmt) > 4) {
    stop(
      "There are more than four lines in the control file ", ctl_file_in,
      " that include the text '#_no timevary. Please make sure you are",
      "using a control file that has the default .ss_new formatting."
    )
  }
  # add short time varying parameter lines at their necessary sections.
  if ("sr" %in% tab$lab) {
    if (!is.null(grep("SR parameters", tv_cmt_lines))) {
      ss3.ctl <- add_tv_parlines("sr", tab, "SR parameters", ss3.ctl)
    } else {
      # TODO:  make it so it is not necessary to run the model file through
      # 3.30.14 or greater if want to use SR params?
      stop(
        "ss3sim could not find the place where stock recruitment time varying",
        "parameters should be added. Please run your OM model through Stock Synthesis ",
        " version >= 3.30.14 to get the necessary line for ss3sim to search",
        "for: `#_no timevary SR parameters` in the control.ss_new file."
      )
    }
  }
  for (n in c("mg", "qs", "sx")) {
    n_string <- switch(n,
      "mg" = "MG parameters",
      "qs" = "Q parameters",
      "sx" = "selex parameters"
    )
    ss3.ctl <- add_tv_parlines(n, tab, n_string, ss3.ctl)
  }
  # Finish changing the data file based on the tv params.
  ss3.dat$N_environ_variables <- dat.varnum.counter
  ss3.dat$envdat <- ss3.dat.tbl

  # Write out manipulated files.
  # TODO: may want to make the ctl file have more standard commenting if not
  # going to run the OM except for once at the end... eventually, perhaps using
  # r4ss::SS_read/writectl instead can be useful in this regard.
  if (!is.null(ctl_file_out)) writeLines(ss3.ctl, ctl_file_out)
  if (!is.null(dat_file_out)) {
    r4ss::SS_writedat(ss3.dat, dat_file_out, overwrite = TRUE, verbose = FALSE)
  }
  invisible(list(ctl_out = ss3.ctl, dat_out = ss3.dat))
}

#' Add short time varying parameter lines. At time of writing, this method will
#' work for MG, selectivity, and catchability time varying, but not for SR
#' @param string The code representing the section the parameter is from.
#' @param tab As created in [change_tv()].
#' @param ctl_string The code as called in the .ss_new comment for time varying.
#' @param ss3.ctl A ss control file that has been read in using [readLines()].
#' @return A modified version of ss3.ctl (a vector of strings), containing the
#'   new parameter line
add_tv_parlines <- function(string, tab, ctl_string, ss3.ctl) {
  if (string %in% tab$lab) {
    line_num <- grep(paste0("#_no timevary ", ctl_string), ss3.ctl)
    n_pars <- tab[tab$lab == string, "Freq"]
    # TODO: may want to add labels?
    if (n_pars > 0) {
      lines <- paste0("# timevary ", ctl_string)
      lines <- c(lines, rep(paste0("-2 2 1 0 99 0 -5 # TV_par_line for ", ctl_string), times = n_pars))
      ss3.ctl <- append(ss3.ctl, lines, after = line_num)
      ss3.ctl <- ss3.ctl[-line_num] # b/c don't want to say no timevary anymore.
    }
  }
  ss3.ctl
}
