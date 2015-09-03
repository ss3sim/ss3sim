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
#' @param rpt_file_in Input SS3 report file
#' @author Kotaro Ono, Carey McGilliard, and Kelli Johnson
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
#' @export
#'
#' @examples
#' \dontrun{
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
#' # Run SS3 to create control.ss_new and Report.sso:
#' system("ss3_24o_safe starter.ss -noest")
#'
#' change_tv(change_tv_list = list("NatM_p_1_Fem_GP_1" = c(rep(0, 20),
#'       rep(.1, 11)), "SR_BH_steep"=rnorm(31, 0, 0.05)), ctl_file_in =
#'   "control.ss_new", ctl_file_out = "example.ctl", dat_file_in =
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
  par_file_in = "ss3.par", par_file_out = "ss3.par",
  str_file_in = "starter.ss", str_file_out = "starter.ss",
  rpt_file_in = "Report.sso") {

  # Always use safe mode here:
  ss_bin <- "ss3_24o_safe"

  ss3.ctl    <- readLines(con = ctl_file_in)
  ss3.dat    <- readLines(con = dat_file_in)
  ss3.starter<- readLines(con = str_file_in)
  ss3.report <- readLines(con = rpt_file_in)

  year.beg <- grep("#_styr",  ss3.dat, value = TRUE )
  year.end <- grep("#_endyr", ss3.dat, value = TRUE )
  year.beg <- as.numeric(sub(" ", "", strsplit(year.beg, "#")[[1]][1]))
  year.end <- as.numeric(sub(" ", "", strsplit(year.end, "#")[[1]][1]))

# For all variables the following coding is used
   # mg = Natural mortality and growth parameters
   # sr = Stock recruit parameters
   # qs = Catchability paramaters
   # sx = Selectivity parameters

  baseom.tv <- grep("_ENV", ss3.report, value = TRUE)
  baseom.tv <- sapply(baseom.tv, function(x) {
                        temp <- strsplit(x, "_ENV")[[1]][1]
                        strsplit(temp, " ")[[1]][2]
                      })
  if(any(baseom.tv %in% names(change_tv_list) == "TRUE")) {
    stop(
"One or more of the parameters listed in change_tv is already time-varying in
the base operating model. ss3sim cannot change time-varying properties of
parameters that are already specified as time-varying.")
  }

# Divide .dat file at the environmental variable table
  # If no environmental variables create an empty table to be filled later
  dat.varnum.text <- grep("#_N_environ_variables", ss3.dat,
                          fixed = TRUE, value = TRUE)
  dat.varnum <- as.numeric(gsub('([0-9]*).*','\\1', dat.varnum.text))
  dat.varnum.counter <- dat.varnum
  dat.tbl.ch <- c(grep("#_N_environ_obs",              ss3.dat, fixed = TRUE),
                  grep("# N sizefreq methods to read", ss3.dat, fixed = TRUE))
  ss3.dat.top <- ss3.dat[1:dat.tbl.ch[1]]
  if(dat.tbl.ch[2] - dat.tbl.ch[1] == 1) {
    ss3.dat.tbl <- data.frame(array(dim=c(0,3),
                                    dimnames = list(NULL,
                                                    c("year", "variable", "value"))))
  } else {
      ss3.dat.tbl <- ss3.dat[(dat.tbl.ch[1] + 1) :
                             (dat.tbl.ch[2] - 1)]
      ss3.dat.tbl <- do.call("rbind",
                             sapply(ss3.dat.tbl,
                                   strsplit, split = " "))[,-1]
      ss3.dat.tbl <- as.data.frame(ss3.dat.tbl,
                                   stringsAsFactors = FALSE,
                                   class = c("integer", "numeric", "numeric"),
                                   rowames = NA)
      colnames(ss3.dat.tbl) <- c("year", "variable", "value")
      row.names(ss3.dat.tbl) <- NULL
    }
  ss3.dat.bottom <- ss3.dat[dat.tbl.ch[2]:length(ss3.dat)]

  fleet.names <- ss3.dat[grep("#_N_areas", ss3.dat)[1] + 1]
  fleet.names <- strsplit(fleet.names, "%")[[1]]

  divider.a <- grep("#_Spawner-Recruitment", ss3.ctl, fixed = TRUE)[1]
  divider.b <- grep("#_Q_setup", ss3.ctl, fixed = TRUE)[1]
  divider.c <- grep("selex_types", ss3.ctl, fixed = TRUE)[1]
  lab <- sapply(names(change_tv_list), function(x) {
                               val <- grep(pattern = x, x = ss3.ctl, fixed = TRUE)[1]
                               if(is.na(val)) {
                                 stop(paste(
"Could not locate the parameter", x, "in the operating model .ctl file.", "Check
that the parameter is spelled", "correctly and in the correct case.", "Have you
standardized your .ctl file", "by running it through SS and used the
control.ss_new file?"))}
                               if(val < divider.a) temp <- "mg"
                               if(val > divider.a & val < divider.b) temp <- "sr"
                               if(val > divider.b & val < divider.c) temp <- "qs"
                               if(val > divider.c) temp <- "sx"
                               if(x %in% fleet.names) temp <- "qs"
                               temp
                             })
  tab <- as.data.frame.table(table(lab))

  mg.ch <- grep("custom_MG-env_setup (0/1)",  ss3.ctl, fixed = TRUE)
  if("mg" %in% tab$lab) {
  if(subset(tab, lab == "mg", select = "Freq") > 0 ) {
    test.tmp <- regmatches(ss3.ctl[mg.ch], gregexpr("[[:digit:]]+", ss3.ctl[mg.ch]))
      test.tmp <- as.numeric(unlist(test.tmp))[1]
      if(test.tmp == 1) {
        stop(
"ss3sim does not support the use of custom environmental linkages. Instead
specify, 0 #_custom_MG-env_setup (0/1), for the environmental linkage.")
    }
    ss3.ctl[mg.ch] <- paste(0, "#custom_MG-env_setup (0/1)")
    ss3.ctl[(mg.ch + 1)] <- "-1 2 1 0 -1 99 -2  # env link specification i.e fixed to 1"
    adjust.ch <- grep("#_env/block/dev_adjust_method", ss3.ctl)[1]
    test.tmp <- ss3.ctl[adjust.ch]
      test.tmp <- as.numeric(strsplit(test.tmp, "#")[[1]][1])
      if(is.na(test.tmp)) {
        ss3.ctl[adjust.ch] <- "1 #_env/block/dev_adjust_method"
      } else {
      if(test.tmp == 2) {
        warning(
"The time-varying adjustment constraint in the natural mortality and growth
section of the given operating model specifies a logistic transformation. change_tv
implements additive environmental deviates and constraining the adjusted
parameter to the bounds of the base parameter may lead to undesired
transformations. To avoid this, either change the bounds of the base parameter
and ignore this warning or change the .ctl file to implement env, blocks,
and deviations that are not constrained by bounds. To do the later, find
2 #_env/block/dev_adjust_method
in the .ctl file and change it to
1 #_env/block/dev_adjust_method.")
      }}

  }
  }

  sx.ch <- grep("custom_sel-env_setup (0/1)", ss3.ctl, fixed = TRUE)
  if("sx" %in% tab$lab) {
  if(subset(tab, lab == "sx", select = "Freq") > 0 ) {
    test.tmp <- regmatches(ss3.ctl[sx.ch], gregexpr("[[:digit:]]+", ss3.ctl[sx.ch]))
      test.tmp <- as.numeric(unlist(test.tmp))[1]
      if(test.tmp == 1) {
        stop(
"ss3sim does not support the use of custom environmental linkages. Instead
specify, 0 #_custom_sel-env_setup (0/1), for the environmental linkage.")
    }
    ss3.ctl[sx.ch] <- paste(0, "#custom_sel-env_setup (0/1)")
    ss3.ctl[(sx.ch + 1)] <- "-1 2 1 0 -1 99 -2 # env link specification i.e fixed to 1"
    adjust.ch <- grep("#_env/block/dev_adjust_method", ss3.ctl)[2]
    test.tmp <- ss3.ctl[adjust.ch]
      test.tmp <- as.numeric(strsplit(test.tmp, "#")[[1]][1])
      if(is.na(test.tmp)) {
        ss3.ctl[adjust.ch] <- "1 #_env/block/dev_adjust_method"
      } else {
      if(test.tmp == 2) {
        warning(
"The time-varying adjustment constraint in the selectivity section of the given
operating model specifies a logistic transformation. ss3sim::change_tv implements additive
environmental deviates and constraining the adjusted parameter to the bounds of
the base parameter may lead to undesired transformations. To avoid this, either
change the bounds of the base parameter and ignore this warning or change the
givent .ctl file to implement env, blocks, and deviations that are not
constrained by bounds. To do the later find 2 #_env/block/dev_adjust_method in
the .ctl file and change it to 1 #_env/block/dev_adjust_method.")
      }}
  }
  }

  temp.data <- change_tv_list[lab == "mg" | lab == "sx"]
for(i in seq_along(temp.data)) {
  dat.varnum.counter <- dat.varnum.counter + 1
  par.ch <- grep(names(temp.data)[i], ss3.ctl, fixed = TRUE)[1]
  par.ex <- regexpr(names(temp.data)[i], ss3.ctl[par.ch])[1]
  val <- strsplit(substr(ss3.ctl[par.ch], start=1, stop=par.ex-1), " ")[[1]]
    # values might include spaces, make them NA and remove
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
    # Set the environmental link (8th element)
    # where abs(link) == variable# in environmental table in ss3.dat
    # negative links use an additive fxn of environmental variable (g)
    # value of g in year y (env(y,-g))
    # param`(y) = param + link*env(y,-g)
    val[8] <- -1 * dat.varnum.counter
    ss3.ctl[par.ch] <- paste(c(val, "#",  names(temp.data)[i]), collapse=" ")
  dat <- data.frame(year = year.beg:year.end,
                    variable = dat.varnum.counter,
                    value = temp.data[i])
  names(dat) <- c("year", "variable", "value")
    ss3.dat.tbl <- rbind(ss3.dat.tbl, dat)
}

temp.data <- change_tv_list[lab == "sr"]
  if(length(temp.data) > 0) {
    sr.ch <- grep("#_SR_env_link", ss3.ctl, fixed = TRUE)
    sr.base <- as.numeric(gsub('([0-9]*).*','\\1',ss3.ctl[sr.ch+1]))
    type <- ifelse(grepl("R0", names(temp.data), ignore.case = TRUE) == 1,
                   "virgin",
                   ifelse(grepl("steep", names(temp.data), ignore.case = TRUE) == 1,
                   "steep",
                   ifelse(grepl("dev", names(temp.data), ignore.case = TRUE) == 1,
                   "devs", "NA")))
    if(type=="NA") {
      stop("Did not recognize the name for the stock recruit parameter
            as recruitment deviations, virgin recruitment, or steepness,
            please rename and rerun the scenario")
    }
    if(type == "devs") {
      warning("ss3sim uses annual recruitment deviations and may not work
               with a model that ties recruitment deviations to environmental
               covariates. If you need to compare the environment to annual
               recruitment deviations, the preferred option is to transform the
               environmental variable into an age 0 pre-recruit survey. See
               page 55 of the SS3 version 3.24f manual for more information.")
    }
    if(length(temp.data) > 1 ) {
      stop("Currently SS3 only allows one stock recruit paramater at a
            time, R0 or steepness, to vary with an environmental
            covariate.")
    }

    if(sr.base > 0) {
      stop("Currently SS3 does not allow environmental deviations
            for multiple stock recruit parameters.
            Please remove the environmental covariate from the base operating model
            and run the scenario again.")
    }

  sr.shortline.ch <- grep("# SR_envlink", ss3.ctl, fixed = TRUE)
  ss3.ctl[sr.shortline.ch] <- "-5 5 1 0 -1 99 -3 # SR_envlink"
  dat.varnum.counter <- dat.varnum.counter + 1
  ss3.ctl[sr.ch] <- paste(dat.varnum.counter, "#_SR_env_link")

  if(length(grep("dev", names(temp.data), fixed = TRUE)) > 0) {
    ss3.ctl[sr.ch+1] <- "1 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness"
  }
  if(length(grep("R0", names(temp.data), fixed = TRUE)) > 0) {
    ss3.ctl[sr.ch+1] <- "2 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness"
  }
  if(length(grep("steep", names(temp.data), fixed = TRUE)) > 0) {
    ss3.ctl[sr.ch+1] <- "3 #_SR_env_target_0=none;1=devs;_2=R0;_3=steepness"
  }

  dat <- data.frame(year = year.beg:year.end,
                    variable = dat.varnum.counter,
                    value = temp.data)
    names(dat) <- c("year", "variable", "value")
    ss3.dat.tbl <- rbind(ss3.dat.tbl, dat)
}
  sr.parameter <- which(lab == "sr")
  if(length(sr.parameter) > 0) {
    names(change_tv_list)[sr.parameter] <- "SR_envlink"
  }

  temp.data <- change_tv_list[lab == "qs"]
  paste.into.ctl <- NULL
for(i in seq_along(temp.data)) {
  dat.varnum.counter <- dat.varnum.counter + 1
  ctl.relevant <- grep("#_Q_setup", ss3.ctl) :
                  grep("#_Cond 0 #_If q has random component", ss3.ctl)
  par.ch <- grep(names(temp.data)[i], ss3.ctl, fixed = TRUE)
  par.ch <- par.ch[which(par.ch %in% ctl.relevant)]
  par.ex <- regexpr(names(temp.data)[i], ss3.ctl[par.ch])[1]
  val <- ss3.ctl[par.ch]
  val.name <- strsplit(val, "#")[[1]][2]
  val.pars <- strsplit(val, "#")[[1]][1]
  val.pars <- strsplit(gsub(" ","",val.pars,""),"")[[1]]
  val.pars[2] <- dat.varnum.counter
  ss3.ctl[par.ch] <- paste(paste(val.pars, collapse = " "),
                           "#", val.name, sep = " ")

  names(change_tv_list)[which(names(change_tv_list) ==
                              names(temp.data)[i])] <-
                  paste0("Q_envlink_",
                        which(fleet.names == names(temp.data)[i]),
                        "_", names(temp.data)[i])

  paste.into.ctl <- c(paste.into.ctl,
                      paste("-2 2 1 0 -1 99 -5 #", names(temp.data)[i]))

  dat <- data.frame(year = year.beg:year.end,
                    variable = dat.varnum.counter,
                    value = temp.data[i])
  names(dat) <- c("year", "variable", "value")
    ss3.dat.tbl <- rbind(ss3.dat.tbl, dat)
}
  par.spec <- grep("#_Q_parms\\(if_any\\)", ss3.ctl)
  # Check to see if any Q_parms have a power function,
  # if so change par.spec to place Q_env after Q_power
  par.power<- grep("Q_power_", ss3.ctl, fixed=TRUE)
  par.power<- ifelse(length(par.power) == 0, 0, length(par.power))
  if(!is.null(paste.into.ctl)) ss3.ctl <- append(ss3.ctl,
                                                 paste.into.ctl,
                                                 (par.spec + 1 + par.power))

    ss3.dat.top[grep(" #_N_environ_variables",
                     ss3.dat.top, fixed = TRUE)] <- paste(dat.varnum.counter,
                                                          " #_N_environ_variables")
    ss3.dat.top[grep(" #_N_environ_obs",
                     ss3.dat.top, fixed = TRUE)] <- paste((year.end - year.beg + 1) * dat.varnum.counter,
                                        " #_N_environ_obs")
    ss3.dat.new=c(ss3.dat.top,
                  apply(ss3.dat.tbl, 1, paste, collapse = " "),
                  ss3.dat.bottom)
    writeLines(ss3.dat.new, con = dat_file_out)
    writeLines(ss3.ctl, con= ctl_file_out)

    #run SS with with no estimation and no hessian
    #first change starter file option to use .par to .ctl
    usepar.ch <- grep("# 0=use init values in control file; 1=use ss3.par",
                      ss3.starter, fixed=TRUE)

    ss3.starter[usepar.ch] <- "0 # 0=use init values in control file; 1=use ss3.par"
    ss3.starter[usepar.ch-2] <- dat_file_out
    ss3.starter[usepar.ch-1] <- ctl_file_out
    writeLines(ss3.starter, con = str_file_out)

    bin <- get_bin(ss_bin)

    #Call ss3 for a run that includes the environmental link
    os <- .Platform$OS.type
      if(os == "unix") {
        system(paste(ss_bin, "-noest"), ignore.stdout = TRUE)
      } else {
        system(paste(ss_bin, "-noest"), show.output.on.console = FALSE, invisible = TRUE, ignore.stdout = TRUE)
      }

    #Change starter file option back to using .par!
    ss3.starter[usepar.ch] = "1 # 0=use init values in control file; 1=use ss3.par"
    writeLines(ss3.starter, con = str_file_out)

  ss3.report <- readLines(con = rpt_file_in)
  ss3.par    <- readLines(con = par_file_in)

  env.name <- sapply(names(change_tv_list), function(x) {
                ifelse(grepl("envlink", x),
                       x,
                       paste(x, "ENV", sep = "_"))
              })
  env.parnum <- sapply(env.name, function(x) {
      temp <- grep(x, ss3.report, value = TRUE)
      temp <- strsplit(temp, " ")[[1]][1]
      as.numeric(temp)
    })
    # ensure order is same throughout:
    env.name <- sort(env.parnum)
    env.lab <- sort(lab)
    env.parnum <- sort(env.parnum)
for(q in seq_along(change_tv_list)) {
    if(env.lab[q] == "sr" | env.lab[q] == "qs") next
    if(env.lab[q] == "mg") {
      search.phrase <- paste0("# MGparm[", env.parnum[q] - 1, "]:")
      line.a <- grep(search.phrase, ss3.par, fixed = TRUE)
      add.par <- c(paste0("# MGparm[",env.parnum[q],"]:"),
                   "1.00000000000")
      ss3.par <- append(ss3.par, add.par, (line.a + 1))
          }
    if(env.lab[q] == "sx") {
      num.sx <- grep("Sel_.._", ss3.report )
      pos.sx <- grep(env.name[q], ss3.report[num.sx])
      # The above code is looking for a number, which might be in more
      # than one line, but really we just want to check that the
      # parameter number is in a line that contains the letters ENV
      if (length(pos.sx) > 1) {
        allnames <- grep(env.name[q], ss3.report[num.sx], value = TRUE)
        allchars <- strsplit(allnames, " ")
        allchars <- lapply(allchars, function(x) x[!x == ""])
        getthisone <- grep(env.name[q], sapply(allchars, "[[", 1))
        doublecheck <- grep("ENV", allnames)
        if (getthisone != doublecheck) {
          stop("The selectivity parameter cannot be indexed using\n
               the current framework. Please contact the developers\n
               and let them know, so that this can be fixed.")
        }
        pos.sx <- pos.sx[getthisone]
      }
      search.phrase <- paste0("# selparm[", pos.sx - 1, "]:")
      line.a <- grep(search.phrase, ss3.par, fixed = TRUE)
      add.par <- c(paste0("# selparm[",pos.sx,"]:"),
                   "1.00000000000")
      ss3.par <- append(ss3.par, add.par, (line.a + 1))
          } }
    if(any(env.lab == "qs")) {
      qs.relevant <- (max(grep("F_fleet", ss3.report)) + 1) :
                      (grep("Sel_", ss3.report)[1] - 1)
      qs.old  <- sapply(strsplit(ss3.report[qs.relevant], " "),
                        "[[", 3)
      # find the section in the par with the q params
      # delete them
      # put in new words and new vals
      qs.count <- seq_along(qs.old)
      qs.new <- as.vector(rbind(paste0("# Q_parm[", qs.count, "]:"),
                                qs.old))
      ss3.par  <- ss3.par[-(grep("# Q_parm[1]:", ss3.par, fixed = TRUE) :
                 (grep("# selparm[1]:", ss3.par, fixed = TRUE) - 1))]
      ss3.par <- append(ss3.par, qs.new,
                        (grep("# selparm[1]:",
                              ss3.par, fixed = TRUE) - 1))
      }
    if(any(env.lab == "sr")) {
      sr.parnum <- which(grep("SR_envlink", ss3.report) == grep("SR", ss3.report))
      ss3.par[grep(paste0("# SR_parm[", sr.parnum, "]:"),
              ss3.par, fixed = TRUE) + 1 ] <- "1.00000000000"
          }
    writeLines(ss3.par, con = par_file_out)
  }
