#' Add a q setup line into an SS control file
#'
#' This function adds a q setup line to an SS 3.30 control file
#' @param ctl.in An SS control file name to read in.
#' @param ctl.out The SS control file to read out.
#' @param overwrite Logical. Overwrite an existing file with the same name as
#'   \code{ctl.out}?
#' @param q a dataframe containing the q parameter lines to add.
#' @return A modified SS control file.
#' @author Kelli Johnson
add_CPUE <- function(ctl.in, ctl.out = NULL, overwrite = FALSE,
	q = data.frame(
		"fleet" = 3, "link" = 1, "link_info" = 0, "extra_se" = 0, "biasadj" = 0, "float" = 0,
		"LO" = -20, "HI" = 20, "INIT" = 0, "PRIOR" = 0, "PR_SD" = 99,
		"PR_type" = 0, "PHASE" = 1, "env_var" = 0, "use_dev" = 0,
		"dev_mnyr" = 0,  "dev_mxyr" = 0, "dev_PH" = 0,
		"Block" = 0, "Blk_Fxn" = 0, "name" = NULL)) {

	ctl <- readLines(ctl.in)

	startline <- findspot("Q_setup", ctl, gopast = "#")
	if (is.null(q[, "name"])) q[, "name"] <- paste0("#LnQ_base_Survey(", q[, "fleet"], ")")
  if (substr(trimws(q[, "name"]), 1, 1) != "#") q[, "name"] <- paste0("#", q[, "name"])
  Q_setup <- apply(q[, c("fleet", "link", "link_info", "extra_se", "biasadj", "float", "name")],
  	1, paste, collapse = " ")
  ctl <- append(x = ctl,
  	values = Q_setup,
	  after = startline)

  endline <- findspot("Q_parms", ctl, goto = "#_no")
  Q_parms <- apply(q[, c("LO", "HI", "INIT", "PRIOR", "PR_SD",
		"PR_type", "PHASE", "env_var", "use_dev", "dev_mnyr",
		"dev_mxyr", "dev_PH", "Block", "Blk_Fxn", "name")],
  	1, paste, collapse = " ")
  ctl <- append(x = ctl,
  	values = Q_parms,
	  after = endline - 1)
  if (!is.null(ctl.out)) {
  	write <- TRUE
  	if (file.exists(ctl.out) & !overwrite) write <- FALSE
  	if (write) writeLines(text = ctl, con = ctl.out)
  }
  invisible(ctl)
}

findspot <- function(string, lines, gopast = NULL, goto = NULL) {
	searchfor <- NULL
	if (!is.null(gopast)) searchfor <- gopast
	if (!is.null(goto)) {
		searchfor <- paste(sapply(strsplit(goto, "")[[1]],
		function(x) paste0("[^", x, "]")), collapse = "")
	}
	if (is.null(searchfor)) stop("gopast or goto must be specified.")

	loc <- grep(string, lines)
	while(grepl(searchfor,
		substring(trimws(lines[loc]), 1, nchar(searchfor)))) {
		loc <- loc + 1
	}
	if (!is.null(goto)) loc <- loc - 1
	return(loc)
}

#' Remove a q setup line into an SS control file
#'
#' This function removesa q setup line from a SS 3.30 control file
#' @param string A string with the fleetname to remove.
#' @param ctl.in An SS control file name to read in.
#' @param ctl.out The SS control file to read out.
#' @param dat.in An SS data file name to read in.
#' @param dat.out An SS data file name to read out.
#' @param overwrite Logical. Overwrite an existing file with the same name as
#'   \code{ctl.out} or \code{data.out}?
#' @return A modified SS control file.
#' @author Kelli Johnson
remove_CPUE <- function(string,
	ctl.in, ctl.out,
	dat.in, dat.out,
	overwrite = FALSE) {

	# ctl file
	ctl <- readLines(ctl.in)
	line <- findspot("Q_setup", ctl, gopast = "#")
  while(!grepl(string, ctl[line])) line <- line + 1
  ctl <- ctl[-line]
  line <- findspot("Q_parms", ctl, gopast = "#")
  while(!grepl(string, ctl[line])) line <- line + 1
  ctl <- ctl[-line]
  if (!is.null(ctl.out)) {
  	write <- TRUE
  	if (file.exists(ctl.out) & !overwrite) write <- FALSE
  	if (write) writeLines(text = ctl, con = ctl.out)
  }

  # dat file
  dat <- r4ss::SS_readdat(dat.in, verbose = FALSE)
  fleetnum <- grep(string, dat$fleetnames)
  dat$CPUE <- dat$CPUE[dat$CPUE$index != fleetnum, ]
  r4ss::SS_writedat(dat, dat.out,
  	verbose = FALSE, overwrite = overwrite)
}

#' Remove a q setup line into an SS control file only
#'
#' This function removesa q setup line from a SS 3.30 control file
#' @param string A string with the fleetname to remove.
#' @param ctl.in An SS control file name to read in.
#' @param ctl.out The SS control file to read out.
#' @param overwrite Logical. Overwrite an existing file with the same name as
#'   \code{ctl.out} or \code{data.out}?
#' @return A modified SS control file.
#' @author Kelli Johnson
remove_q_ctl <- function(string,
                        ctl.in, ctl.out,
                        overwrite = FALSE) {

  # ctl file
  ctl <- readLines(ctl.in)
  line <- findspot("Q_setup", ctl, gopast = "#")
  while(!grepl(string, ctl[line])) line <- line + 1
  ctl <- ctl[-line]
  line <- findspot("Q_parms", ctl, gopast = "#")
  while(!grepl(string, ctl[line])) line <- line + 1
  ctl <- ctl[-line]
  if (!is.null(ctl.out)) {
    write <- TRUE
    if (file.exists(ctl.out) & !overwrite) write <- FALSE
    if (write) writeLines(text = ctl, con = ctl.out)
  }
}
