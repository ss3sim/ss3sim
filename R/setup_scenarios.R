#' Set up scenarios for a simulation with ss3sim
#' 
#' Set up scenarios with default arguments for a simulation with ss3sim.
#' The data frame passed by the user will likely be a truncated version
#' of all of the information needed to run a simulation, and therefore,
#' additional arguments will be added at their default values. The resulting
#' data frame is then turned into a list and passed to \code{\link{ss3sim_base}}
#' within \code{\link{run_ss3sim}}.
#'
#' @param input A data frame with one row per scenario. If \code{NULL},
#' which is the default, then a generic scenario will be run with minimal
#' data to check that everything is working.
#'
#' @author Kelli Faye Johnson
#' @export
#' 
setup_scenarios <- function(input = NULL) {
  if (is.null(input)) {
    input <- setup_scenarios_defaults()
  }
  info <- setup_scenarios_2list(setup_scenarios_fillmissing(input))
  info <- lapply(info, setup_scenarios_list_names)
  return(info)
}

#' Add missing arguments needed to run scenarios
#' 
#' Add columns for missing arguments that are needed to run scenarios
#' and that can be set to some default value. E.g., the operating model
#' can be the default operating model in the package but users must set
#' the number of age-composition samples that they want because users
#' might not want to sample any ages so we don't want to set everything
#' to a default value.
#' 
#' @param dataframe A data frame input by the user specifying the scenario
#' structure for the simulation.
#' @return A data frame with potentially more columns than what was provided.
setup_scenarios_fillmissing <- function(dataframe) {
  musthavecols <- data.frame(
    om = system.file("extdata", "models", "cod-om", package = "ss3sim"),
    # todo: make this NULL such that the EM is created from the OM internally
    # within ss3sim, e.g., in the run_ss3sim function
    em = system.file("extdata", "models", "cod-em", package = "ss3sim")
  )

  missingcols <- !colnames(musthavecols) %in% colnames(dataframe)
  dataframe <- cbind(musthavecols[, missingcols], dataframe)
  return(dataframe)
}

#' Set up a generic scenario
#' 
#' Create a data frame of scenario inputs for a generic simulation that will
#' run within ss3sim. Users can add more arguments, but the scenario will run
#' without changing the returned value.
#'
#' @author Kelli Faye Johnson
#' @export
#' @return A data frame with the minimal information needed to run a scenario.
#'
setup_scenarios_defaults <- function() {
  data.frame(
    cf.years.1 = '26:100',
    cf.fvals.1 = 'rep(0.1052, 75)',
    si.years.2 = 'seq(62, 100, by = 2)',
    si.sds_obs.2 = 0.1,
    sl.Nsamp.1 = 50,
    sl.years.1 = '26:100',
    sl.Nsamp.2 = 100,
    sl.years.2 = 'seq(62, 100, by = 2)',
    sl.cpar = NA,
    sa.Nsamp.1 = 50,
    sa.years.1 = '26:100',
    sa.Nsamp.2 = 100,
    sa.years.2 = 'seq(62, 100, by = 2)',
    sa.cpar = NA
  )
}

setup_scenarios_2list <- function(dataframe) {
  text2obj <- function(x) {
    if (is.character(x)) {
      tryCatch(eval(parse(text = x)), error = function(e) as.character(x))
    } else {x}
  }
  text2obj.v <- function(x) {
    mapply(text2obj, x)
  }
  list2fleets <- function(get, data) {
    workwith <- data[get]
    workwith <- workwith[order(names(workwith))]
    mat <- sapply(strsplit(names(workwith), "\\."), "[", 2:3)
    if (all(is.na(mat))) return(data[[get]])
    if (NROW(mat) == 2 & all(is.na(mat[2, ]))) {
      names(workwith) <- mat[1, ]
      return(workwith)
    }
    out <- split(workwith, mat[1, ])
    out$fleets <- type.convert(unique(mat[2, !is.na(mat[2, ])]), as.is = TRUE)
    return(out)
  }
  list2fleets.v <- function(x) {
    lnamessep <- strsplit(names(x), "\\.")
    lnamesdf <- sapply(lnamessep, "[",
      1:max(sapply(lnamessep, length)))
    tapply(seq_along(x), lnamesdf[1, ], list2fleets, data = x)
  }
  listin <- apply(dataframe, 1, text2obj.v)
  listout <- lapply(listin, list2fleets.v)
  return(listout)
}

# Need to find a way to name the elements of the case file list so that it
# matches the names that I created here.
setup_scenarios_list_names <- function(x) {

  names(x) <- gsub("^([eo])m$", "\\1m_dir", names(x))

  names(x) <- gsub("^cf$", "f_params", names(x))
  names(x) <- gsub("^si$", "index_params", names(x))

  names(x) <- gsub("^sa$", "agecomp_params", names(x))
  names(x) <- gsub("^sc$", "calcomp_params", names(x))
  names(x) <- gsub("^sl$", "lcomp_params", names(x))
  names(x) <- gsub("^sm$", "mlacomp_params", names(x))
  names(x) <- gsub("^sw$", "wtatage_params", names(x))

  names(x) <- gsub("^wc$", "weight_comps_params", names(x))
  # todo(feature): weight the index
  names(x) <- gsub("^wi$", "weight_index", names(x))

  names(x) <- gsub("^cb$", "em_binning_params", names(x))
  names(x) <- gsub("^cd$", "data_params", names(x))
  names(x) <- gsub("^ce$", "estim_params", names(x))
  names(x) <- gsub("^co$", "operat_params", names(x))
  names(x) <- gsub("^ct$", "tv_params", names(x))
  names(x) <- gsub("^cr$", "retro_params", names(x))

  return(x)
}

setup_scenarios_name <- function(check = FALSE) {
  makename <- function() {
    format(Sys.time(), "s%m%d%H%M%S")
  }
  dt <- makename()
  if (check) {
    while(file.exists(dt)) {
      dt <- makename()
    }
  }
  return(dt)
}
