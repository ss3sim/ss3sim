#' Set up fleet-specific information
#'
#' Sometimes, users will want to pass a single input instead of fleet-specific
#' information to make things easier to keep track of for the user.
#' `get_fleet` copies this single object over to all fleets
#' for a given sampling type.
#'
#' @details
#' In the data frame that stores scenario-specific information by row,
#' columns are fleet-specific with the fleet denoted after the last full stop.
#' If this terminal full stop followed by a numerical value is not supplied,
#' then the value will be copied for all fleets.
#' For example, `sa.Nsamp.1` specifies the sample size for age-composition data
#' for fleet number one. Whereas, `sa.Nsamp` specifies the input sample-size
#' for all fleets.
#'
#' A todo list for future features is as follows:
#' * remove fleets that have NA
#' * allow for arguments rather than hardwiring arg and fleet
#' * see if sa.Nsamp and sa.Nsamp.1 can be in the same data frame and just
#' fill in the value for fleets that aren't specified; would need to fill
#' up and down I think within a group to make it work.
#' * accomodate -999 in sample function cpar arguments
#' * create add_args to fill in missing arguments across fleets
#' * implement add_args before expand fleet such that the new
#' arg would be expanded for all fleets but I only have to specify
#' the default one time
#' x <- enquo(x)
#' y <- enquo(y)
#' ggplot(data) + geom_point(aes(!!x, !!y))
#'
#' @param data A data frame of scenario information that was passed to
#' [setup_scenarios()] and as subsequently been passed to this function as a
#' long data frame rather than a wide data frame.
#'
#' @author Kelli F. Johnson
#' @export
#' @return An augmented data frame is returned in the same form as the
#' input data. The new rows correspond to parsing input arguments out
#' across all fleets that are sampled when a single input value is provided.
setup_scenarios_fleet <- function(data) {
  #### Set up
  # Create a FULL data set with missing values by
  # expanding and nesting arg and fleet for sampling args
  # Use lookup to see if they are from the beginning section
  potentiallabels <- setup_scenarios_lookup()
  fleetspecificlabels <- names(potentiallabels)[
    1:which(potentiallabels == "wtatage_params")
  ]
  beginlabel <- gsub("(.{2})\\..+", "\\1", data[["label"]])
  if (all(is.na(data$arg))) {
    return(data)
  }
  if (all(is.na(match(beginlabel, fleetspecificlabels)))) {
    return(data)
  }

  # Remove fleets that have an NA, which means they weren't sampled
  removedfleets <- data |>
    dplyr::filter(is.na(value)) |>
    dplyr::pull(fleet)
  data <- data |> dplyr::filter(!fleet %in% removedfleets)
  #### Make data
  # Create a full data set providing one argument for each fleet
  fleet <- NULL # To remove "no visible binding for global variable 'fleet'"
  newdata <- dplyr::full_join(
    by = c("arg", "fleet"),
    data,
    tidyr::expand(
      data,
      arg,
      # see https://github.com/tidyverse/tidyr/issues/971
      tidyr::nesting(fleet)
    ) |>
      # Remove the rows that aren't fleet-specific
      tidyr::drop_na(fleet)
  ) |>
    # Arrange and group so fill up works
    dplyr::arrange(arg, fleet) |>
    dplyr::group_by(arg) |>
    tidyr::fill(value, .direction = "up") |>
    dplyr::ungroup() |>
    tidyr::drop_na(fleet)

  # If no new data then return the old data
  if (NROW(newdata) == 0) {
    return(data)
  } else {
    # Make the full data set to return, including new fleet variable
    return(
      dplyr::nest_join(
        by = colnames(newdata),
        newdata,
        data
      ) |>
        dplyr::select(-data) |>
        dplyr::full_join(
          by = c("label", "arg", "value"),
          tibble::tibble(
            label = paste(beginlabel[1], "fleets", sep = "."),
            arg = "fleets",
            value = list(utils::type.convert(
              as.is = TRUE,
              data |> tidyr::drop_na(fleet) |>
                dplyr::distinct(fleet) |> dplyr::pull(fleet)
            ))
          )
        )
    )
  }
}

#' Get scenario information from a data frame of specifications
#'
#' @param df A data frame with scenarios in the rows and
#' information for function arguments in the columns.
#' See [setup_scenarios_defaults] for how to set up the data frame.
#' This data frame is used by default if you do not supply anything
#' to `df`.
#' @param returntype The class of object that you want to return.
#' ss3sim was a big fan of lists of lists until the `tidyverse` packages
#' were included. Now, data frames of list columns are preferred.
#' Eventually, `list` will be downgraded from the default and data frames
#' will be the only option as a return.
#'
#' @author Kelli F. Johnson
#' @export
#' @return Either a long data frame or a list is returned.
#' See the input argument `returntype` for more information.
#' @examples
#' defaultscenarios <- setup_scenarios()
setup_scenarios <- function(df = "default",
                            returntype = c("list", "dataframe")) {
  returntype <- match.arg(returntype)
  if (is.character(df) && df == "default") {
    df <- setup_scenarios_defaults()
  }

  if (all(!grepl("^om_*", names(df)))) {
    df[["om_dir"]] <- setup_om_dir()
  }
  if (all(!grepl("^em_*", names(df)))) {
    df[["em_dir"]] <- setup_em_dir()
  }
  scenarios <- df |>
    # Use rownames to track scenarios
    tibble::rownames_to_column() |>
    # evaluate all arguments, e.g., '1:4' to 1,2,3,4
    dplyr::mutate(dplyr::across(
      dplyr::everything(),
      ~ purrr::map(.x, text2obj)
    )) |>
    # wide to long by scenario
    tidyr::pivot_longer(
      !rowname,
      names_to = "label",
      values_to = "value"
    ) |>
    # change NULL to -999 b/c NULL has a zero length
    # dplyr::mutate(
    #   value = map(value, ~replace_x(.x, -999))
    # ) |>
    # Split name into columns
    tidyr::separate(
      col = label,
      into = c("type", "arg", "fleet"),
      sep = "\\.",
      fill = "right",
      remove = FALSE
    ) |>
    # Create one row per data type for each scenario
    dplyr::group_by(rowname, type) |>
    tidyr::nest() |>
    # Duplicate info by fleet
    dplyr::mutate(data = purrr::map(data, setup_scenarios_fleet)) |>
    # Bring everything back together as a list of lists
    tidyr::unnest(data) |>
    dplyr::ungroup() |>
    tidyr::unite(
      col = "label", type, arg, fleet,
      sep = ".", na.rm = TRUE, remove = FALSE
    ) |>
    dplyr::mutate(type = purrr::map_chr(type, ~ setup_scenarios_lookup()[.x])) |>
    dplyr::arrange(type, arg, as.numeric(fleet))
  # check for NA or NULL values
  labs_with_null_or_nas <- scenarios$label[unlist(lapply(
    scenarios$value,
    function(x) {
      isTRUE(is.null(x)) |
        isTRUE(any(is.na(x)))
    }
  ))]

  if (returntype == "dataframe") {
    return(scenarios)
  }

  # Very convoluted way to make a list of lists b/c I am not familiar
  # with dplyr and purrr, this is ugly and will eventually be removed
  # after time to just use tibbles that can be more easily accessed.
  if (returntype == "list") {
    out <- scenarios |>
      dplyr::ungroup() |>
      dplyr::mutate(value = purrr::map(value, ~ replace_x(.x))) |>
      dplyr::mutate(value = stats::setNames(value, label)) |>
      dplyr::group_by(rowname, type) |>
      tidyr::nest() |>
      dplyr::summarize(value = purrr::map(
        .x = data, ~ {
          xxx <- base::split(.x$value, .x$arg)
          if (length(xxx) == 0) {
            if (.x[["label"]] == "user_recdevs") {
              return(.x[["value"]][[1]])
            }
            return(stats::setNames(unlist(.x$value), NULL))
          }
          if (!is.null(xxx[["fleets"]])) {
            xxx[["fleets"]] <- stats::setNames(
              stats::na.omit(unlist(xxx[["fleets"]])),
              NULL
            )
          }
          return(xxx)
        }
      )) |>
      dplyr::ungroup() |>
      dplyr::mutate(value = stats::setNames(value, type)) |>
      dplyr::select(-type) |>
      dplyr::group_by(rowname) |>
      tidyr::nest() |>
      dplyr::summarize(out = purrr::lmap(data, ~ do.call(as.list, .x)))
    return(stats::setNames(out$out, out$rowname))
  }
}

#' Create a named vector to look up full names for types of arguments
setup_scenarios_lookup <- function() {
  lookuptable <- data.frame(
    # This first section could have fleet specific parameters
    # DO NOT change the first or the last, insert new ones between
    # agecomp_params and wtatage_params, preferably in alphabetical order
    c("sa", "agecomp_params"),
    c("sc", "calcomp_params"),
    c("sd", "discard_params"),
    c("cf", "f_params"),
    c("si", "index_params"),
    c("sl", "lcomp_params"),
    c("sm", "mlacomp_params"),
    c("sw", "wtatage_params"),
    # This is the second section that will not be fleet-specific
    c("wc", "weight_comps_params"),
    # todo(feature): weight the index
    c("wi", "weight_index"),
    c("em", "em_dir"),
    c("om", "em_dir"),
    c("em_dir", "em_dir"),
    c("cb", "em_binning_params"),
    c("cd", "data_params"),
    c("ce", "estim_params"),
    c("co", "operat_params"),
    c("ct", "tv_params"),
    c("cr", "retro_params"),
    c("extras", "extras")
  )
  extraargs <- names(formals(ss3sim_base))
  names(extraargs) <- extraargs
  out <- lookuptable[2, ]
  names(out) <- lookuptable[1, ]
  out <- unlist(c(
    out, extraargs[!extraargs %in% c(lookuptable[2, ], "...")]
  ))

  return(out)
}

#' Set up a generic scenario
#'
#' Create a data frame of scenario inputs for a generic simulation that will
#' run within ss3sim. Users can add more arguments, but the scenario will run
#' without changing the returned value.
#'
#' @param nscenarios The number of rows you want returned in the data frame.
#' This argument removes the need for users to call [base::rbind()] repeatedly
#' on the output when you want to have more than one scenario.
#' All rows will be identical with the default settings.
#' The default is a single row.
#' @author Kelli F. Johnson
#' @export
#' @return A data frame with the minimal information needed to run a scenario.
#' The number of rows of the data frame depends on `nscenarios`.
#'
setup_scenarios_defaults <- function(nscenarios = 1) {
  data.frame(
    cf.years.1 = "26:100",
    cf.fvals.1 = "rep(0.1052, 75)",
    si.years.2 = "seq(62, 100, by = 2)",
    si.sds_obs.2 = 0.1,
    si.seas.2 = 1,
    sl.Nsamp.1 = 50,
    sl.years.1 = "26:100",
    sl.Nsamp.2 = 100,
    sl.years.2 = "seq(62, 100, by = 2)",
    sl.cpar = "NULL",
    sa.Nsamp.1 = 50,
    sa.years.1 = "26:100",
    sa.Nsamp.2 = 100,
    sa.years.2 = "seq(62, 100, by = 2)",
    sa.cpar = "NULL",
    stringsAsFactors = FALSE
  ) |>
    dplyr::slice(rep(1:dplyr::n(), each = nscenarios))
}

#' Create a name for an unnamed scenario
#'
#' Create a name for an unnamed scenario based on [Sys.time].
#'
#' @param check A logical that enables checking for a unique name.
#' If `check = TRUE` then the function enters a loop and will generate
#' a names until it finds one that doesn't already exist.
#' This could be helpful when running scenarios in parallel.
#'
#' @return A single character value is returned.
#' The object starts with the letter `s` and is followed by [Sys.time]
#' Where, the date/time portion is `%m%d%H%M%S`, better known as
#' a two-digit month, e.g., 01; a two-digit number for the day of the month;
#' and finally a two-digit hour, then minute, then second.
#'
setup_scenarios_name <- function(check = FALSE) {
  makename <- function() {
    format(Sys.time(), "s%m%d%H%M%S")
  }
  dt <- makename()
  if (check) {
    while (file.exists(dt)) {
      dt <- makename()
    }
  }
  return(dt)
}

text2obj <- function(x) {
  if (is.character(x)) {
    tryCatch(eval(parse(text = x)), error = function(e) as.character(x))
  } else {
    x
  }
}

setup_em_dir <- function() {
  stats::setNames(
    system.file("extdata", "models", "cod-em",
      package = "ss3sim"
    ),
    "em_dir"
  )
}

setup_om_dir <- function() {
  stats::setNames(
    system.file("extdata", "models", "cod-om",
      package = "ss3sim"
    ),
    "om_dir"
  )
}
