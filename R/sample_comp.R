#' Sample composition data from expected values
#'
#' Apply the multinomial or Dirichlet distribution to sample
#' composition data, creating a data frame that mimics
#' observed composition data.
#'
#' @details
#' Sample size, i.e., 'Nsamp', is used as a measure of precision,
#' where higher sample sizes lead to simulated samples that more accurately
#' represent the truth provided in `data`.
#'
#' @param data A data frame with informational columns followed by
#' columns of compositional data.
#' The informational columns must include columns labeled
#' 'Yr' and 'FltSvy' and end with a column labeled 'Nsamp'.
#' Columns of compositional data should follow 'Nsamp'.
#' Rows of compositional data do not need to sum to one.
#' @template Nsamp
#' @template lcomp-agecomp-index
#' @template lcomp-agecomp
#' @template sampledots
#'
#' @author Kelli F. Johnson
#' @return A data frame of observed composition data.
#'
sample_comp <- function(data,
                        Nsamp,
                        fleets,
                        years,
                        ESS = NULL,
                        cpar = 1,
                        ...) {

  #### Perform input checks
  if (is.null(fleets)) {
    return(data[0, ])
  }
  if (is.null(Nsamp)) {
    return(data[0, ])
  }
  cpar <- switch(class(cpar),
    list = ifelse(mapply(is.null, cpar), NA, unlist(cpar, recursive = FALSE)),
    numeric = cpar,
    logical = NA,
    vector = cpar,
    NULL = NA
  )

  # ESS can be (1) user input, (2) NULL -> Nsamp, (3) Dirichlet calculated ESS
  useESS <- ifelse(is.null(ESS), FALSE, TRUE)
  if (is.null(ESS)) {
    ESS <- Nsamp
  }
  if (useESS) { # in case there are NA values.
    ESS <- mapply(function(ess, nsamp) {
      if (any(is.na(ess))) {
        if (length(ess != nsamp) & length(nsamp == 1)) {
          nsamp <- rep(nsamp, length.out = length(ess))
        }
        new_ess <- mapply(function(e, n) ifelse(is.na(e), n, e),
          e = ess, n = nsamp, SIMPLIFY = FALSE
        )
        new_ess <- unlist(new_ess)
      } else {
        new_ess <- ess
      }
      new_ess
    }, ess = ESS, nsamp = Nsamp, SIMPLIFY = FALSE)
  }

  # Check for bad inputs
  lapply(
    list(years, fleets, Nsamp, ESS, cpar, ...),
    function(x, fleetN = length(fleets)) {
      if (!length(x) %in% c(1, fleetN)) {
        stop(
          call. = FALSE,
          "Bad input to ss3sim sampling function.\n",
          "There is only ", fleetN, " fleets, yet your input was a ",
          class(x), " with a length of ", length(x), ". See below:\n", x
        )
      }
    }
  )

  # Repeat short inputs
  new <- dplyr::bind_cols(tibble::tibble(FltSvy = fleets), tibble::tibble(
    Yr = years,
    newN = Nsamp, ESS = ESS, cpar = cpar, ...
  )) %>%
    dplyr::rowwise() %>%
    tidyr::unnest(dplyr::everything()) %>%
    dplyr::bind_rows()
  colnames(new) <- gsub("part", "Part", colnames(new))
  colnames(new) <- gsub("seas", "Seas", colnames(new))
  # todo: make the rename above more generic

  #### Multinomial or DM sampling based on case_when with cpar
  # Results are slightly different because of some seed thing with dplyr
  # sample_dm or sample_mn will give same values if used in loop
  # or force seed in the function
  all <- dplyr::inner_join(
    x = data,
    y = new,
    by = stats::na.omit(colnames(new)[match(colnames(data), colnames(new))])
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      comp = dplyr::case_when(
        is.na(.data[["cpar"]]) ~ list(sample_mn(
          data = dplyr::c_across(dplyr::matches("[0-9]+")),
          n = .data[["newN"]]
        )),
        is.numeric(.data[["cpar"]]) ~ list(sample_dm(
          data = dplyr::c_across(matches("[0-9]+")),
          n = .data[["newN"]], par = .data[["cpar"]]
        ))
      ),
      ncalc = dplyr::case_when(
        is.na(.data[["cpar"]]) ~ .data[["newN"]],
        is.numeric(.data[["cpar"]]) ~ .data[["newN"]] / .data[["cpar"]]^2
      )
    ) %>%
    dplyr::select(
      1:(dplyr::matches("Nsamp") - 1),
      Nsamp = .data[[ifelse(useESS, "ESS", "ncalc")]],
      .data[["comp"]]
    )
  comp <- NULL # To remove "no visible binding for global variable 'comp'"
  return(
    cbind(
      dplyr::select(all, -comp),
      do.call("rbind", all$comp)
    ) %>%
      `colnames<-`(colnames(data))
  )
}
