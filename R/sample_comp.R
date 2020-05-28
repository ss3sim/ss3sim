#' Sample composition data from expected values
#'
#' Apply the multinomial or Dirichlet distribution to sample
#' composition data, creating a data frame that mimics
#' observed composition data.
#'
#' Sample size, i.e., 'Nsamp', is used as a measure of precision,
#' where higher sample sizes lead to simulated samples that more accurately
#' represent the truth provided in \code{data}.
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
#'
#' @author Kelli Faye Johnson
#' @return A data frame of observed composition data.
#'
sample_comp <- function(data, Nsamp, fleets, years, ESS = NULL, cpar = 1) {

  #### Perform input checks
  if (is.null(fleets)) return(data[0, ])
  if (is.null(Nsamp)) return(data[0, ])
  if (is.null(cpar)) cpar <- NA
  Nfleets <- length(fleets)
  if (length(Nsamp) == 1 & Nfleets > 1) Nsamp <- rep(Nsamp, Nfleets)
  if (length(years) == 1 & Nfleets > 1) years <- rep(years, Nfleets)
  if (length(years) != Nfleets) stop("Need >=1 year per fleet in years")

  # Use input sample size if ESS=NULL unless using Dirichlet
  useESS <- TRUE
  if (is.null(ESS)) {
    ESS <- Nsamp
    useESS <- FALSE
  }

  cpar <- unlist(cpar, use.names = FALSE)
  new <- do.call("rbind", lapply(1:Nfleets, function(x) {
    data.frame(
    "FltSvy" = unlist(fleets[x], use.names = FALSE),
    "Nsamp" = unlist(Nsamp[x], use.names = FALSE),
    "Yr" = unlist(years[x], use.names = FALSE),
    "cpar" = ifelse(length(cpar) > 1, cpar[x], cpar),
    "ESS" = unlist(ESS[x], use.names = FALSE))
  }))

  if (length(Nsamp) != Nfleets)
    stop("Nsamp needs to be the same length as fleets")
  if (length(ESS) != Nfleets)
    stop("ESS needs to be the same length as fleets")
  if (class(years) != "list" | length(years) != Nfleets)
    stop("years needs to be a list of same length as fleets")

  #### Resample the data
  # Loop through each row; resample depending on Nsamp and cpar
  all <- merge(data, new, sort = FALSE,
    by = c("Yr", "FltSvy"), suffixes = c(".x", ".y"))
  cols <- (which(colnames(all) == "Nsamp.x") + 1):
    (grep("\\.y", colnames(all))[1] - 1)
  for (i in 1:nrow(all)) {
    if (is.na(all[i, "cpar"])) { # Multinomial sampling
      all[i, cols] <- rmultinom(1,
        size = all[i, "Nsamp.y"],
        prob = all[i, cols] / sum(all[i, cols]))
    } else { # Dirichlet sampling
      lambda <- all[i, "Nsamp.y"]/all[i, "cpar"]^2 - 1
      if (lambda < 0) stop("Invalid lambda Dirichlet parameter of ", lambda)
      all[i, cols] <- gtools::rdirichlet(1,
        unlist((all[i, cols] / sum(all[i, cols])) * lambda))
      # replace Nsamp with effective sample size
      all[i, "Nsamp.y"] <- all[i, "Nsamp.y"]/all[i, "cpar"]^2
    }
  }
  all <- all[, !colnames(all) == "Nsamp.x"]
  colnames(all) <- gsub("Nsamp\\.y", "Nsamp", colnames(all))
  if (useESS) all[, "Nsamp"] <- all[, "ESS"]
  all <- all[, colnames(data)]
  return(all)
}
