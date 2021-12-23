#' Control file for the cod operating model
#'
#' A list of controls returned from [r4ss::SS_readctl()] for the
#' North Sea cod operating model.
#' The input file is stored in `extdata/models`.
#'
#' @format A list with many items, some of which are highlighted below:
#' \describe{
#'   \item{fleetnames}{a vector of names for the fleets}
#'   \item{MG_parms}{natural mortality and growth parameters}
#'   \item{SR_parms}{stock-recruitment relationship parameters}
#'   ...
#' }
#' @source North Sea cod
#' (*Gadus morhua*; Richard D. Methot, Jr., NMFS, NOAA, pers. comm.)
#' @seealso
#' * [r4ss::SS_readctl()]
#' * [r4ss::SS_readdat()]
"codomctl"

#' Data for the cod operating model
#'
#' A list of data returned from [r4ss::SS_readdat()] for the
#' North Sea cod operating model.
#' The input file is stored in `extdata/models`.
#'
#' @format A list with many items, some of which are highlighted below:
#' \describe{
#'   \item{catch}{data frame of catches by year, fleet, and season}
#'   \item{CPUE}{catch-per-unit-effort data}
#'   \item{lencomp}{length-composition data}
#'   ...
#' }
#' @source North Sea cod
#' (*Gadus morhua*; Richard D. Methot, Jr., NMFS, NOAA, pers. comm.)
#' @seealso [r4ss::SS_readdat()]
"codomdat"

#' Control file for the cod estimation method
#'
#' A list of controls returned from [r4ss::SS_readctl()] for the
#' North Sea cod operating model.
#' The input file is stored in `extdata/models`.
#'
#' @format A list with many items, some of which are highlighted below:
#' \describe{
#'   \item{fleetnames}{a vector of names for the fleets}
#'   \item{MG_parms}{natural mortality and growth parameters}
#'   \item{SR_parms}{stock-recruitment relationship parameters}
#'   ...
#' }
#' @source North Sea cod
#' (*Gadus morhua*; Richard D. Methot, Jr., NMFS, NOAA, pers. comm.)
#' @seealso
#' * [r4ss::SS_readctl()]
#' * [r4ss::SS_readdat()]
"codomctl"
