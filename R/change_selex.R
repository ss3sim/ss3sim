change_selex_type <- function(object, df) {
  columnname <- substitute(object)
  if (is.null(names(object))) names(object) <- rownames(df)
  colnames(df) <- tolower(colnames(df))
  df[
    unlist(lapply(names(object), grep, rownames(df))),
    as.character(columnname)
    ] <- object
   return(df)
}

change_selex_parms <- function(pattern = 17,
  fleets = c("Fishery" = 1, "Survey" = 2),
  fleetnames = names(fleets),
  type = c("age", "size"),
  ncats = NULL) {
  type <- match.arg(type)
  stopifnot(pattern %in% c(0, 17))
  out <- data.frame(check.names = FALSE,
    "LO" = -10,
    "HI" = 10,
    "INIT" = 0,
    "PRIOR" = 0,
    "PR_SD" = 0,
    "PR_type" = 0,
    "PHASE" = -99,
    "env_var&link" = 0,
    "dev_link" = 0,
    "dev_minyr" = 0,
    "dev_maxyr" = 0,
    "dev_PH" = 0,
    "Block" = 0,
    "Block_Fxn" = 0
  )

  if (pattern == 0) {
    return(out[0, ])
  }
  if (pattern %in% 17) {
    stopifnot(!is.null(ncats))
    out <- out[rep(1, ncats * length(fleets)), ]
    row.names(out) <- paste(sep = "_",
      rep(paste0("AgeSel_P_", seq(ncats)), length(fleets)),
      rep(paste0(fleetnames, "(", fleets, ")"), each = ncats))
    out[seq(1, nrow(out), by = ncats), "LO"] <- -1001
    out[seq(1, nrow(out), by = ncats), "INIT"] <- -1000
  }
  return(out)
}

change_selex <- function(ctllist, type = c("size", "age"),
  pattern, discard, male, special) {
  type <- match.arg(type, several.ok = FALSE)
  names <- paste0(type, "_selex_", c("types", "parms"))

  if (!missing(pattern)) {
    if (!is.null(names(pattern))) {
      names(pattern) <- rownames(ctllist[[names[1]]])[seq_along(pattern)]
    }
    ncats <- rep(3, length(pattern))
    # Change type table
    ctllist[[names[1]]] <- change_selex_type(object = pattern, ctllist[[names[1]]])
    # Remove all parameters related to fleets of interest
    ctllist[[names[2]]] <- ctllist[[names[2]]][
      -unlist(lapply(names(pattern), grep, rownames(ctllist[[names[2]]]))), ]
    # Add new parameters for fleets of interest
    ctllist[[names[2]]] <- rbind(ctllist[[names[2]]],
      do.call("rbind", mapply(change_selex_parms,
        pattern = pattern,
        ncats = ncats,
        fleets = unlist(lapply(names(pattern), grep, rownames(ctllist[[names[1]]]))),
        fleetnames = unlist(lapply(names(pattern), grep, rownames(ctllist[[names[1]]]), value = TRUE)),
        MoreArgs = list(type = type), SIMPLIFY = FALSE, USE.NAMES = FALSE))
    )
  }
  if (!missing(discard)) {
    if (any(discard > 4)) {
      stop("discard options are 0:4 or negative values to mirror fleets\n",
        "you provided", paste(discard, collapse = ", "))
    }
    ctllist[[names[1]]] <- change_selex_type(object = discard, ctllist[[names[1]]])
  }
  if (!missing(male)) {
    ctllist[[names[1]]] <- change_selex_type(object = male, ctllist[[names[1]]])
  }
  if (!missing(special)) {
    ctllist[[names[1]]] <- change_selex_type(object = special, ctllist[[names[1]]])
  }

  return(ctllist)
}

# change_selex(ctllist = om, patterns = c(1, 1))
