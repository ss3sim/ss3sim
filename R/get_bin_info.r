#' Get the parameter values for change_bin
#'
#' This function organizes arguments for other functions needed by 
#' \code{change_bin}.
#'

get_bin_info <- function() {
    lcomp <- ifelse(exists("lcomp_params") == TRUE, TRUE, FALSE)
      if(lcomp) {lcomp <- lcomp_params}
    acomp <- ifelse(exists("agecomp_params") == TRUE, TRUE, FALSE)
      if(acomp) {acomp <- agecomp_params}
    ccomp <- ifelse(exists("calcomp_params") == TRUE, TRUE, FALSE)
      if(ccomp) {ccomp <- calcomp_params}
    mla <- ifelse(exists("mla_params") == TRUE, TRUE, FALSE)
      if(mla) {mla <- mla_params}    
    mwa <- ifelse(exists("mwa_params") == TRUE, TRUE, FALSE)
      if(mwa) {mwa <- mwa_params}
    waa <- ifelse(exists("waa_params") == TRUE, TRUE, FALSE)
      if(waa) {waa <- waa_params}
    data <- list("lcomp" = lcomp, "acomp" = acomp, "ccomp" = ccomp,
                  "mla" = mla, "mwa" = mwa, "waa" = waa)
    data <- data[!sapply(data, is.null)]
    data <- data[!sapply(data, function(x) (x[[1]] == FALSE)[1])]
    data <- as.data.frame(do.call("rbind", 
        lapply(data, function(x) {
            cbind(do.call("c", x$years), 
              unlist(mapply(rep, x$fleets, each = sapply(x$years, length))),
              type = eval.parent(quote(names(X)))[substitute(x)[[3]]])
        })))
    colnames(data) <- c("year", "fleet", "type")
    data$year <- as.numeric(data$year)
    data$fleet <- as.numeric(data$fleet)
    invisible(data)
}