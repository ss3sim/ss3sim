#' Plot scalar values as points.
#'
#' @template plot-functions
#' @export
#' @import ggplot2
#' @template plot-functions-x
#' @template plot-functions-color
#' @examples
#' scalar_dat$depletion <- with(scalar_dat,
#'   (depletion_om - depletion_em) / depletion_om)
#' plot_scalar_points(scalar_dat, x = "E", y = "depletion", horiz = 'D',
#'   color = "max_grad", relative.error = TRUE)
plot_scalar_points <- function(data, x, y, horiz=NULL, horiz2=NULL, vert=NULL,
  vert2=NULL, color=NULL, relative.error=FALSE, axes.free=TRUE, print=TRUE) {
    ## Verify the inputs are correct, throws informative error if not
    verify_plot_arguments(data = data, x = x, y = y, horiz = horiz,
      horiz2 = horiz2, vert = vert, vert2 = vert2, color = color,
      relative.error = relative.error, axes.free = axes.free, print = print)
    ## Build up the ggplot object
    g <- ggplot(data=data)
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    ## Use helper function to build formula for facet_grid
    form <- facet_form(horiz, horiz2, vert, vert2)
    if(is.null(color)){
        g <- g+geom_jitter(aes_string(x=x, y=y), size=1,
                           position=position_jitter(height=0))
    } else {
        g <- g+geom_jitter(aes_string(x=x, y=y, color=color), size=1,
                           position=position_jitter(height=0)) +
                               scale_color_gradient(low="black", high="red")
    }
    if(!is.null(form))
        g <- g+facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
    ## Print and return the plot object
    if(print) print(g)
    return(invisible(g))
}
#' Print scalar values as boxplots.
#'
#' @template plot-functions
#' @export
#' @import ggplot2
#' @template plot-functions-x
#' @examples
#' scalar_dat$depletion <- with(scalar_dat,
#'   (depletion_om - depletion_em) / depletion_om)
#' plot_scalar_boxplot(scalar_dat, x = "E", y = "depletion", horiz = "D",
#'   relative.error = TRUE)
plot_scalar_boxplot <- function(data, x, y, horiz=NULL, horiz2=NULL,
  vert=NULL, vert2=NULL, relative.error=FALSE, axes.free=TRUE, print=TRUE) {
    ## Verify the inputs are correct, throws informative error if not
    verify_plot_arguments(data = data, x = x, y = y, horiz = horiz,
      horiz2 = horiz2, vert = vert, vert2 = vert2, color = NULL,
      relative.error = relative.error, axes.free = axes.free, print = print)
    ## Build up the ggplot object
    g <- ggplot(data=data)
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    ## Use helper function to build formula for facet_grid
    form <- facet_form(horiz, horiz2, vert, vert2)
    g <- g+geom_boxplot(aes_string(x=x,y=y), size=.2, outlier.size=1,
                        outlier.colour=rgb(0,0,0,.5))
    if(!is.null(form))
        g <- g + facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
        if(print) print(g)
    return(invisible(g))
}
#' Plot timeseries values as boxplots.
#'
#' @template plot-functions
#' @export
#' @import ggplot2
#' @examples
#' \dontrun{
#' ts_dat$SpawnBio <- with(ts_dat, (SpawnBio_om-SpawnBio_em)/SpawnBio_om)
#' # Merge in max_grad, a performance metric, to use for color
#' ts_dat <- merge(scalar_dat[, c("ID", "max_grad")], ts_dat)
#' plot_ts_boxplot(ts_dat, y = "SpawnBio", horiz = "D", vert = "E",
#'   relative.error = TRUE)
#' }
plot_ts_boxplot <- function(data, y, horiz=NULL, horiz2=NULL, vert=NULL,
  vert2=NULL, relative.error=FALSE, axes.free=TRUE, print=TRUE) {
    ## Verify the inputs are correct, throws informative error if not
    verify_plot_arguments(data = data, x = NULL, y = y, horiz = horiz,
      horiz2 = horiz2, vert = vert, vert2 = vert2, color = NULL,
      relative.error = relative.error, axes.free = axes.free, print = print)
    ## Build up the ggplot object
    g <- ggplot(data=data, aes_string(x="year"))+ xlab("Year")
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    ## Use helper function to build formula for facet_grid
    form <- facet_form(horiz, horiz2, vert, vert2)
    g <- g+geom_boxplot(aes_string(y=y,group="year"),
                        outlier.colour=rgb(0,0,0,.3),  lwd=.3,
                        outlier.size=.8, fatten=3)
    if(!is.null(form))
        g <- g+ facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
        if(print) print(g)
    return(invisible(g))
}
#' Plot timeseries values as points.
#'
#' @template plot-functions
#' @export
#' @import ggplot2
#' @template plot-functions-color
#' @examples
#' ts_dat$SpawnBio <- with(ts_dat, (SpawnBio_om-SpawnBio_em)/SpawnBio_om)
#' # Merge in max_grad, a performance metric, to use for color
#' ts_dat <- merge(scalar_dat[, c("ID", "max_grad")], ts_dat)
#' plot_ts_points(ts_dat, y = "SpawnBio", horiz = "D", vert = "E",
#'   relative.error = TRUE, color = "max_grad")
plot_ts_points <- function(data, y, horiz=NULL, horiz2=NULL, vert=NULL,
  vert2=NULL, relative.error=FALSE, color=NULL, axes.free=TRUE, print=TRUE) {
    ## Verify the inputs are correct, throws informative error if not
    verify_plot_arguments(data = data, x = NULL, y = y, horiz = horiz,
      horiz2 = horiz2, vert = vert, vert2 = vert2, color = color,
      relative.error = relative.error, axes.free = axes.free, print = print)
    ## Build up the ggplot object
    g <- ggplot(data=data, aes_string(x="year"))+ xlab("Year")
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    form <- facet_form(horiz, horiz2, vert, vert2)
    if(is.null(color)){
        g <- g+geom_jitter(aes_string(y=y,group="year"),
                           alpha=.5, size=1, position=position_jitter(height=0))+
                  facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))

    } else {
        g <- g+geom_jitter(aes_string(y=y,group="year", colour=color),
                           alpha=.5, size=1,
                           position=position_jitter(height=0)) +
               facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))+
                   scale_color_gradient(low="black", high="red")
    }
        if(print) print(g)
    return(invisible(g))
}
#' Plot timeseries values as lines.
#'
#' @template plot-functions
#' @export
#' @import ggplot2
#' @template plot-functions-color
#' @examples
#' ts_dat$SpawnBio <- with(ts_dat, (SpawnBio_om-SpawnBio_em)/SpawnBio_om)
#' # Merge in max_grad, a performance metric, to use for color
#' ts_dat <- merge(scalar_dat[, c("ID", "max_grad")], ts_dat)
#' plot_ts_lines(ts_dat, y = "SpawnBio", horiz = "D", vert = "E",
#'   relative.error = TRUE, color = "max_grad")
plot_ts_lines <- function(data, y, horiz=NULL, horiz2=NULL, vert=NULL,
  vert2=NULL, relative.error=FALSE, color=NULL, axes.free=TRUE, print=TRUE) {
    ## Verify the inputs are correct, throws informative error if not
    verify_plot_arguments(data = data, x = NULL, y = y, horiz = horiz,
      horiz2 = horiz2, vert = vert, vert2 = vert2, color = color,
      relative.error = relative.error, axes.free = axes.free, print = print)
    ## Build up the ggplot object
    g <- ggplot(data=data, aes_string(x="year"))+ xlab("Year")
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    ## Use helper function to build formula for facet_grid
    form <- facet_form(horiz, horiz2, vert, vert2)
    if(is.null(color)){
        g <- g+geom_line(aes_string(y=y,group="ID"), alpha=.5, lwd=.5)+
                  facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))

    } else {
        g <- g+geom_line(aes_string(y=y,group="ID", color=color), alpha=.5, lwd=.5)+
                  facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))+
                   scale_color_gradient(low="black", high="red")
    }
    if(print) print(g)
    return(invisible(g))
}

#' A helper function for building a ggplot facet. Used internally by the
#' plotting functions.
#'
#' @param horiz,horiz2 A character string denoting which column to use as
#' the first (\code{horiz}) and second (\code{horiz2}) level of faceting in
#' the horizontal direction. E.g. "M" or "species". A value of NULL (default)
#' indicates no faceting.
#' @param vert,vert2 A character string denoting which column to use as
#' the first (\code{vert}) and second (\code{vert2}) level of faceting in
#' the vertical direction. E.g. "M" or "species". A value of NULL (default)
#' indicates no faceting.
#' @author Cole Monnahan
#' @return A formula which can be used in \code{facet_grid}, or NULL if all
#' arguments are NULL

facet_form <- function(horiz=NULL, horiz2=NULL, vert=NULL, vert2=NULL) {
    h <- !is.null(horiz)
    h2 <- !is.null(horiz2)
    v <- !is.null(vert)
    v2 <- !is.null(vert2)
    ## All NULL means no faceting
    if( !h & !h2 & !v & !v2) return(NULL)
    ## If user provides horiz2 but not horiz1, switch them, likewise with vert
    if(!h & h2) {
        horiz <- horiz2
        horiz2 <- NULL
        h2 <- FALSE
        h <- TRUE
    }
    if(!v & v2) {
        vert <- vert2
        vert2 <- NULL
        v2 <- FALSE
        v <- TRUE
    }
    ## Build the formula, depending on nested cases
    if(!h & !h2) {
        if(v & !v2) form <- as.formula(paste(". ~", vert))
        else form <- as.formula(paste(". ~", vert, "+", vert2))
    }
    else if(h & !h2) {
        if(!v & !v2) form <- as.formula(paste(horiz,"~ ."))
        else if(v & !v2) form <- as.formula(paste(horiz,"~", vert))
        else form <- as.formula(paste(horiz,"~", vert, "+", vert2))
    }
    else if(h & h2){
        if(!v & !v2) form <- as.formula(paste(horiz,"+", horiz2, "~ ."))
        else if(v & !v2) form <- as.formula(paste(horiz, "+", horiz2, "~", vert))
        else form <- as.formula(paste(horiz,"+", horiz2, "~", vert, "+", vert2))
    }
    else stop("Incompatible horiz and vert arguments")
    return(form)
}

#' A helper function to check the correct input for the plotting functions.
#'
#' @template plot-functions
#' @template plot-functions-color
#' @template plot-functions-x
#' @return Nothing is returned; an informative error is throw if an
#' argument is invalid.
verify_plot_arguments <- function(data, x, y, horiz, horiz2, vert, vert2,
  color, relative.error, axes.free, print) {
    if(!is.data.frame(data))
        stop("data must be data.frame")
    else if(nrow(data)<2)
        stop("data has too few rows")
    ## x is a special case since the timeseries objects don't let the user
    ## specify it
    if(!is.null(x)){
        if(!is.character(x) | !x %in% names(data))
            stop("x must be character matching column in data")
    }
    if(!is.character(y) | !y %in% names(data))
        stop("y must be character matching column in data")
    if(!is.null(horiz)){
        if(!is.character(horiz) | !horiz %in% names(data))
            stop("horiz must be character matching column in data")
    }
    if(!is.null(horiz2)){
        if(!is.character(horiz2) | !horiz2 %in% names(data))
            stop("horiz2 must be character matching column in data")
    }
    if(!is.null(vert)){
        if(!is.character(vert) | !vert %in% names(data))
            stop("vert must be character matching column in data")
    }
    if(!is.null(vert2)){
        if(!is.character(vert2) | !vert2 %in% names(data))
            stop("vert2 must be character matching column in data")
    }
    if(!is.null(color)){
        if(!is.character(color) | !color %in% names(data))
            stop("color must be character matching column in data")
    }
    stopifnot(is.logical(relative.error))
    stopifnot(is.logical(axes.free))
    stopifnot(is.logical(print))
    ## No need to return anything, throws an error if something wrong
}
