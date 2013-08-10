#' Calculate run time
#'
#' Internal function used by \code{get_results_scenario} to calculate the
#' runtime (in minutes) from a Report.sso file.
#'
#' @param start_time Vector of characters as read in from the r4ss report file
#' @param end_time Vector of characters as read in from the r4ss report file
#' @author Cole Monnahan

calculate_runtime <- function(start_time, end_time){
    start <- data.frame(do.call(rbind,strsplit(x=as.character(start_time),
                                              split=" ", fixed=T))[,-(1:2)])
    end <- data.frame(do.call(rbind,strsplit(x=as.character(end_time),
                                              split=" ", fixed=T))[,-(1:2)])
    names(start) <- names(end) <- c("month","day","time", "year")
    start.date <- lubridate::ymd_hms(with(start, paste(year, month, day, time, sep="-")))
    end.date <- lubridate::ymd_hms(with(end, paste(year, month, day, time, sep="-")))
    run.mins <- as.vector(end.date-start.date)/60
    return(run.mins)
}

#' Flexible function to make jittered scatter plots from a scalar data
#frame. Use it to quickly explore results by varying dimensions and $x$ and $y$
#variables to explore patterns. Intended use is to explore model convergence and
#other issues to help identify outliers or model misspecification.
#' @param data A data frame holding scalar results
#' @param x The $x$-value for all plots as a character string.
#' @param y The $y$-value for all plots as a character string.
#' @param horiz Character of a variable for which variable to use
#for grouping into horizontal rows.
#' @param vert Character of a variable for which variable to use
#for grouping into vertical columns.
#' @param vert2 Character of a variable for how to further divide the vertical
#columns
#' @param color Character of a variable for how to color the points. Useful
#variables are max_grad or other measures of convergence.
#' @param relative.error Boolean for whether the $y$ value is a relative
#error. If TRUE it centers the $y$-axis on c(-1,1) and adds a red line at $y=0$.
#' @param axes.free Boolean. If TRUE axes between different rows
# and columns (i.e. between levels of horiz, vert and vert2) will be set
#automatically. If FALSE all $x$ and $y$-axes will have a shared scale.
#' @author Cole Monnahan

#' @examples
#' Explore the error in log R0 vs the maximum gradient. Outliers may be
#' apparent
#' library(ggplot2)
#' scalars$log_max_grad <- log(scalars$max_grad)
#' plot_scalar_points(data=scalars, x = "SR_LN_R0_om", y = "SR_LN_R0_em",
#'   color = "log_max_grad", vert = "F")


plot_scalar_points <- function(data=scalars,x,y,horiz="species", vert=".",
    vert2=NULL, color="log_max_grad", relative.error=F, axes.free=TRUE){
    g <- ggplot(data=data)+ theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_jitter(aes_string(x=x, y=y, color=color), size=1) +
        scale_color_gradient(low="gray", high="red") +
            facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
    print(g)
    return(invisible(g))
}

#' The same as \code{plot_scalar_points} except it produces boxplots and
#therefore has no color parameter. Intended use is to compare performance
#metrics, particuarly relative error.
plot.scalar.boxplot <- function(data=scalars,x,y,horiz="species", vert=".",
                                vert2=NULL, relative.error=F, axes.free=TRUE){
    g <- ggplot(data=data) + theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_boxplot(aes_string(x=x,y=y), size=.2, outlier.size=1,
                        outlier.colour=rgb(0,0,0,.5)) +
        facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))
    print(g)
    return(invisible(g))
}

#' The same as \code{plot_scalar_points} except it produces boxplots across
#years for a timeseries data frame. Intended use is to compare performance
#metrics, particuarly relative error.
plot.ts.boxplot <- function(data=ts, y,horiz="species", vert=".", vert2=NULL,
                    relative.error=F, axes.free=TRUE){
    g <- ggplot(data=data, aes(x=year))+ xlab("Year") + theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_boxplot(aes_string(y=y,group="year"),
                        outlier.colour=rgb(0,0,0,.3),  size=.5,
                        outlier.size=.8) + facet_grid(form,
                        scales=ifelse(axes.free, "free", "fixed"))
    print(g)
    return(invisible(g))
}

#' The same as \code{plot_scalar_points} except it produces points across
#years for a timeseries data frame. Intended use is to explore model convergence
#and diagnostics.
plot.ts.points <- function(data=ts, y,horiz="species", vert=".", vert2=NULL,
                           color="log_max_grad", relative.error=F,
                           axes.free=TRUE){
    g <- ggplot(data=data, aes(x=year))+ xlab("Year")+ theme_bw()
    if(relative.error){
        g <- g+coord_cartesian(ylim=c(-1,1))+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g <- g+geom_jitter(aes_string(y=y,group="year", colour=color), alpha=.5, size=1)+
        facet_grid(form, scales=ifelse(axes.free, "free", "fixed"))+
            scale_color_gradient(low="gray", high="red")
    print(g)
    return(invisible(g))
}




