
library(ggplot2)
library(plyr)
library(lubridate)
library(ss3sim)
calculate_runtime <- function(StartTime, EndTime){
    ## Quick function to take the StartTime and EndTime stampe from the
    ## Report.sso file and calculate how many minutes it took to run.
    start <- data.frame(do.call(rbind,strsplit(x=as.character(StartTime),
                                              split=" ", fixed=T))[,-(1:2)])
    end <- data.frame(do.call(rbind,strsplit(x=as.character(EndTime),
                                              split=" ", fixed=T))[,-(1:2)])
    names(start) <- names(end) <- c("month","day","time", "year")
    start.date <- ymd_hms(with(start, paste(year, month, day, time, sep="-")))
    end.date <- ymd_hms(with(end, paste(year, month, day, time, sep="-")))
    run.mins <- as.vector(end.date-start.date)/60
    return(run.mins)
}
## Boxplot with a GUI for plotting results
my.boxplot <- function(x,y,horiz="species", vert=".", vert2=NULL,
                       relative.error=F, axes.free=TRUE){
    g <- ggplot(data=scalars)
    if(relative.error){
        g <- g+ylim(-1,1)+ylab(paste("relative error for:", y))
        g <- g+geom_hline(yintercept=0, col="red")
    }
    if(is.null(vert2) | vert == ".") {
        form <- as.formula(paste(horiz, "~",vert))
    } else {
        form <- as.formula(paste(horiz, "~",vert,"+",vert2))
    }
    g+geom_boxplot(aes_string(x=x,y=y)) + facet_grid(form,
                       scales=ifelse(axes.free, "free", "fixed"))
}

scalars <- read.csv("final_results_scalar.csv")
ts <- read.csv("final_results_ts.csv")

scalars <- transform(scalars, uniqueID= paste0(scenario, "-",replicate),
                     runtime=calculate_runtime(StartTime, EndTime))

scalars <- transform(scalars,
  SSB_MSY=(SSB_MSY_em-SSB_MSY_om)/SSB_MSY_om)

g <- ggplot(scalars)
g <- g+geom_boxplot(aes(x=F,y=SSB_MSY))+facet_grid(E~D)
ggsave("ssb-msy-box.png", width = 5, height = 5, dpi = 100)

ts <- transform(ts, SpawnBio=(SpawnBio_em-SpawnBio_om)/SpawnBio_om)
g <- ggplot(ts, aes(x=year))+ ylab("Relative bias in biomass") + xlab("Year")
g <- g+geom_jitter(aes(y=SpawnBio, group=replicate), size=.3, alpha=.7)+
    geom_smooth(method="loess",aes(y=SpawnBio), color="red") +
    facet_grid(E~D)+
    geom_hline(yintercept = 0, lty = 2)
ggsave("rel-bias-biomass-ts.png", width = 5, height = 5, dpi = 100)
