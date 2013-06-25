#' Extract SS3 simulation output
#'
#' This high level function extracts results from SS3 model runs. Give it a
#' directory which contains directories for different "scenario" runs,
#' within which are replicates and potentially bias adjustment runs. It
#' writes two data.frames to file: one for single scalar values (e.g.
#' MSY) and a second that contains output for each year of the same model 
#' (timeseries, e.g. biomass(year)). These can always be joined later.
#'
#' @param directory The directory which contains scenario folders with
#' results.
#' @param files.overwrite A switch to determine if existing files should be
#' overwritten, useful for testing purposes or if new replicates are run.
#' @export
#' @author Cole Monnahan
#' @examples \dontrun{
#' ## Put this R script in a folder which contains the Scenario folders, then run
#' ## the code below.
#' 
#' ## Exploring ss3 model results
#' library(ggplot2)
#' devtools::install_github("ss3sim", username="seananderson")
#' library(ss3sim)
#' 
#' ## This function reads in results for all runs in a particular directory
#' get_results_all(files.overwrite=F)
#' 
#' ## Rread in the final results produced by above function
#' scalars <- read.csv("final_resuls_scalar.csv")
#' ts <- read.csv("final_resuls_ts.csv")
#' 
#' ## NOTE: For my case I had run different F cases (F0,F1, F2) for the base
#' ## case. THus below I've grouped by F. You might want to group by something
#' ## else, such as M, D, E, etc. depending on what you've run.
#' 
#' ## Check convergence
#' g <- ggplot(scalars)
#' round(with(scalars, tapply(max_grad, species, mean)),3)
#' round(with(scalars, tapply(max_grad, species, median)),3)
#' ## Plot w/ free ylim to see differences
#' g+geom_boxplot(aes(F,max_grad))+facet_grid(species~., scales="free")
#' 
#' ## Calculate and plot a metric, e.g. relative error of SSB_MSY
#' scalars <- transform(scalars,
#'                        SSB_MSY=(SSB_MSY_em-SSB_MSY_om)/SSB_MSY_om)
#' g <- ggplot(scalars)
#' g+geom_boxplot(aes(x=F,y=SSB_MSY))+facet_grid(species~.)
#' 
#' ## steepness
#' scalars <- transform(scalars,
#'                        SR_BH_steep=(SR_BH_steep_om-SR_BH_steep_em)/SR_BH_steep_om)
#' g <- ggplot(scalars)
#' g+geom_boxplot(aes(x=F,y=SR_BH_steep))+facet_grid(species~.)
#' 
#' ## SSB unfished
#' scalars <- transform(scalars,
#'                        SSB_Unfished=(SSB_Unfished_om-SSB_Unfished_em)/SSB_Unfished_om)
#' g <- ggplot(scalars)
#' g+geom_boxplot(aes(x=F,y=SSB_Unfished))+facet_grid(species~.)
#' 
#' ##  log(R0)
#' scalars <- transform(scalars,
#'                        SR_LN_R0=(SR_LN_R0_om-SR_LN_R0_em)/SR_LN_R0_om)
#' g <- ggplot(scalars)
#' g+geom_boxplot(aes(F,SR_LN_R0))+facet_grid(species~.)
#' 
#' ## Make some timeseries plots
#' ##  Plot error in relative biomass by year
#' ts <- transform(ts, SpawnBio=(SpawnBio_em-SpawnBio_om)/SpawnBio_om)
#' g <- ggplot(ts, aes(x=year))+ ylab("Relative bias in biomass") + xlab("Year")
#' g+geom_jitter(aes(y=SpawnBio, group=replicate), size=.1, alpha=.3)+
#'     geom_smooth(method="loess",aes(y=SpawnBio), color="red") +
#'     facet_grid(species~., )+
#'     geom_hline(yintercept = 0, lty = 2)
#' 
#' ## Look at recruitment
#' ts <- transform(ts, Recruit_0=(Recruit_0_em-Recruit_0_om)/Recruit_0_om)
#' g <- ggplot(ts, aes(x=year))+ ylab("Relative bias in recruitment") + xlab("Year")
#' g+geom_jitter(aes(y=Recruit_0), size=.1, alpha=.3)+
#'     geom_smooth(method="loess",aes(y=Recruit_0), color="red") +
#'     facet_grid(species~., )+
#'     geom_hline(yintercept = 0, lty = 2)
#' }

get_results_all <- function(directory=getwd(), files.overwrite=FALSE){

    ## Get unique scenarios that exist in the folder. Might be other random
    ## stuff in the folder so be careful to extract only scenario folders.
    all.dirs <- list.dirs(path=directory, full.names=F, recursive=F)
    ## To select scenarios that has at least 2 files (1 iteration folder and 1
    ## bias folder) i.e to only examine cases that passed the convergence test
    nb.iter <- sapply(1:length(all.dirs), function(x) length(list.files(all.dirs[x])))
    select.dirs <- all.dirs[which(nb.iter>1)]
    temp.dirs <- sapply(1:length(select.dirs), function(i) {
        x <- unlist(strsplit(select.dirs[i], split="/"))
        return(x[length(x)])
    })
    scenarios <- temp.dirs[substr_r(temp.dirs,4) %in% c("-cod", "-fla","-sar")]
    if(length(scenarios)==0) stop(print(paste("Error: No scenarios found in:",directory)))
    print(paste("Extracting results from",length(scenarios), "scenarios"))

    ## Loop through each scenario in folder
    ts.list <- scalar.list <- list()
    for(i in 1:length(scenarios)){
        setwd(directory)
        scen <- scenarios[i]
        ## If the files already exist, just read them in otherwise get results
        scalar.file <- paste0(scen,"/results_scalar_",scen,".csv")
        ts.file <- paste0(scen,"/results_ts_",scen,".csv")
        ## Delete them if this is flagged on
        if( files.overwrite){
            if(file.exists(scalar.file)) file.remove(scalar.file)
            if(file.exists(ts.file)) file.remove(ts.file)
            get_results_scenario(scenario=scen, directory=directory,
                                 overwrite.files=overwrite.files)
        } else if(!file.exists(scalar.file) |  !file.exists(ts.file)){
            ## Check if still there and skip if already so, saves a lot of runtime
            get_results_scenario(scenario=scen, directory=directory,
                                 overwrite.files=overwrite.files)
        }
        scalar.list[[i]] <- read.csv(scalar.file)
        ts.list[[i]] <- read.csv(ts.file)
    }
    ## Combine all scenarios together into big files
    scalar.all <- do.call(rbind, scalar.list)
    ts.all <- do.call(rbind, ts.list)
    setwd(directory)
    write.csv(scalar.all, file="final_resuls_scalar.csv")
    write.csv(ts.all, file="final_resuls_ts.csv")
    print(paste("Final result files written to", directory))
}

#' Extract SS3 simulation results for one scenario.
#'
#' Take a path to a scenario folder with results and write the individual
#' scenario results to two data.frames in that folder. This function is
#' called by \code{\link{get_results_all}} or can be used individually for 
#' testing.
#'
#' @param scenario A folder name in the directory folder which contains
#' replicates and potentially bias adjustment runs.
#' @param directory A path to folder containing the scenario folder.
#' @param overwrite.files A switch to determine if existing files should be
#' overwritten, useful for testing purposes or if new replicates are run.
#' @export
#' @author Cole Monnahan

get_results_scenario <- function(scenario, directory=getwd(),
                                 overwrite.files=FALSE){

## get_results_scenario(scenario)

    library(r4ss)
    ## Get results for all reps within a scenario folder
    old.wd <- getwd(); on.exit(setwd(old.wd))
    setwd(directory); setwd(scenario)

    ## Stop if the files already exist or maybe delete them
    scalar.file <- paste0("results_scalar_",scenario,".csv")
    ts.file <- paste0("results_ts_",scenario,".csv")
    if(file.exists(scalar.file) | file.exists(ts.file)){
        if(overwrite.files) {
            ## Delete them and continue
            print(paste0("Files deleted for ", scenario))
            file.remove(scalar.file, ts.file)
        } else {
            ## Stop the progress
            stop(paste0("Files already exist for ", scenario," and overwrite.files=F"))
        }
    }
    ## Check for bias correction for this scenario, grab it if exists
    bias <- rep(NA,7)
    names(bias) <- c("bias1","bias2","bias3","bias4","bias5",
                     "bias.converged","bias.tried")
    if(length(grep("bias", dir()))==1){
        bias[1:5] <- unlist(read.table(file="bias/AvgBias.DAT", header=T))
        bias.file <- read.table(file="bias/AdjustBias.DAT", header=F)
        ## The ones with NAs mean it didn't converge
        bias[6] <- nrow(na.omit(bias.file))
        bias[7] <- nrow(bias.file)
    }

    ## Loop through each replicate, not including the bias folders, and get
    ## results from both models

    ## Remove the .csv files and bias folder, they are not reps, note here I'm
    ## including only reps <1000, this will need to be changed manually if there
    ## are future runs with more than this.
    reps.dirs <- dir()[as.character(1:1000) %in% dir()]
    reps.dirs <- sort(as.numeric(reps.dirs))
    if(length(reps.dirs)==0)
        stop(print(paste("Error:No replicates for scenario", scenario)))
    ## Loop through replicates and extract results using r4ss::SS_output
    for(rep in reps.dirs){
        ## print(paste0("Starting", scen, "-", rep))
        report.em <- SS_output(paste0(rep,"/em/"), covar=F, verbose=F,
                               compfile="none", forecast=F, warn=F, readwt=F,
                               printstats=F, NoCompOK=T)
        report.om <- SS_output(paste0(rep,"/om/"), covar=F, verbose=F,
                               compfile="none", forecast=F, warn=F, readwt=F,
                               printstats=F, NoCompOK=T)
        ## Get scalars from the two models
        scalar.om <- get_results_scalar(report.om)
        names(scalar.om) <- paste0(names(scalar.om),"_om")
        scalar.em <- get_results_scalar(report.em)
        names(scalar.em) <- paste0(names(scalar.em),"_em")
        ## Get timeseires from the two
        timeseries.om <- get_results_timeseries(report.om)
        names(timeseries.om) <- paste0(names(timeseries.om),"_om")
        timeseries.em <- get_results_timeseries(report.em)
        names(timeseries.em) <- paste0(names(timeseries.em),"_em")

        ## Combine them together and massage a bit
        scalar <- cbind(scalar.om, scalar.em, t(bias))
        ts <- cbind(timeseries.om, timeseries.em)
        scalar$scenario <- ts$scenario <- scenario
        scalar$replicate <- ts$replicate <- rep
        ## parse the scenarios into columns for plotting later
        scenario.scalar <-
            data.frame(do.call(rbind, strsplit(as.character(scalar$scenario),
                                               "-")))
        scenario.ts <-
            data.frame(do.call(rbind, strsplit(as.character(ts$scenario), "-")),
                       row.names=row.names(ts))
        names(scenario.scalar) <- names(scenario.ts) <-
            c("D","E","F","G","M","R", "S", "species")
        scalar <- cbind(scalar, scenario.scalar)
        ts <- cbind(ts, scenario.ts)
        ## Other calcs
        ts$year <- ts$Yr_om
        ts <- subset(ts, select=-c(Yr_om, Yr_em))
        scalar$max_grad <- scalar$max_grad_em
        scalar <- subset(scalar, select= -c(max_grad_om, max_grad_em))

        ## Write them to file in the scenario folder
        scalar.exists <- file.exists(scalar.file)
        write.table(x=scalar, file=scalar.file, append=scalar.exists,
                    col.names=!scalar.exists, row.names=FALSE, sep=",")
        ts.exists <- file.exists(ts.file)
        write.table(x=ts, file=ts.file, append=ts.exists,
                    col.names=!ts.exists, row.names=FALSE, sep=",")
    }
    ## End of loops for extracting results
    print(paste0("Result files created for ",scenario, " with ",
                 length(reps.dirs), " replicates"))
    setwd(old.wd)
}

#' Extract time series from a model run.
#'
#' Extract time series from a model run. Returns a data.frame of the
#' results (single row) which can be rbinded later.
#'
#' @param report.file An \code{SS_output} list for a model (OM or EM).
#' @export
#' @author Cole Monnahan
get_results_timeseries <- function(report.file){

    years <- with(report.file, startyr:endyr)
    xx <- subset(report.file$timeseries, select=c("Yr","SpawnBio", "Recruit_0",
                                         "F:_1"))
    xx <- xx[xx$Yr %in% years,]
    names(xx) <- gsub(":_1","", names(xx))
    ## create final data.frame
    df <- data.frame(xx, row.names=NULL )
    return(invisible(df))
}

#' Extract scalar quantities from a model run.
#'
#' Extract scalar quantities from a model run. Returns a data.frame of the
#' results (single row) which can be rbinded later.
#' @param report.file An SS_output list for a model (OM or EM).
#' @export
#' @author Cole Monnahan
get_results_scalar <- function(report.file){

    temp <- report.file$derived_quants
    SSB_MSY <-  temp[which(temp$LABEL=="SSB_MSY"),]$Value
    TotYield_MSY <-  temp[which(temp$LABEL=="TotYield_MSY"),]$Value
    SSB_Unfished <-  temp[which(temp$LABEL=="SSB_Unfished"),]$Value
    xx <- data.frame(t(report.file$parameters$Value))
    names(xx) <- report.file$parameters$Label
    yy <- subset(xx,
                 select=c("SR_LN(R0)",
                 "SR_BH_steep",
                 "SR_sigmaR",
                 "SizeSel_1P_1_Fishery" ,
                 "SizeSel_1P_2_Fishery",
                 "SizeSel_2P_1_Survey",
                 "SizeSel_2P_2_Survey"))
    max_grad <- report.file$maximum_gradient_component
    names(yy) <- gsub("\\(","_", names(yy))
    names(yy) <- gsub("\\)","", names(yy))
    df <- cbind(yy, SSB_MSY, TotYield_MSY, SSB_Unfished, max_grad)
    return(invisible(df))
}
