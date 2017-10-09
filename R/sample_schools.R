#' @title Sample length compositions from a single vector of expected values
#'  accounting for empirical aggregation patterns (or schools).
#'  
#' @description Takes a vector of probabilities, which can be extracted from the
#'  \code{lencomp} component of a \code{data.SS_new} file containing expected
#'  values, and sampleS a number of draws (or hauls) of observed length
#'  compositions, accounting for autocorrelation in hauls, using a mixture 
#'  distribution approach.
#'  
#' @details The function can be used independantly but was specifically written 
#'  to be integrated into \code{\link{sample_lcomp}}. It is advised to use the 
#'  \code{parallel_lgths} option when turning the schooling option on in 
#'  \code{\link{sample_lcomp}}.
#'  
#' @author Gwladys Lambert
#'    
#' @param probs vector of probabilities at length (length composition vector)
#' @param vec vector of length bins corresponding to the probabilities
#' @param Nhauls number of hauls to sample lengths data from
#' @param Nsamp number of length samples to take from each haul
#' @param cl_factor this parameter determines the characteristics of the
#'  clusters (or fish schools) that form the overall distribution (combined by 
#'  mixture distribution). \code{cl_factor} sets the maximum number of 
#'  clusters that can be used to fit the expected length distribution 
#'  \code{probs} and sets the mean length of these clusters. The number of 
#'  clusters is defined by \code{cl_factor}*(max(\code{vec})-min(\code{vec})).
#'  A value of \code{cl_factor}=1 means that the number of clusters will be 
#'  equal to the range of lengths, i.e. the mean of each underlying distribution 
#'  will be 1 cm apart. A value of 0.5 means that the number of clusters will be 
#'  divided by 2, i.e. mean length of each distribution every 2cm etc.
#'  Each cluster is the gamma equivalent of a normal distribution (gamma 
#'  equivalent is used to avoid drawing negative length values). 
#'  Using empirical \code{gamma_sh} and \code{gamma_ra} will overwrite the  
#'  \code{cl_factor}.
#' @param vals_at_cutoff THIS ARGUMENT SHOULD BE RENAME (LEGACY) Value used to 
#'  estimate the SD of the clusters (or fish schools), whose means are defined 
#'  by \code{cl_factor}. A value of 0.2 means that the SD of the normal 
#'  distributions will be 20 percent of the full range of lengths given 
#'  by \code{vec}. 
#'  Using \code{gamma_sh} and \code{gamma_ra} will overwrite the  
#'  \code{vals_at_cutoff}. 
#' @param gamma_sh vector of shape parameters for the gamma distributions of the 
#'  clusters. Use with \code{gamma_ra}. If used, these arguments will overwrite 
#'  \code{cl_factor} and \code{vals_at_cutoff}. Default is NULL.
#' @param gamma_ra vector of rate parameters for the gamma distributions of the 
#'  clusters. Use with \code{gamma_sh}. If used, these arguments will overwrite 
#'  \code{cl_factor} and \code{vals_at_cutoff}. Default is NULL. 
#' @param plot.schools Default is NULL. Set to TRUE to visualise the schools
#'  fitting process.
#' @param yr Specify the year of the data. This is for plotting purposes only. 
#'  Useful in the context of full simulations using \code{\link{ss3sim_base}}.
#' @return A list of 2 objects. The first one is the resulting sampled length 
#'  composition, the second one is a dataframe detailing the actual sampling, 
#'  i.e. lengths measurements on a haul by haul basis.
#'    
#' @note The shape \code{gamma_sh} and rate \code{gamma_ra} parameters can be 
#' obtained with \code{\link{schooling_pattern}} prior to using 
#' \code{\link{sample_schools}}, as in \code{\link{sample_lcomp}}.  
#' Using \code{gamma_sh} and \code{gamma_ra} allows the direct use of empirical 
#' data. \code{cl_factor} and \code{vals_at_cutoff} are an alternative to create 
#' clusters based on some functional definition of the link between mean 
#' length in the hauls and standard deviations, they do not require empirical 
#' data. Note - THERE is a warning sign, from ADMB, that can be safely ignored.
#'
#'    
#' @importFrom Hmisc %nin%
#' @import stats
#' @export
#' @family sampling functions
#'    
#' @examples
#'  d <- system.file("extdata", package = "ss3sim")
#'  f_in <- paste0(d, "/models/cod-om/codOM.dat")
#'  dat_list <- r4ss::SS_readdat(f_in, verbose = FALSE)
#'  dat_list <- change_fltname(dat_list)
#'  \dontrun{
#'  # Option to sample in schools (length aggregated fish)
#'  
#'  # getwd()
#'  probs <- as.numeric(dat_list$lencomp[dat_list$lencomp$Yr ==95 & 
#'                                   dat_list$lencomp$FltSvy ==2,-c(1:6)])
#'  vec   <- dat_list$lbin_vector
#'  ex1   <- sample_schools(probs=probs, Nhauls= 300, Nsamp=100, vec=vec,
#'                       cl_factor = 0.5, vals_at_cutoff = 0.1,
#'                       plot.schools = TRUE,  yr=95)
#'                       
#'  ### THERE IS A PROBLEM WITH THE EXTREME SIZES THAT I DID NOT HAVE
#'  ### WHEN I WAS TWEEKING THE DATA FROM NORMAL TO MULTINOMIAL -
#'  ### WILL HAVE TO LOOK INTO THIS CLOSER !!!!!!!!!!!!!!!
#'  ### SHOULD ALL THE SIZES BELOW THE MINIMUM SIZE BIN SHOULD BE DISCARDED
#'  ### RATHER THAN SUMMED UP INTO THAT BIN? ... WHAT ABOUT THE MAX SIZE?
#'  
#'  # dat file
#'  ex1a <- ex1[[1]]
#'  # hauls file
#'  ex1b <- ex1[[2]]
#'  }
#'  

sample_schools <- function(probs, Nhauls, Nsamp, vec, 
                           cl_factor = 1, vals_at_cutoff= 0.05,
                           gamma_sh = NULL, gamma_ra = NULL,
                           plot.schools = F, yr = NULL) {

  # might be needed for CMD checks, not sure
  to_return <- NULL
  
  # make sure the distribution sums up to 1
  probs <- probs/sum(probs)
 
   
  ######################################################################
  ####### FIRST SECTION - MIXTURE MODEL - FIND THE BEST COMBINATION OF #
  #######                UNDERLYING DISTRIBUTIONS                      #
  ######################################################################
  
  # If male and female - combine distributions
  # This is just for the first section where we define the clusters 
  # (or aggregations) (or school)
  # After that the sampling will be sex-based
  probs_save <- NULL
  if (length(probs) == 2*length(vec)){
    probs_save <- probs 
    probs_f  <- probs_save[1:c(length(probs_save)/2)] 
    target_f <- probs_f/sum(probs_f)
    probs_m  <- probs_save[c(length(probs_save)/2+1):length(probs_save)]
    target_m <- probs_m/sum(probs_m)
    probs    <- target_f + target_m
    probs    <- probs/sum(probs)
  }
 
  # define minimum and maximum length categories in the population to sample from
  min.len=min(as.numeric(as.character(vec)))
  max.len=max(as.numeric(as.character(vec)))
  
  # defined cluster characteristics if no use of empirical data i.e. gamma shape
  # and rate parameters are null)
  if (is.null(gamma_sh)) {
  # number of clusters (or school) (or distinct aggregations)
  cl.nb = cl_factor*(max(vec)-min(vec))
  # define the mean length of each cluster 
  # this is done by splitting the length vector at equal intervals
  cl.mean = round(seq(min.len*1.1, max.len*0.9, length.out=cl.nb),2)
  # define the sd associated with each cluster (sd is a function of the mean)
  cl.sd <- rep(vals_at_cutoff*(max.len-min.len), length(cl.mean))
  # add noise to cl.sd
  # This could be another argument in the function
  cl.sd <- cl.sd + rnorm(length(cl.sd),0,2)
  cl     <- cbind.data.frame(cl.mean,cl.sd)
  cl <- cl[cl.sd >0,]
  # Turn normal into gamma distribution to keep lengths positive
  gamma_ra = (cl.mean + sqrt(cl.mean^2 + 4 * 
                               cl.sd^2))/(2 * cl.sd^2)
  gamma_sh = 1 + cl.mean * gamma_ra
  }
    
    cl     <- cbind.data.frame(gamma_sh,gamma_ra)
    ## create list of pgammas, i.e. cumulative density function for each cluster
    # Given a number or a list pgamma computes the probability that a gamma 
    # distributed random number will be less than that number
    # so it defines the probability of a fish that belongs to a cluster to be 
    # under a certain length
    pgammas <- list()
    for (k in 1:nrow(cl)) {
      temp <- pgamma(vec,cl$gamma_sh[k],cl$gamma_ra[k])
      if (any(is.na(temp))) temp <- rep(0, each=length(temp))
      pgammas[[k]] <- temp
    }
    pgammas <- cbind.data.frame(clust=1:length(pgammas), data.frame(matrix(unlist(pgammas), ncol=length(pgammas[[1]]), byrow=T)))
   
    # plot the distributions
    if (plot.schools){
      par(mfrow=c(2,2))
      plot(function(x) dgamma(x, cl$gamma_sh[1], cl$gamma_ra[1] ), min.len, max.len,
           type="l", ylab="Probability", xlab="Length", main="AGGREGATION PATTERN", ylim=c(0,0.2))
      for (idx in c(1:length(cl$gamma_sh))){
        plot(function(x) dgamma(x, cl$gamma_sh[idx], cl$gamma_ra[idx] ), min.len, max.len, type="l", ylim=c(0,0.2), add=T,col=idx)
      }
      if(!is.null(yr)) mtext(3, text= paste("Yr =", yr), outer=T, line=-2)
    }
  
  
  ### OBJECTIVE FUNCTION TO OPTIMIZE IN MIXTURE DISTRIBUTION
    # This relies on ADMB
    idx <- 1:nrow(cl)
    pgammas_mat <- as.matrix(pgammas[,-1])
    probs_cum   <- cumsum(unlist(probs))
    wd_old      <- getwd()
    R2admb::setup_admb()
    d  <- strsplit(system.file("admb_code", "optim_schools.tpl", package = "ss3sim"),".tpl")[[1]]
    d2 <- strsplit(d,"/optim_schools")[[1]]  
    setwd(d2)
    # compile the code once and copy over for efficiency
    if (!file.exists("optim_schools.exe")) R2admb::compile_admb(d, verbose=F)
    lst <- list.files(d2)[grep("optim",list.files(d2))]
    lst <- pastef(d2,lst)
    # create a new directory for each year
    temps <- round(rnorm(1,1,100000000000)) # using tempdir() might be better here - would imply rewriting a few things below...
    dir.create(pastef(d2,temps))
    #copy files across
    setwd(pastef(d2,temps))
    file.copy(lst, to=getwd())
    #now read the tpl file in that folder and run
    newd <- strsplit(system.file(pastef("admb_code",temps), "optim_schools.tpl", package = "ss3sim"),".tpl")[[1]] #
    # run optimisation to find appropriate weight on each underlying distribution
     pars_ad <- R2admb::do_admb(fn=newd, 
                               data= list(nclust = nrow(pgammas_mat), maxlgth = ncol(pgammas_mat), pgammas_mat= pgammas_mat, probs_cum=probs_cum),
                               params = list( a =rep(0.1, each=nrow(pgammas_mat)), f= 0), admb_errors = "ignore", extra.args = "-nox -nohess") 
    setwd(wd_old)
    unlink(pastef(d2,temps), recursive=T)
    pars <- pars_ad$coefficients
    
  #plot clusters on top of expected ditribution for illustration
  cl1     <- cl
  cl1$par <- pars
  if (plot.schools){
    plot(probs ~ vec, type="l", xlab="LENGTH", ylab="Percentage", main="TARGET DISTRIBUTION") #expected_temp$lbin_vector
  }
  
  
  # nb of fish at size in each clust 
  # discretise the cluster distributions by length bin
  pval         <- NULL
  idx1         <- 1:nrow(cl)
  for (k in 1:nrow(cl)) {
    if (pars[k] >=1e-6) {
      if (is.null(gamma_sh)) {pval_temp <- round(rnorm(pars[k]*1e6, cl$cl.mean[k], cl$cl.sd[k]),0)
      } else {pval_temp <- round(rgamma(pars[k]*1e6, cl$gamma_sh[k], cl$gamma_ra[k]),0)}
      pval_temp <- as.data.frame(table(pval_temp))
      names(pval_temp) <- c("LENGTH","nbatlgth")
      if (length(pval_temp)!=0) {
        pval_temp <- cbind.data.frame(clust=k, pval_temp)
        pval <- rbind(pval, pval_temp)
      }
    }
  }
  clust_nb        <- pval
  

  
  #####################################################################
  ####### SECOND SECTION - SAMPLING - SAMPLE NHAULS AND NSAMP   ####### 
  #######                WITHIN EACH HAULS                      ####### 
  #####################################################################
  
  if (length(unique(clust_nb$clust)) > 1) {

    res8 <- clust_nb
    res8$LENGTH <- as.numeric(as.character(res8$LENGTH))
     
    # plot selected clusters
    if (plot.schools){
      plot(res8$nbatlgth ~ res8$LENGTH,
           xlab="LENGTH", ylab="Percentage", main="SCHOOLS TO SAMPLE FROM", xlim=c(min(vec),max(vec)),ylim=c(0,0.2),type="n") #
      for (i in sort(unique(res8$clust))){
        cl4    <- res8[res8$clust==i & !is.na(res8$clust),]
        cl4$Percentage <- cl4$nbatlgth/sum(cl4$nbatlgth)
        vec_df <- data.frame(LENGTH=vec)
        cl4    <- merge(cl4, vec_df, by="LENGTH", all=T)
        cl4$Percentage[is.na(cl4$Percentage)] <- 0
        points(cl4$Percentage ~ cl4$LENGTH, col=i, type="l")
      }
    }
    
    prop_cl       <- aggregate(nbatlgth ~ clust, res8, sum)
    prop_cl$props <- prop_cl$nbatlgth/sum(prop_cl$nbatlgth)

    ## Now I know exactly how many fish there are in each cluster, I can choose the site to sample using a multivariate
    
    # Take Nhauls tows and Nsamp lengths per tow
    x=Nhauls
    
    tows <- rmultinom(1,x,prop_cl$props)
    tows <- cbind.data.frame(clust=prop_cl$clust, tows)
    tows <- tows[tows$tows!=0,]
    prop_cl <- prop_cl[prop_cl$clust %in% c(tows$clust),]
    
  } else {
    prop_cl <-  cbind.data.frame(clust="999", nbatlgth= 1e6, props=1)
    tows <- cbind.data.frame(clust="999", tows= Nhauls)
    res8 <- cbind.data.frame(LENGTH= vec, clust="999", nbatlgth = round(probs*1e6))
  }
  
  # If sex split res8 must be split by sex too
  
  if (!is.null(probs_save)) {
    res8a <- res8
    res8a$LENGTH[res8a$LENGTH<=min.len] <- min.len
    res8a$LENGTH[res8a$LENGTH>=max.len] <- max.len
    res8a <- aggregate(nbatlgth ~ clust + LENGTH, res8a, sum)
    res8b <- NULL
    for (ll in sort(unique(res8a$LENGTH))){
      sub_temp <- res8a[res8a$LENGTH==ll,]
      sub_f <- target_f[which(vec==ll)]
      sub_m <- target_m[which(vec==ll)]
      # ratio female to amles in that length
      ra_f <- sub_f/(sub_f+sub_m)
      sub_temp$fem <- round(ra_f*sub_temp$nbatlgth)
      sub_temp$mal <- round((1-ra_f)*sub_temp$nbatlgth)
      sub_temp[is.na(sub_temp)] <- 0
      for (idx in 1:nrow(sub_temp)){
        if(sub_temp$fem[idx]+sub_temp$mal[idx] < sub_temp$nbatlgth[idx]){
          sub_temp$mal <- sub_temp$nbatlgth-sub_temp$fem
        }
        if(sub_temp$fem[idx]+sub_temp$mal[idx] > sub_temp$nbatlgth[idx]){
          rmve <- (sub_temp$fem[idx]+sub_temp$mal[idx]) - sub_temp$nbatlgth[idx]
          if ( sub_temp$fem[idx] >  sub_temp$mal[idx]) sub_temp$fem[idx] <- sub_temp$fem[idx]-rmve
          if ( sub_temp$fem[idx] <=  sub_temp$mal[idx]) sub_temp$mal[idx] <- sub_temp$mal[idx]-rmve
          if (sub_temp$fem[idx] <0 ) sub_temp$fem[idx]  <- 0
          if (sub_temp$mal[idx] <0 ) sub_temp$mal[idx]  <- 0
        }

      }
      res8b <- rbind(res8b, sub_temp)
    }
    res8 <- res8b
  }
  
  # Take Nsamp in each clust as defined
  x=Nsamp
  res10 <- NULL

  for (k in 1:nrow(prop_cl)) {
    # here there could be the option of using dirichlet..
    
    if (is.null(probs_save)) {
      res_clust_temp <- as.data.frame(rmultinom(tows[tows$clust==prop_cl$clust[k],"tows"],x, 
                                                res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), c("nbatlgth")]))
    } else { # if sexes
      vec_props <- c( res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), c("fem")],  res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), c("mal")])
      res_clust_temp <- as.data.frame(rmultinom(tows[tows$clust==prop_cl$clust[k],"tows"],x,vec_props))
    }
    
    names(res_clust_temp) <- c(1:ncol(res_clust_temp))
    
    res_clust_temp$LENGTH <- res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), "LENGTH"]
    
    res9 <- melt(res_clust_temp, id.vars="LENGTH")
    names(res9)[c(2,3)] <- c("Haul","Nb")
    res9[,2] <- paste("cl",k,res9[,2], sep=".")
    
    if (!is.null(probs_save)) {
      res9$sex <- c(rep(1, length(res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), c("fem")])),
                    rep(2, length(res8[res8$clust==prop_cl$clust[k] & !is.na(res8$clust), c("mal")])))
    }
    
    res10 <- rbind(res10,res9)
  }
  
  # to return for testing intra-haul correlations
  to_return <- res10 
  
  if (!is.null(probs_save)) {
    final <- aggregate(Nb ~ LENGTH + sex, res10, sum)
    vec_df <- data.frame(LENGTH=c(vec,vec), sex=c(rep(1, length(vec)), rep(2, length(vec))))
    
    final <- merge(final, vec_df,all=T) # by.x="LENGTH", by.y="vec", 
    final$Nb[is.na(final$Nb)] <- 0
    
    final$props[final$sex==1] <- final$Nb[final$sex==1]/sum(final$Nb[final$sex==1])
    final$props[final$sex==2] <- final$Nb[final$sex==2]/sum(final$Nb[final$sex==2])
    final <- final[order(final$LENGTH),]
    final <- final[order(final$sex),]
    
  } else{
    final <- aggregate(Nb ~ LENGTH, res10, sum)
    vec_df <- data.frame(LENGTH=vec)
    
    final <- merge(final, vec_df,all=T)  
    final$Nb[is.na(final$Nb)] <- 0
    
    final$props <- final$Nb/sum(final$Nb)
    final <- final[order(final$LENGTH),]
  }
  
  # Put data back into original length bins - Question here... if vector of length is 20,23,26 etc, is all fish that is below 23 into 20? Or is it discarded if it is below 20??? Here assuming that it is all in 20!
  final$LENGTH         <- cut(final$LENGTH, c(0,vec, c(max(vec)+1)), include.lowest = F, right=F)
  levels(final$LENGTH) <- c(vec[1],vec)
  final$LENGTH         <- as.numeric(as.character(final$LENGTH))
  final                <- aggregate(cbind(Nb,props) ~ LENGTH, final, sum)
  if (is.null(probs_save)) {final <- aggregate(cbind(Nb,props) ~ LENGTH, final, sum)} else {
    final <- aggregate(cbind(Nb,props) ~ LENGTH + sex, final, sum)
  }
  
  if (plot.schools){
    
    if (!is.null(probs_save)) {
      plot(probs_f/sum(probs_f) ~ vec, type="l", xlab="LENGTH", ylab="Percentage", main="OBSERVED DISTRIBUTION", ylim=c(0,c(max(probs_f,probs_m,final$props)+0.02)), col="black", lty=1, lwd=1)
      points(probs_m/sum(probs_m) ~ vec, type="l", col="black", lty=1, lwd=1)
      points(final$props[final$sex==1] ~ final$LENGTH[final$sex==1], type="l", col="red", lwd=1)
      points(final$props[final$sex==2] ~ final$LENGTH[final$sex==2], type="l", col="blue", lwd=1)
      legend("topright", legend = c("Female","Male"), col=c("red","blue"), lty = c(1))
    } else {
      p_temp <-  probs/sum(probs)
      plot(p_temp ~ vec, type="l", xlab="LENGTH", ylab="Percentage", main="OBSERVED DISTRIBUTION", ylim=c(0,c(max(final$p_temp,final$props)+0.02)))
      points(final$props ~ final$LENGTH, type="l", col="red", lwd=2)
    }
  }
  
  # change final into line for lencomp in dat_file and rbind the years
  newcomp <- cbind.data.frame(matrix(final$props, nrow=1))
  res <- list(newcomp, to_return)
  if(length(res)==1) res <- res[[1]]
  
  invisible(res)
}





