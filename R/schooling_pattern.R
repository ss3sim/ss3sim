#' @title Draws an aggregation pattern from empirical data
#'  
#' @description Takes a dataset of mean lengths and associated standard deviations,
#' that should be empirical data, i.e. mean length and SD from a number of hauls,
#' fits a Gamma GAM model and draws new SD predictions for a given
#' vector of mean lengths. These mean and SDs are the parameters to be used to 
#' define "clusters". However those clusters can not be normally ditributed as 
#' they are used to sample lengths data (ie positive) so the function also 
#' provides the equivalent Gamma distribution parameters for each mean length/SD 
#' combination
#'  
#' @details The function can be used independantly but was specifically written 
#' to be integrated into the length sampling function. See
#'  \code{\link{sample_lcomp}}. It is advised to use the \code{parallel_lgths}
#'  when turning the schooling option on in \code{\link{sample_lcomp}}.
#'  
#' @author Gwladys Lambert
#'    
#' @param mean_lengths vector of observed mean lengths (i.e. mean length in haul)
#' @param SD vector of associated standard deviations (i.e. deviations in length
#' in haul)
#' @param new_lengths vector of lengths for which to generate SDs.
#' @param plot_fit creates 2 plots. One that show the empirical relationship between 
#' mean and SD, overlaid with GAM confidence interval, and another one with GAM 
#' predictions, prediction interval and new SDs.
#' 
#' @return A dataset of \code{new_lengths} and generated SDs and corresponding 
#' Gamma distribution parameters
#'    
#' @import stats
#' @import mgcv
#' @import scales
#' @importFrom graphics lines polygon
#' @export
#' @family sampling functions
#'    
#' @examples
#'  \dontrun{
#'  mean_lengths <- seq(1:500)
#'  SD <- 0.1*1:500 + rnorm(100, 0, 5) # SHOULD PUT BETTER EXAMPLE - 
#'                                     # E.G. DOME SHAPE
#'  new_lengths = mean_lengths
#'  schooling_pattern(mean_lengths, SD, new_lengths, plot_fit=T) 
#'  }
#'  

schooling_pattern <- function(mean_lengths, SD, new_lengths, plot_fit=T) {
  
  temp <- cbind.data.frame(mean_lengths, SD)
  temp <- temp[!is.na(temp$SD) & temp$SD>0,]
  
  ## fit empirical relationship
  a <- gam(SD ~ s(mean_lengths), data=temp, family=Gamma(link=log), method="REML")
 
  # plot empirical data and relationship + c.i.
  if (plot_fit==T)
  {
    par(mfrow=c(2,2))
  plot(SD ~ mean_lengths, temp, bty="l", #xlim=c(xmin-20, xmax+20),ylab=ylab1,xlab="",
        main="Empirical data", cex.lab=1.4)
  
  new_data <- cbind.data.frame(mean_lengths=seq(min(temp$mean_lengths), max(temp$mean_lengths), by=0.1))
  pv <- predict(a, new_data, type="response",se=TRUE) 
  lcl <- pv$fit - 1.96 * pv$se.fit
  ucl <- pv$fit + 1.96 * pv$se.fit
  i.for <- order( new_data$mean_lengths )
  i.back <- order( new_data$mean_lengths , decreasing = TRUE )
  x.polygon <- c( new_data$mean_lengths[i.for] , new_data$mean_lengths[i.back] )
  y.polygon <- c( ucl[i.for] , lcl[i.back] )
  polygon( x.polygon , y.polygon , col = alpha("blue",0.2), border = NA )
  lines(new_data$mean_lengths,pv$fit, col="blue", lwd=2) 
  }
  
  ## simulate predictions intervals
  
  # extract parameter estiamtes and cov matrix
  beta <- coef(a);Vb <- vcov(a)
  
  ## simulate replicate beta vectors from posterior
  Cv <- chol(Vb)
  n.rep=10000; nb <- length(beta)
  br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta
  
  ## turn these into replicate linear predictors...
  xp <- min(temp$mean_lengths):max(temp$mean_lengths)#/600
  Xp <- predict(a,newdata=data.frame(mean_lengths=xp),type="lpmatrix")
  lp <- Xp%*%br
  fv <- exp(lp) ## ... finally, replicate expected value vectors
  
  ## now simulate from Gamma deviates with mean as in fv
  ## and estimated scale...
  
  yr <- matrix(rgamma(fv*0,shape=1/a$scale,scale=fv*a$scale),nrow(fv),ncol(fv))
  
  if (plot_fit==T){
  plot(rep(xp,n.rep),yr, type="n", main="Predicted intervals (red)") ## plotting replicates
  points(temp$mean_lengths,temp$SD, col="blue") ## and original data
  
  ## compute 95% prediction interval...
  PI <- apply(yr,1,quantile,prob=c(.025,0.975))
  ## and plot it...
  lines(xp,PI[1,],col=2,lwd=2);lines(xp,PI[2,],col=2,lwd=2)
  legend("topright", legend=c("empirical data","new draws"), col=c("blue","green"), pch=18)
  }
  
  ## Now one draw of SDs for the new vector of lengths
  n.rep=1
  br <- t(Cv) %*% matrix(rnorm(n.rep*nb),nb,n.rep) + beta
  new_xp <- new_lengths#/600
  new_xp <- predict(a,newdata=data.frame(mean_lengths=new_xp),type="lpmatrix")
  lp <- new_xp%*%br
  fv <- exp(lp) ## ... finally, replicate expected value vectors
  yr <- matrix(rgamma(fv*0,shape=1/a$scale,scale=fv*a$scale),nrow(fv),ncol(fv))
  if (plot_fit==T)
  {
  points(new_lengths, yr, col="green") 
  }
  
  out <- cbind.data.frame(new_lengths,SD=yr)
  
  # to assess gamma parameters knowing mean and sd...
   out$rate = ( out$new_lengths + sqrt( out$new_lengths^2 + 4*out$SD^2 ) ) / ( 2 * out$SD^2 )
   out$shape = 1 + out$new_lengths * out$ra
  
   if (plot_fit==T)
   {
   plot(function(x) dnorm(x, out$new_lengths[1], out$SD[1] ), -10, 700,
        main = "Normal density", ylim=c(0,0.1), xlab="Mean length in hauls to draw from", ylab="Length distribution probability")
   for (i in 2:nrow(out)){
   plot(function(x) dnorm(x, out$new_lengths[i], out$SD[i] ), -10, 700,
        main = "Normal density", ylim=c(0,0.1), add=T)
   }
   
   plot(function(x) dgamma(x, out$sh[1], out$ra[1] ), -10, 700,
        main = "Gamma", ylim=c(0,0.1), col="green", xlab="Mean length in hauls to draw from", ylab="Length distribution probability")
   for (i in 2:nrow(out)){
     plot(function(x) dgamma(x, out$sh[i], out$ra[i]  ), -10, 700,
          main = "Gamma", ylim=c(0,0.1), add=T, col="green")
   }
   }
   
   # plot(function(x) dnorm(x, out$new_lengths[210], out$SD[210] ), -1100, 1750,
   #      main = "Normal density")
   # plot(function(x) dgamma(x, out$sh[210], out$ra[210] ), -1100, 1750,
   #      main = "Gamma", add=T, col="red")
   # 
   
   # # First mean must be between 0 and 1
   # out2 <- out
   # out2$new_lengths <- out2$new_lengths/max(out$new_lengths)
   # out2$SD <- out2$SD/max(out$new_lengths)
   # 
   # 
   # estBetaParams <- function(mu, var) {
   #  # alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
   #   #beta <- alpha * (1 / mu - 1)
   #   ab = mu*(1-mu)/(var) - 1
   #   alpha = mu*ab
   #   beta = (1-mu)*ab
   #   return(params = list(alpha = alpha, beta = beta))
   # }
   # 
   # bet <- estBetaParams(out2$new_lengths, out2$SD)
   # 
   # for (i in 1:length(bet[[1]])){
   #  plot(function(x) dbeta(x, bet$alpha[i], bet$beta[i]), 0, 1,
   #      main = "Beta", col="blue", add=T)
   # }

   return(out)
}


  