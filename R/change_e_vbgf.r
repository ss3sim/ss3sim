#' External estimation procedure for von Bertalanffy growth
#'
#' @export
#' @importFrom bbmle mle2
#TODO:
#document these functions
#make sure that when the package is loaded that these functions are available to change_e

  #Function to predict length given VBGF parameters
  vbgf_func <- function(L1, L2, k, ages, a3){
    predLength <- L2 + (L1 - L2) * exp(-k * (ages - a3))
    return(predLength)
  }


sample_fit_VBGF <- function(length.data,start.L1,start.L2,start.k,start.cv.young,a3,A){
  #function takes data, number of samples, start values, start age
  #Data must have colums ordered: Year, Length, Weight, Sex, age
  #Then fits VBGF to subsampled data
  #Remove fish younger than a3 and older than A
  length.df<-length.data[length.data[, 1] > a3, ]
  length.df<-length.df[length.df[, 1] < A, ]
  
  #Returns the negative log likelihood function
  get_log_likelihood<-function(logL1,logL2,logk,logsigma){
    L1 <- exp(logL1)
    L2 <- exp(logL2)
    k <- exp(logk)
    sigma <- exp(logsigma)*length.df[,1]
    predLength <- vbgf_func(L1,L2,k,length.df[,1],a3)
    logLik <- sum(-log(sigma)-((log(predLength)-log(length.df[,2]))^2)/(2*sigma^2))
    return(-logLik)
  }
  
  start.sigma<-log(start.cv.young/a3)
  #Fit using MLE
  mod <- mle2(get_log_likelihood,start=list(logL1=start.L1,logL2=start.L2,logk=start.k,logsigma=.5))
  
  if(mod@details$convergence == 1){
  return(list("L1"=999,"L2"=999,"K"=999,"cv.young"=999,"cv.old"=999))
  } else {
      #Put estimated coefficients in EM terms
      expCoef <- exp(mod@coef)
      cv.young <- expCoef[4]*a3
      cv.old <- expCoef[4]*A
      return(list("L1" = expCoef[1], "L2" = expCoef[2], 
        "K" = expCoef[3], "cv.young" = cv.young, "cv.old" = cv.old))
  }

}
