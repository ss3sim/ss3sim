#' Predict length given VBGF parameters
#'
#' @description External estimation procedure for von Bertalanffy growth.
#'
#' @param L1 mean length at youngest age which is well sampled in the data (a3)
#' @param L2 mean length at oldest age which is well sampled in the data (A)
#' @param k von bertalanffy growth rate parameter
#' @param ages vector of ages in the data for which you want to predict mean length-at-age
#' @param a3 youngest age which is well sampled in the data
#' @return a vector of lengths predicted which correspond to the input ages vector.
#'
#' @importFrom bbmle mle2

# TODO:
# document these functions

vbgf_func <- function(L1, L2, k, ages, a3){
  predLength <- L2 + (L1 - L2) * exp(-k * (ages - a3))
  predLength
}


#' @description Function to estimate the log likelihood of a fit to length and age data
#' Uses mle2 to minimize the negative log likelihood and uses vbgf_func to predict
#' data.
#' @param logL1 numeric,  log(L1) parameter.
#' @param logL2 numeric, log(L2) parameter.
#' @param logk numeric, log(k) parameter.
#' @param logsigma numeric, log(sigma) parameter, where
#' sigma is the slope of the CV line where the CV of an age = sigma*age.
#' @param data_ data.frame, column 1 is ages and column 2 is lengths
#' @param a3 numeric, the youngest age well sampled in the data.
get_vbgf_loglik <- function(logL1, logL2, logk, logsigma, data_, a3){
  L1 <- exp(logL1)
  L2 <- exp(logL2)
  k <- exp(logk)
  sigma <- exp(logsigma)*data_[,1]
  predLength <- vbgf_func(L1, L2, k, data_[, 1], a3)
  logLik <- sum(-log(sigma) - ((log(predLength) -
      log(data_[, 2]))^2)/(2*sigma^2))
  -logLik
}


#' @description Function to estimate the log likelihood of a fit to length and age data
#' Uses mle2 to minimize the negative log likelihood and uses vbgf_func to predict
#' data.
#' @param length.data data.frame which contains the lengths and ages
#'  to fit the vbgf model..
#' @param start.L1 numeric, starting guess for mle2 for L1 parameter.
#' @param start.L2 numeric, starting guess for mle2 for L2 parameter.
#' @param start.k numeric, starting guess for mle2 for k parameter.
#' @param start.cv.young, starting guess for mle2 for cv.young parameter.
#' @param a3 integer, the youngest age well sampled in the data.
#' @param A integer, the oldest age well sampled in the data.
sample_fit_vbgf <- function(length.data, start.L1, start.L2, start.k,
  start.cv.young, a3, A){

  #function takes data, number of samples, start values, start age
  #Data must have colums ordered: Year, Length, Weight, Sex, age
  #Then fits VBGF to subsampled data
  #Remove fish younger than a3 and older than A
  length.df <- length.data[length.data[, 1] > a3, ]
  length.df <- length.df[length.df[, 1] < A, ]

  start.sigma<-log(start.cv.young/a3)
  #Fit using MLE
  mod <- mle2(get_vbgf_loglik,
    start = list(logL1 = start.L1, logL2 = start.L2, logk = start.k, logsigma = .5),
    data_ = length.df, a3 = a3)

  if(mod@details$convergence == 1){
    out <- list("L1" = 999, "L2" = 999, "K" = 999, "cv.young" = 999, "cv.old" = 999)
  } else {
    #Put estimated coefficients in EM terms
    expCoef <- exp(mod@coef)
    cv.young <- expCoef[4]*a3
    cv.old <- expCoef[4]*A
    out <- list("L1" = expCoef[1], "L2" = expCoef[2],
      "K" = expCoef[3], "cv.young" = cv.young, "cv.old" = cv.old)
  }
  out
}
