#' Predict length given VBGF parameters
#'
#' @description External estimation procedure for von Bertalanffy growth.
#'
#' @param L1 mean length at youngest age which is well sampled in the data (a3)
#' @param L.inf Length at infinity
#' @param k von bertalanffy growth rate parameter
#' @param ages vector of ages in the data for which you want to predict mean
#'   length-at-age
#' @param a3 youngest age which is well sampled in the data
#' @return a vector of lengths predicted which correspond to the input ages
#'   vector.
#' @importFrom bbmle mle2
vbgf_func <- function(L1, L.inf, k, ages, a3){
  predLength <- L.inf + (L1 - L.inf) * exp(-k * (ages - a3))
  predLength
}

#' Function to sample the log likelihood of a fit to length and age data
#'
#' @param length.data data.frame which contains the lengths and ages
#'  to fit the vbgf model..
#' @param start.L1 numeric, starting guess for mle2 for L1 parameter.
#' @param start.L2 numeric, starting guess for mle2 for L2 parameter.
#' @param start.k numeric, starting guess for mle2 for k parameter.
#' @param start.cv.young starting guess for mle2 for cv.young parameter.
#' @param start.cv.old starting guess for mle2 for cv.old parameter.
#' @param a3 integer, the youngest age well sampled in the data.
#' @param A integer, the oldest age well sampled in the data.
sample_fit_vbgf <- function(length.data, start.L1, start.L2, start.k,
  start.cv.young, start.cv.old, a3, A){

  get_vbgf_loglik <- function(logL1, logL2, logk, logcv.old, logcv.young){
    L1 <- exp(logL1)
    L2 <- exp(logL2)
    k <- exp(logk)
    slope <- (exp(logcv.old) - exp(logcv.young)) / (A - a3)
    cv <- exp(logcv.young) + slope * (data_$age-a3)  # intercept = cv_young
    cv <- ifelse(cv < 0, 0, cv)
    sigma <- cv * (data_$mean)
    L.inf<-L1+(L2-L1)/(1-exp(-k*(A-a3)))
    predLength <- vbgf_func(L1, L.inf, k, data_[, 1], a3)
    logLik <- sum(-log(sigma) - ((predLength - data_[, 2])^2)/(2*sigma^2))
    -logLik
  }

  #function takes data, number of samples, start values, start age
  #Data must have colums ordered: Year, Length, Weight, Sex, age
  #Then fits VBGF to subsampled data
  #Remove fish younger than a3 and older than A
  length.df <- length.data[length.data$age > a3, ]
  length.df <- length.df[length.df$age < A, ]
  data_ <- length.df[, colnames(length.df) %in% c("length", "age", "mean")]
  #Fit using MLE
  mod <- mle2(get_vbgf_loglik,
    start = list(logL1 = log(start.L1), logL2 = log(start.L2),
      logk = log(start.k), logcv.old = log(start.cv.old),
      logcv.young=log(start.cv.young)))
  if(mod@details$convergence == 1 |
     grepl("Error", mod@coef[1], ignore.case = TRUE)){
    out <- list("L1" = 999, "L2" = 999, "K" = 999, "cv.young" = 999, "cv.old" = 999)
  } else {
    #Put estimated coefficients in EM terms
    expCoef <- exp(mod@coef)
    cv.young <- expCoef[5]
    cv.old <- expCoef[4]
    L.inf<-expCoef[1]+(expCoef[2]-expCoef[1])/(1-exp(-expCoef[3]*(A-a3)))
    out <- list("L1" = expCoef[1], "L2" = L.inf,
      "K" = expCoef[3], "cv.young" = cv.young, "cv.old" = cv.old)
  }
  out
}
