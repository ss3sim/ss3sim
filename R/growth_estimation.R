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

# Function to sample the log likelihood of a fit to length and age data
#
# @param length.data data.frame which contains the lengths and ages
#  to fit the vbgf model..
# @param start.L1 numeric, starting guess for mle2 for L1 parameter.
# @param start.L2 numeric, starting guess for mle2 for L2 parameter.
# @param start.k numeric, starting guess for mle2 for k parameter.
# @param start.cv.young starting guess for mle2 for cv.young parameter.
# @param start.cv.old starting guess for mle2 for cv.old parameter.
# @param a3 integer, the youngest age well sampled in the data.
# @param A integer, the oldest age well sampled in the data.
sample_fit_vbgf <- function(length.data, start.L1, start.L2, start.k,
  start.cv.young, start.cv.old, lo.L1, lo.L2, lo.k, lo.cv.young, lo.cv.old,
  hi.L1, hi.L2, hi.k, hi.cv.young, hi.cv.old, a3, A){

  get_vbgf_loglik <- function(logL1, logLinf, logk, logcv.old, logcv.young){
    L1 <- get_logistic_transform(logL1, lo.L1, hi.L1)
    L.inf <- get_logistic_transform(logLinf, lo.L2, hi.L2)
    k <- get_logistic_transform(logk, lo.k, hi.k)
    cv.young <- get_logistic_transform(logcv.young, lo.cv.young, hi.cv.young)
    cv.old <- get_logistic_transform(logcv.old, lo.cv.old, hi.cv.old)
    slope <- (cv.old - cv.young) / (A - a3)
    cv <- cv.young + slope * (data_$age-a3)  # intercept = cv_young
    cv <- ifelse(cv < 0, 0, cv)
    sigma <- cv * (data_$mean)
    predLength <- vbgf_func(L1, L.inf, k, data_[, 1], a3)
    logLik <- sum(-log(sigma) - ((predLength - data_[, 2])^2)/(2*sigma^2))
    -logLik
  }

  get_logistic_transform <- function(par, lo, hi){
    par <- lo + (hi - lo)/(1+exp(-par))
    return(par)
  }

  inv_logistic <- function(par, lo, hi){
    par <- -log(hi-par)+log(par-lo)
    return(par)
  }

  #function takes data, number of samples, start values, start age
  #Data must have colums ordered: Year, Length, Weight, Sex, age
  #Then fits VBGF to subsampled data
  #Remove fish younger than a3 and older than A
  length.df <- length.data[length.data$age > a3, ]
  length.df <- length.df[length.df$age < A, ]
  data_ <- length.df[, colnames(length.df) %in% c("length", "age", "mean")]
#   start.Linf <- start.L1+(start.L2-start.L1)/(1-exp(-start.k*(A-a3)))
#   lo.Linf <- lo.L1+(lo.L2 - lo.L1)/(1-exp(-lo.k*(A-a3)))
#   hi.Linf <- hi.L1+(hi.L2 - hi.L1)/(1-exp(-hi.k*(A-a3)))
  pars.mat <- matrix(nrow=5, ncol=3, rbind(c(start.L1,lo.L1,hi.L1),
    c(start.L2,lo.L2,hi.L2),c(start.k,lo.k,hi.k),
    c(start.cv.young,lo.cv.young, hi.cv.young),
    c(start.cv.old, lo.cv.old, hi.cv.old)))
  transformed <- rep(0,5)
  for(i in 1:nrow(pars.mat)){
    transformed[i] <- inv_logistic(pars.mat[i,1], pars.mat[i,2], pars.mat[i,3])
  }

  #Fit using MLE
  mod <- mle2(get_vbgf_loglik,
    start = list(logL1 = transformed[1], logLinf = transformed[2],
      logk = transformed[3], logcv.old = transformed[4],
      logcv.young=transformed[5]))
  if(mod@details$convergence == 1 |
     grepl("Error", mod@coef[1], ignore.case = TRUE)){
    out <- list("L_at_Amin_Fem_GP_1" = 999, "L_at_Amax_Fem_GP_1" = 999,
      "VonBert_K_Fem_GP_1" = 999, "CV_young_Fem_GP_1" = 999,
      "CV_old_Fem_GP_1" = 999)
  } else {
    #Put estimated coefficients in EM terms
    logCoef <- mod@coef
    L1 <- get_logistic_transform(logCoef[1], lo.L1, hi.L1)
    L.inf <- get_logistic_transform(logCoef[2], lo.L2, hi.L2)
    k <- get_logistic_transform(logCoef[3], lo.k, hi.k)
    cv.young <- get_logistic_transform(logCoef[4], lo.cv.young, hi.cv.young)
    cv.old <- get_logistic_transform(logCoef[5], lo.cv.old, hi.cv.old)
    out <- list("L_at_Amin_Fem_GP_1" = L1, "L_at_Amax_Fem_GP_1" = L.inf,
      "VonBert_K_Fem_GP_1" = k, "CV_young_Fem_GP_1" = cv.young,
      "CV_old_Fem_GP_1" = cv.old)
  }
  out
}
