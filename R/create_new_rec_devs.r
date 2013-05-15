#' Generate a new recruitment deviation time series to put into a simulation 
#' 
#' @param rec_dev_dat Recruitment deviation data. A vector. Only used to get
#' the length of recruitment deviations to create.
#' @param best_model A model from arima(). Intended to be output from 
#' find_rec_dev_arma().
#' @return A vector of new recruitment deviations.
#' @export
#' @author Kotaro Ono
#' m <- arima(lh, order = c(1,0,0))
#' create_new_rec_devs(rep(0, 20), m)

create_new_rec_devs <- function(rec_dev_dat, best_model){
  ar_process <- grep("ar", names(best_model$coef))
  ma_process <- grep("ma", names(best_model$coef))
  datdat <- arima.sim(n = length(rec_dev_dat), list(
      ar = best_model$coef[ar_process], 
      sd=best_model$var.coef[ar_process],
      ma = best_model$coef[ma_process],
      sd=best_model$var.coef[ma_process]))
  return(datdat)
}
