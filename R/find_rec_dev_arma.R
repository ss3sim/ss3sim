#' Find the best ARMA model of the recruitment deviations
#' 
#' @details Fits all ARMA all models from AR [0:2] and MA [0:2] to the
#' recruitment deviations. Returns the model with the lowest AIC value.  
#' 
#' @param working_folder The folder with SS3 output. 
#' @return An arima() model object.
#' @export

find_rec_dev_arma <- function(working_folder) {
  Rec_dev <- get_rec_devs(working_folder)
  model.fits = list()
  count = 0
  for(ar in 0:2) {
    for(ma in 0:2) {
      count = count + 1
      AR = ar  # add 1 because of 0
      MA = ma # add 1 because of 0
      model.fits[[count]] = arima(ts(Rec_dev), c(AR, 0, MA))
    }
  }
  AR_order <- sapply(model.fits, function(x) getElement(x, "arma")[1])
  MA_order <- sapply(model.fits, function(x) getElement(x, "arma")[2])
  Model <- seq(1,9)
  extractaic = function(x) {
    getElement(x, "aic")
  }
  listAICs = lapply(model.fits,extractaic)
  allAICs = unlist(listAICs)
  DeltaAICs <- allAICs-min(allAICs)
  Summary_table <- data.frame(Model, AR_order,MA_order,AICs=allAICs,DeltaAICs)
  Summary_table <- Summary_table[order(Summary_table$DeltaAICs),]
  Best_model <- model.fits[[Summary_table[1,1]]]
  return(Best_model)
}
