#' @title Visualize differences between Imputed Values and Real Values
#' 
#' @description Visualize the differences between imputed values and real values to show performance of
#' imputation algorithm (only possible if NAs were artificially introduced and real values are known)
#' 
#' @param ts.withNA Time Series (\code{\link{ts}}) object with NAs before imputation
#' @param ts.withImpuations Time Series (\code{\link{ts}}) object with NAs replaced by imputed values
#' @param ts.withTruth Time Series (\code{\link{ts}}) object containing the real values behind the NAs 
#' 
#' @author Steffen Moritz
#' 
#' @examples
#' #Load dataset
#' require(datasets)
#' data(AirPassengers)
#' 
#' #Dataset with the real values
#' airp.tru <- AirPassengers
#' 
#' #Create dataset with NAs
#' airp.na <- AirPassengers
#' airp.na[3:5] <- NA
#' 
#' #Perform imputation using na.mean
#' airp.imp <- na.mean(airp.na)
#' 
#' #Visualize difference between imputed and real values
#' vis.differences(airp.na, airp.imp, airp.tru)
#' 
#' @import stats
#' @import datasets
#' @importFrom graphics legend lines par plot points


#' @export

vis.differences <- function(ts.withNA, ts.withImpuations, ts.withTruth) {
  
  
  #Check for wrong input and change identifier to NA
  precheck(ts.withNA, NA)
  precheck(ts.withImpuations, NA)
  precheck(ts.withTruth, NA)
  
 
  id.na <- which(is.na(ts.withNA))
  
  
  par(mfrow = c(2, 1), mar = c(2.2,2.2,2,2))
  plot(ts.withTruth, col = "gray")
  lines(ts.withNA)
  points(time(ts.withTruth)[id.na], ts.withTruth[id.na], col = "blue", pch = 19)
  points(time(ts.withImpuations)[id.na], ts.withImpuations[id.na], col = "red", pch = 17)
  legend("topleft", legend = c("true values", "imputed values"), 
         col = c("blue", "red"), pch = c(19, 17))
}