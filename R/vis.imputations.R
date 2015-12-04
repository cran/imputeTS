#' @title Visualize the Imputed Values
#' 
#' @description Visualize the imputed values in a time series. Optional: For imputation
#' algorithm tests, with manuelly deleted values the the real values can be included.
#' 
#' @param ts.withNA Time Series (\code{\link{ts}}) object with NAs before imputation
#' @param ts.withImpuations Time Series (\code{\link{ts}}) object with NAs replaced by imputed values
#' @param ts.withTruth Time Series (\code{\link{ts}}) object with the real values. (only for imputation algortihm tests)
#' @param legend Adds a legend above the plot if set to TRUE.
#' @param ... Additional graphical parameters like e.g. main
#' @author Steffen Moritz
#' 
#' @examples
#' ##Example 1
#' #Load dataset
#' require(datasets)
#' data(AirPassengers)
#' 
#' #Create dataset with NAs
#' airp.na <- AirPassengers
#' airp.na[c(3:5,5,60:65,80,110:117)] <- NA
#' 
#' #Perform imputation using na.mean
#' airp.imp <- na.mean(airp.na)
#' 
#' #Visualize the imputed values
#' vis.imputations(airp.na, airp.imp)
#' 
#' 
#' ##Example 2
#' #Load dataset
#' require(datasets)
#' data(AirPassengers)
#' 
#' airp.na <- airp.truth <- AirPassengers
#' 
#' #Create dataset with NAs
#' airp.na[c(3:5,5,60:65,80,110:117)] <- NA
#' 
#' #Perform imputation using na.mean
#' airp.imp <- na.mean(airp.na)
#' 
#' #Visualize the imputed values
#' vis.imputations(airp.na, airp.imp, airp.truth)
#' @import stats
#' @import datasets
#' @importFrom graphics legend lines plot points


#' @export

vis.imputations <- function(ts.withNA, ts.withImpuations, ts.withTruth = NULL,legend = T,...) {
  
  
  #Check for wrong input and change identifier to NA
  precheck(ts.withNA, NA)
  precheck(ts.withImpuations, NA)

  
  id.na <- which(is.na(ts.withNA))
  
  #real time series (ts.withtruth) not available
  if (is.null(ts.withTruth)) {
    plot(ts.withImpuations, col = "indianred2", ylab="",xlab="",...)
    points(ts.withImpuations, col = "indianred2", pch = 20, cex=0.8)
    lines(ts.withNA)
    
    legend("topright", inset=c(0, -0.1), xpd=T,  bty ='n', horiz = T , cex=0.8, legend = c("imputed values", "known values"), col = c("indianred2","steelblue"), pch = c(20))
    points(ts.withNA, col ="steelblue", pch = 20, cex=0.8)
  }
  else {
    precheck(ts.withTruth, NA)
    
    plot(ts.withTruth, col ="green4", ylab="",xlab="", ...)
  # lines(ts.withImpuations, col = "indianred4")
    lines(ts.withNA)
    
    
    points( ts.withImpuations, col = "indianred2", pch = 20, cex=0.8)
    points(ts.withTruth, col ="green2", pch = 20, cex=0.8)
    points(ts.withNA, col ="steelblue", pch = 20, cex=0.8)
    
    if (legend == T) {
      legend("topright", inset=c(0, -0.1), xpd=T,  bty ='n', horiz = T , cex=0.8, legend = c("imputed values", "real values", "known values"), col = c("indianred2","green","steelblue"), pch = c(20))
    }
  }
}
