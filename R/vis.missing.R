#' @title Visualize distribution of Missing Values
#' 
#' @description Visualize the distribution of missing values within a time series.
#' 
#' @param ts.withNA Time Series (\code{\link{ts}}) object with NAs 
#' 
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value.
#' 
#' @param xlab Label for x axis of the plot
#' @param ylab Label for y axis of plot
#' @param ... Additional graphical parameters like e.g. main
#' 
#' @author Steffen Moritz
#' 
#' @examples
#' #Load dataset
#' require(datasets)
#' data(AirPassengers)
#' 
#' #Create dataset with NAs
#' airp.na <- AirPassengers
#' airp.na[c(3:5,44,77,99:103)] <- NA
#' 
#' #Visualize the missing values in this time series
#' vis.missing(airp.na)
#' 
#' @import stats
#' @import datasets
#' @importFrom graphics lines par plot points barplot
#' @export

vis.missing <- function(ts.withNA, na.identifier = NA, xlab = "", ylab = "", ... ) {
  
  
  #Check for wrong input and change identifier to NA
  precheck(ts.withNA, NA)

  id.na <- which(is.na(ts.withNA))
  
  barplotData <- rep(NA,length(ts.withNA))
  
  #make sure the end of the bar can not be seen
  barplotData[id.na] <- max(ts.withNA, na.rm =T )*100
    
  barplot(barplotData, col = "indianred2",xaxt = "n", yaxt = "n",   xlab = "", ylab = "", border = "indianred2")
  
  par(new=TRUE)
  
  plot(ts.withNA, cex = .2, type = "l", xlab = xlab, ylab = ylab, ... )
  points(ts.withNA, pch= 20 , cex = 0.8, col = "steelblue")
  
}




