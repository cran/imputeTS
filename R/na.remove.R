
#' @title Remove Missing Values
#' 
#' @description Removes all missing values from a time series. 
#'  
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be removed
#'  
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' @details Removes all missing values from a time series. This shortens the time series by
#' the number of missing values in the series. Should be handled with care, because this can affect
#' the seasonality of the time series. Seasonal patterns might be destroyed and/or frequency parameter of the 
#' ts object might be no more correct.
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.mean}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' 
#' #Remove all NAs
#' na.remove(x)
#' 
#' #Create another Time series with missing values (marked with "-")
#' x2 <- ts(c(2,3,"-",5,6,"-",7,8))
#' 
#' #Remove all missing values (identified with "-")
#' na.remove(x2, na.identifier ="-")
#' 
#' @import stats
#' @export


na.remove <- function(data, na.identifier = NA) {
  
  
  #Check for wrong input and change identifier to NA
  data <- precheck(data, na.identifier)
  
  #if no missing data, do nothing
  if(!anyNA(data)) {
    warning("No missing data found")
    return(data)
  }
  
  ##
  ## Imputation Code
  ##
  temp <- numeric()
  for (i in 1:length(data)) {
    if ( ! is.na(data[i])) {      
      temp <- c(temp, data[i])
    }
  }
  data <- ts(temp,frequency = frequency(data))
  
  return(data)
}