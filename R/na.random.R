
#' @title Missing Value Imputation by Random Sample
#' 
#' @description Replaces each missing value by drawing a random sample between two given bounds.
#'  
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be removed
#' 
#' @param lowerBound Lower bound for the random samples (default is min(data) )
#' 
#' @param upperBound Upper bound for the random samples (default is max(data) )
#' 
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' @details Replaces each missing value by drawing a random sample bewteen two given bounds. The 
#' default bounds are the minimum and the maximum value in the non-NAs from the time series. Funtion uses
#' \link{runif} function to get the random values.
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.mean}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' 
#' #Replace all NAs by random values between min and max of the inpute time series
#' na.random(x)
#' 
#' #Replace all NAs by random values between 1 and 10
#' na.random(x, lowerBound = 1, upperBound = 10)
#' 
#' @import stats
#' @export

na.random <- function(data, lowerBound = min(data, na.rm = T) , upperBound = max(data, na.rm = T), na.identifier = NA ) {
  
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
  
  missindx <- is.na(data)  
  
  #Check that lower bound is not higher than upper boun
  if (lowerBound > upperBound)
    {stop("Lower Bound for Random Number larger than Upper Bound ")}
  
  data[missindx] <- runif(1,min=lowerBound,max=upperBound)

  
  return(data)
}

  

  