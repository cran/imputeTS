#' @title Seasonally Splitted Missing Value Imputation 
#' 
#' @description Splits the times series into seasons and afterwards performs imputation
#' seperatly for each of the resulting time series datasets (each containing the data 
#' for one specific season).
#
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be replaced
#' @param algorithm Algorithm to be used after splits. Accepts the following input:
#' \itemize{
#'    \item{"interpolation" - Imputation by Interpolation}
#'    \item{"locf" - Imputation by Last Observation Carried Forward}
#'    \item{"mean" - Imputation by Mean Value}
#'    \item{"random" - Imputation by Random Sample}
#'    }
#'
#'@param ... Additional parameters for these algorithms that can be passed through. Look at \code{\link[imputeTS]{na.interpolation}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.mean}} for parameter options.
#'    
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value. No support for lists or vectors.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.interpolation}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.mean}}
#' 
#' @examples
#' #Load dataset
#' require(datasets)
#' data(AirPassengers)
#' ap <- AirPassengers
#' 
#' #Insert NAs for testing purposes
#' ap[c(3,44,107)] <- NA
#' 
#' #Perform imputation using na.mean
#' na.seasplit(data = ap, algorithm = "interpolation")
#' 
#' 
#' @import stats 
#' @import datasets
#' @export


na.seasplit <- function(data, algorithm , na.identifier = NA,...) { 
  
  #Check for wrong input and change identifier to NA
  data <- precheck(data, na.identifier)
  
  #if no missing data, do nothing
  if(!anyNA(data)) {
    warning("No missing data found")
    return(data)
  }
  if(frequency(data)==1) {
    warning("No seasonality set for dataset found, going on without splitting")
  }
  
  ##
  ## Imputation Code
  ##
  
  for(i in 1:frequency(data)) {
    
    #get indices for one season
    indices <- seq(from = i, to = length(data), by = frequency(data))
    
    #Create time series just with one season
    ts.temp <- ts(data[indices])
    
    #Apply algorithm on this season
    ts.temp <- apply.base.algorithm(ts.temp, algorithm = algorithm,...)
    
    #Write result back into original time series
    data[indices] <- as.vector(ts.temp)
  }
  
  return(data)
}