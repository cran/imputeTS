#' @title Seasonally Decomposed Missing Value Imputation 
#' 
#' @description Removes the seasonal component from the time series, performs imputation on the deseasonalized series and afterwards adds the seasonal component again.
#'  
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be replaced
#' @param algorithm Algorithm to be used after decomposition. Accepts the following input:
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
#' na.seadec(data = ap, algorithm = "interpolation")
#' 
#' 
#' @import stats
#' @import datasets
#' @export



na.seadec <- function(data, algorithm , na.identifier = NA,...) { 
  
  #Check for wrong input and change identifier to NA
  data <- precheck(data, na.identifier)
  
  #if no missing data, do nothing
  if(!anyNA(data)) {
    warning("No missing data found")
    return(data)
  }
  if(frequency(data)==1) {
    warning("No seasonality set for dataset found, going on without decomposition")
    data <- apply.base.algorithm(data, algorithm = algorithm,...)
    return(data)
  }
  ##
  ## Imputation Code
  ##

  missindx <- is.na(data)  
  
  #approx NAs, to get complete series, because stl does not work with NAs
  temp <- na.interpolation(data)
  
  stl <- stl(temp,robust=TRUE, s.window = 11)
  # just take trend component + irregular  (remove seasonality)
  ts.noSeasonality <- stl$time.series[,2]+stl$time.series[,3]
  
  #Fill in NAs again
  ts.noSeasonality[missindx] <- NA
  
  #Perform imputation
  ts.imputed <- apply.base.algorithm(ts.noSeasonality, algorithm = algorithm,...)
  
  # add seasonality 
  data <- ts.imputed + stl$time.series[,1]
  
  
  return(data)
}