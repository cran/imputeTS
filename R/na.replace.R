
#' @title Missing Value Imputation by Defined Value
#' 
#' @description Replaces all missing values with a given value.
#'  
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be replaced
#' 
#' @param fill Value used to replace the missing values
#' 
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.mean}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,NA,5,6,NA,7,8))
#' 
#' #Replace all NAs with 3.5
#' na.replace(x, fill = 3.5 )
#' 
#' @import stats
#' @export


na.replace <- function(data, fill = 0, na.identifier = NA) {
  
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
  data[missindx] <- fill
  
  return(data)
}