#' @title Missing Value Imputation by Interpolation
#' 
#' @description Uses either linear or spline interpolation to replace missing values.
#'  
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"linear" - for linear interpolation}
#'    \item{"spline" - for spline interpolation}
#'    }
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value. No support for lists or vectors.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' @details Missing values get replaced by values of a \link{approx} or \link{spline} interpolation.
#'  Both functions are thereby called with their default parameters (except for parameter rule at approx, which is set to 2)
#'  If more flexibility in parameter settings is needed, na.approx / na.spline from the \pkg{zoo} 
#'  package can be a option.
#' 
#' @author Steffen Moritz
#' @seealso \code{\link[imputeTS]{na.mean}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,4,5,6,NA,7,8))
#' 
#' #Perform linear interpolation
#' na.interpolation(x)
#' 
#' #Perform spline interpolation
#' na.interpolation(x, option ="spline")
#' 
#' @import stats
#' @export


na.interpolation <- function(data, option = "linear", na.identifier = NA) { 
 
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
  
  n <- length(data)
  allindx <- 1:n
  indx <- allindx[!missindx]
  
  if(option =="linear") {
    interp <- as.ts(approx(indx,data[indx],1:n, rule=2)$y)
  }
  else if(option == "spline") {
    interp <- as.ts(spline(indx,data[indx],n = n )$y)
  }
  else {
    stop("Wrong parameter 'option' given. Value must be either 'linear' or 'spline'.")
  }
  
  for (i in 1:length(data)) {
    if (is.na(data[i])) {
      data[i] <- interp[i]
    }
  }
  
  return(data)
}