#' @title Missing Value Imputation by Mean Value
#' 
#' @description Missing value replacement by mean values. Different means like median, mean, mode possible.
#' @param data Time Series (\code{\link{ts}}) object in which missing values are to be replaced
#' @param option Algorithm to be used. Accepts the following input:
#' \itemize{
#'    \item{"mean" - take the mean for imputation}
#'    \item{"median" - take the median for imputation}
#'    \item{"mode" - take the mode for imputation}
#'    }
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value. No support for lists or vectors.
#' 
#' @return Time Series (\code{\link{ts}}) object
#' 
#' @details Missing values get replaced by overall mean values. The function calculates the mean, median or mode
#' over all the non-NA values and replaces all NAs with this value. Option 'mode' replaces NAs with the 
#' most frequent value in the time series. If two or more values occur equally frequent, the funtion imputes
#' with the lower value. That's why 'mode' is not the best option for decimal values.
#' 
#' @author Steffen Moritz
#' 
#' @seealso \code{\link[imputeTS]{na.mean}}, \code{\link[imputeTS]{na.locf}},
#'  \code{\link[imputeTS]{na.random}}, \code{\link[imputeTS]{na.replace}}
#' 
#' @examples
#' #Create Time series with missing values
#' x <- ts(c(2,3,4,5,6,NA,7,8))
#' 
#' #Perform imputation with the overall mean
#' na.mean(x)
#' 
#' #Perform imputation with overall median
#' na.mean(x, option ="median")
#' 
#' @import stats
#' @export

na.mean <- function(data, option ="mean", na.identifier = NA) {
  
  
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
  
  if(option=="median") {
      median <- median(data, na.rm = T)    
      data[missindx] <- median 
  }
  else if(option=="mode") {
    #Calculate Mode
    temp <- table(as.vector(data))
    mode <- names(temp)[temp == max(temp)]
    mode <- (as.numeric(mode))[1]
    
    data[missindx] <- mode 
    
  }
  else if(option =="mean") {  
    #Use Mean
    mean <- mean(data, na.rm = T)
    data[missindx] <- mean 
  }
  else
  {stop("Wrong option given, must be 'mean', 'mode' or 'median'")}
  
  return(data)
}