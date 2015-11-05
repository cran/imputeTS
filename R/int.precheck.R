##De-Roxygenized to avoid appearance in the package documentation

# @title Pre-Check Imputation Input (Internal function)
# @description Internal function for pre-checking the input of imputation functions
# @param data  Supposed to be a univariate time series 
# @param na.identifier Supposed to be a character string or numeric value. 
# If another value than NA indicates missing values this is specified with this parameter. 
# @details Checks, if data is really a univariate time series and checks that na.identifier is no list or vector.
# @return Time Series (\code{\link{ts}}) object that fulfills the requirements
# @author Steffen Moritz
#' @import stats


precheck <- function(data, na.identifier) { 
  
  #checking for false input
  if(!is.ts(data))
  {stop("Not a time series")}
  
  
  if(!is.null(dim(data)))
  {stop("The time series is not univariate.")}
  
  if(!length(na.identifier)==1)
  {stop("Error with missing value indicator mv - use something like mv = -1 or mv = NA or mv = \"x\" ")}
  
  
  #if missing data indicator is different to NA, replace with NA
  if (!is.na(na.identifier)) 
  {
    data[data == na.identifier] <- NA
  }
  
  return(data)
  
}