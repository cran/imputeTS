##De-Roxygenized to avoid appearance in the package documentation

# @title Algorithm selection (Internal function)
# @description Internal function for choosing between the basic univariate imputation algortihms
# @param data  Supposed to be a univariate time series 
# @param na.identifier Supposed to be a character string or numeric value. 
# If another value than NA indicates missing values this is specified with this parameter. 
# @details Checks, if data is really a univariate time series and checks that na.identifier is no list or vector.
# @return Time Series (\code{\link{ts}}) object that fulfills the requirements
# @author Steffen Moritz
#' @import stats


apply.base.algorithm <- function(data, algorithm, ...) { 
  
  #checking for false input
  if(algorithm == "locf")
  { data <- na.locf(data, na.identifier = NA, ...) }
  
  else if(algorithm == "mean")
  { data <- na.mean(data, na.identifier = NA, ...) }
  
  else if(algorithm == "random")
  { data <- na.random(data, na.identifier = NA, ...) }
  
  else if(algorithm == "interpolation")
  { data <- na.interpolation(data, na.identifier = NA, ...) }
  
  else 
  {stop("Wrong input for algortihm - possible inputs are (see help) ")}
  
  
  
  return(data)
  
}