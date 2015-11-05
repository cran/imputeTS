#' Time series of a heating systems supply temperature
#'
#' @description Time series of a heating systems supply temperature. Measured from 18.11.2013 - 05:12:00 to 13.01.2015 - 15:08:00 in 1 minute steps.
#' 
#' @details The time series originates from the GECCO Industrial Challenge 2015.
#' This Challenge was about "Recovering missing information in heating system operating data".
#' Goal was to impute missing values in heating system sensor data as accurate as possible.
#' (\url{http://www.spotseven.de/gecco-challenge/gecco-challenge-2015/})
#'
#' In order to use this series for comparing imputation algorithm results,
#' there are two time series provided. One series without missing values, which can
#' be used as ground truth. Another series with NAs, on which the imputation 
#' algorithms can be applied. The NAs thereby were inserted according to patterns
#' found in similar time series.
#' 
#' This are the two time series:
#' \itemize{
#'   \item heating - The time series with NAs.
#'
#'   \item heatingComplete - Time series without NAs.
#' }
#' @docType data
#' @keywords datasets
#' @name heating
#' @aliases heatingComplete
#' @usage data(heating)
#' data(heatingComplete)
#' @source \url{http://www.spotseven.de/gecco-challenge/gecco-challenge-2015/}
#' @format Time Series (\code{\link{ts}}) with 606837 rows including 57391 NAs.
NULL