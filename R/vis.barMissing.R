#' @title Visualize Distribution of Missing Values as a Barplot
#' 
#' @description Visualization of missing values in barplot form. Especially useful for
#' time series with a lot of observations.
#' 
#' @param ts.withNA Time Series (\code{\link{ts}}) object with NAs 
#' 
#' @param na.identifier Missing Value Identifier. 
#' If another value than NA indicates missing values this can be specified here. 
#' Identifier can be a character string as well as a numeric value.
#' 
#' @param breaks Defines the number of bins to be created. If breaksize isn't NULL it is overpowered
#' by this parameter
#' 
#' @param breaksize Defines how many observations should be in one bin. The required number of 
#' overall bins is afterwards calculated automatically.
#'  
#' @param percentage Whether the NA / non-NA ration should be given as percent or absolute numbers
#' 
#' @param legend If true a legend is shown above the plot.
#' 
#' @param ... Additional graphical parameters like e.g. main that can be passed through to barplot
#' 
#' @author Steffen Moritz
#' 
#' @examples
#' #Load dataset
#' data(heating)
#' 
#' #Visualize the missing values in this time series
#' vis.barMissing(heating, breaks = 20)
#' 
#' @import stats
#' @import datasets
#' @importFrom graphics legend barplot axis
#' @export

vis.barMissing <- function(ts.withNA, na.identifier = NA, breaks = 10, breaksize = NULL, percentage = T, legend = T, ... ) {
  
  #Check for wrong input and change identifier to NA
  precheck(ts.withNA, NA)
  
  inputTS <- ts.withNA
     
  
  #Calculate the breakssize from the demanded breaks
  if (is.null(breaksize)) {
    breaksize <- ceiling(length(inputTS) / breaks)
  }
  
  breakpoints <- c(1)
  bp <- 1
  while ( bp < length(inputTS))
  {
    bp <- bp+ breaksize
    if (bp >= length(inputTS))
    { bp <- length(inputTS) }
    breakpoints <- c(breakpoints,bp)  
  }
  
  #Define the width of the last bin in order to make it smaller if it contains less values
  widthLast <- (breakpoints[length(breakpoints)] - breakpoints[length(breakpoints)-1]) / (breakpoints[2] - breakpoints[1])
  
  #calculate NA/non-NA ratio inside of every bin
  naAmount <- numeric(0)
  okAmount <- numeric(0)
  for (i in 1:(length(breakpoints)-1)) {
    
    cut <- inputTS[(breakpoints[i]+1):(breakpoints[i+1])]
    
    nas <- length(which(is.na(cut)))
    naAmount <- c(naAmount,nas )
    
    oks <- length(cut) - nas
    okAmount <- c(okAmount, oks )
    
  }
  
  #calculate percentages if wanted 
  if (percentage == T) {
    
    temp1 <- naAmount/(okAmount+naAmount)
    temp2 <- okAmount/(okAmount+naAmount)
    naAmount[is.infinite(naAmount)] <- 1 
    okAmount[is.infinite(okAmount)] <- 1 
    
    naAmount <- temp1
    okAmount <- temp2
    ylab <- "Percentage non-NAs / NA"
    
  } else if(percentage == F) {
    ylab <- "Amount non-NAs / NA"
  }
  
  #create data to be plotted
  data <- matrix(c(naAmount,okAmount),byrow=TRUE,ncol=length(naAmount))
  
  #create the barplot
  barplot(data,width =c(rep(1,length(naAmount)-1),widthLast) , space =0,col=c('indianred2','green2'),xlab ="Time", ylab = ylab)
  
  #add axis
  axis(1, at=c(seq(0,length(naAmount))), labels = breakpoints, line = 0.5, tick = T)
  
  #add legend if wanted
  if (legend == T) {
    legend("topright", inset=c(0, -0.1), xpd=T,  bty ='n', horiz = T , cex=0.8, legend = c("NAs","non-NAs"), col = c("indianred2","green2"), pch = c(15))
  }
  
  
  
}




