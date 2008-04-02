\name{plot-methods}


\title{Plot a Time Series}


\alias{plot}
\alias{plot,timeSeries,missing-method}
\alias{lines,timeSeries-method}
\alias{points,timeSeries-method}


\description{
    
    Plot 'timeSeries' objects.
    
}


\usage{
\S4method{plot}{timeSeries, missing}(x, \dots) 
\S4method{lines}{timeSeries}(x, \dots)
\S4method{points}{timeSeries}(x, \dots)
}


\arguments{

    \item{x}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    a plot or plot elements of an object of class \code{timeSeries}.
        
}


\examples{
## data -  
   EDHEC = as.timeSeries(data(edhec.tS))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 6)
   finCenter(EDHEC) <- "GMT"

## plot -  
   plot(EDHEC[, 1], type = "o", col = "steelblue", 
     main = "EDHEC", xlab = "1997", ylab = "Return")
   plot(EDHEC[, 1:2], type = "o", col = "steelblue", 
     main = "EDHEC", xlab = "1997", ylab = "Return")
   plot(EDHEC[, 1], EDHEC[, 2], type = "o", col = "steelblue", 
     main = "EDHEC", xlab = "Return 1", ylab = "Return 2")
     
## plot -
   # Example 1 - The wrong way to do it!
   ZRH = as.timeSeries(data(DEM98ZRH), zone = "Zurich", FinCenter = "Zurich")
   NYC = as.timeSeries(data(DEM98NYC), zone = "NewYork", FinCenter = "NewYork")
   finCenter(ZRH) 
   finCenter(NYC)
   plot(ZRH, type = "p", pch = 19, col = "blue")
   points(NYC, pch = 19, col = "red")
   hgrid()
 
## plot - 
   # Example 2 - Convert NYC to Zurich Time
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "Zurich"
   at = unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at, 
       xlab = paste(ZRH@FinCenter, "local Time"), main = ZRH@FinCenter)
   points(NYC, pch = 19, col = "red")
   hgrid()
  
## plot - 
   # Example 3 - Forced everything to GMT using "FinCenter" argument
   finCenter(ZRH) <- "Zurich"
   finCenter(NYC) <- "NewYork"
   at = unique(round(time(ZRH)))
   plot(ZRH, type = "p", pch = 19, col = "blue", format = "\%b \%d", at = at, 
       FinCenter = "GMT", xlab = "GMT", main = "ZRH - GMT")
   points(NYC, FinCenter = "GMT", pch = 19, col = "red")
   hgrid()
}


\keyword{chron}

