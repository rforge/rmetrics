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
   # plot(EDHEC[, 1], type = "o", col = "steelblue", 
   #   main = "EDHEC", xlab = "1997", ylab = "Return")
}


\keyword{chron}

