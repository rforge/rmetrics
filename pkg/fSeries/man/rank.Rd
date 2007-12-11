\name{rank}


\title{timeSeries Correlations}


\alias{rank.timeSeries}


\description{
    
    Returns the sample ranks of the values of a 
    'timeSeries' object.
      
}


\usage{
\method{rank}{timeSeries}(x, ...)
}


\arguments{
        
    \item{x}{
        an univariate object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments to be passed to the default rank function .
        }  
       
}


\value{

    returns the ranks of a \code{timeSeries} object
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## data - 
   x = as.timeSeries(data(msft.dat))[, 1:4]
   x = 100*returnSeries(x)

## rank -
   rank(x[, "Open"])
}


\keyword{chron}

