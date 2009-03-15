\name{rank}


\title{Sample Ranks of a Time Series}


\alias{rank,timeSeries-method}


\description{
    
    Return the sample ranks of the values of a 
    'timeSeries' object.
      
}


\usage{
\S4method{rank}{timeSeries}(x, ...)
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


\examples{
## data -
   x = as.timeSeries(data(msft.dat))[, 1:4]
   x = 100*returns(x)

## rank -
   rank(x[, "Open"])
}


\keyword{chron}

