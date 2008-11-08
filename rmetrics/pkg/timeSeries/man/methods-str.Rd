\name{str-methods}


\title{Object Str}


\alias{str}
\alias{str,timeSeries-method}

\description{
    
    Produce a result str for a time series.         
    
}


\usage{
\S4method{str}{timeSeries}(object, \dots)
}


\arguments{

    \item{object}{  
        an object of class \code{timeSeries}.
        }
    \item{\dots}{
        arguments passed to other methods.
        }

}


\value{

    returns a str report for an object of class \code{timeSeries}.
        
}


\examples{
## data -
   MSFT = as.timeSeries(data(msft.dat))[1:12, 1:4]
   colnames(MSFT) <- abbreviate(colnames(MSFT), 4)
    
## str -
   str(MSFT)
}


\keyword{chron}

