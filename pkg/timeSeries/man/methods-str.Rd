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
   require(fEcofin)
   EDHEC = as.timeSeries(data(edhec.tS))[1:12, 1:4]
   colnames(EDHEC) <- abbreviate(colnames(EDHEC), 4)
    
## str -
   str(EDHEC)
}


\keyword{chron}

