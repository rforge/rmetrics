\name{colCum}

\alias{cumColumnRowStatistics}

\alias{colCumsums} 
\alias{colCumsums.default}
\alias{colCumsums.timeSeries}
\alias{colCumsums.zoo}

\alias{colCummaxs} 
\alias{colCummaxs.default}
\alias{colCummaxs.timeSeries}
\alias{colCummaxs.zoo} 

\alias{colCumprods} 
\alias{colCumprods.default}
\alias{colCumprods.timeSeries}
\alias{colCumprods.zoo}  


\alias{cumsum.timeSeries}  


\title{Cumulated Column Statistics}


\description{

    Functions to compute cumulative column statistics.

}


\usage{
\method{colCumsums}{default}(x, na.rm = FALSE, \dots)
\method{colCumsums}{timeSeries}(x, na.rm = FALSE, \dots)
\method{colCumsums}{zoo}(x, na.rm = FALSE, \dots)

\method{colCummaxs}{default}(x, na.rm = FALSE, \dots)
\method{colCummaxs}{timeSeries}(x, na.rm = FALSE, \dots)
\method{colCummaxs}{zoo}(x, na.rm = FALSE, \dots)

\method{colCumprods}{default}(x, na.rm = FALSE, \dots)
\method{colCumprods}{timeSeries}(x, na.rm = FALSE, \dots)
\method{colCumprods}{zoo}(x, na.rm = FALSE, \dots)

\method{cumsum}{timeSeries}(x)
}


\arguments{
  
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a time series, may be an object of class \code{"matrix"}, 
        \code{"timeSeries"}, or \code{"zoo"}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    all functions return an S4 object of class \code{timeSeries}.
    
}


\note{
    
    The method \code{cumsum.timeSeries} will be no longer supported
    use instead \code{colCumsums}.
    
    The methods for the \code{"zoo"} methods are still preliminary and 
    untested. \code{na.rm} is not yet supported in all case, please test.

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## Simulated Monthly Return Data:
   x = matrix(rnorm(24), ncol = 2)
     
## colStats -
   colCumsums(x)  
}


\keyword{univar}

