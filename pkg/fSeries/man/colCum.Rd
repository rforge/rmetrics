\name{colCum}


\title{Cumulated Column Statistics}


\alias{colCum}

\alias{colCumsums} 
\alias{colCumsums.default}
\alias{colCumsums.timeSeries}

\alias{colCummaxs} 
\alias{colCummaxs.default}
\alias{colCummaxs.timeSeries}

\alias{colCummins} 
\alias{colCummins.default}
\alias{colCummins.timeSeries}

\alias{colCumprods} 
\alias{colCumprods.default}
\alias{colCumprods.timeSeries}

\alias{colCumreturns}
\alias{colCumreturns.default}
\alias{colCumreturns.timeSeries}

\alias{cumsum.timeSeries}  


\description{

    Functions to compute cumulative column statistics.

}


\usage{
\method{colCumsums}{default}(x, na.rm = FALSE, \dots)
\method{colCumsums}{timeSeries}(x, na.rm = FALSE, \dots)

\method{colCummaxs}{default}(x, na.rm = FALSE, \dots)
\method{colCummaxs}{timeSeries}(x, na.rm = FALSE, \dots)

\method{colCumprods}{default}(x, na.rm = FALSE, \dots)
\method{colCumprods}{timeSeries}(x, na.rm = FALSE, \dots)

\method{colCumreturns}{default}(x, method = c("geometric", "simple"), na.rm = FALSE, \dots)
\method{colCumreturns}{timeSeries}(x, method = c("geometric", "simple"), na.rm = FALSE, \dots)

\method{cumsum}{timeSeries}(x)
}


\arguments{
  
    \item{method}{
        a character string to indicate if geometric (TRUE) or simple 
        (FALSE) returns should be computed.
        }
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a time series, may be an object of class \code{"matrix"}, 
        or \code{"timeSeries"}.
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
    
}


\examples{
## Simulated Monthly Return Data:
   x = matrix(rnorm(24), ncol = 2)
     
## colStats -
   colCumsums(x)  
}


\keyword{univar}

