\name{rowCum}


\title{Cumulated Column Statistics}


\alias{rowCum}
\alias{rowCumsums} 
\alias{rowCumsums.default}
\alias{rowCumsums.timeSeries}
% \alias{rowCumsums.zoo}


\description{

    Functions to compute cumulative row Statistics.

}


\usage{
\method{rowCumsums}{default}(x, na.rm = FALSE, \dots)
\method{rowCumsums}{timeSeries}(x, na.rm = FALSE, \dots)
% \method{rowCumsums}{zoo}(x, na.rm = FALSE, \dots)
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
   rowCumsums(x)  
}


\keyword{univar}

