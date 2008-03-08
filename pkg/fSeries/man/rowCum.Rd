\name{rowCum}


\title{Cumulated Column Statistics}


\alias{rowCum}
\alias{rowCumsums} 
\alias{rowCumsums.default}
\alias{rowCumsums.timeSeries}


\description{

    Compute cumulative row Statistics.

}


\usage{
\method{rowCumsums}{default}(x, na.rm = FALSE, \dots)
\method{rowCumsums}{timeSeries}(x, na.rm = FALSE, \dots)
}


\arguments{
  
    \item{na.rm}{
        a logical. Should missing values be removed?
        }
    \item{x}{
        a time series, may be an object of class \code{"matrix"}
        or \code{"timeSeries"}.
        }
    \item{\dots}{
        arguments to be passed.
        }
        
}


\value{
    
    all functions return an S4 object of class \code{timeSeries}.
    
}



\examples{
## Simulated Monthly Return Data:
   x = matrix(rnorm(24), ncol = 2)
     
## colStats -
   rowCumsums(x)  
}


\keyword{univar}

