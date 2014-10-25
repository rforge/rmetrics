\name{rowCum}


\title{Cumulated Column Statistics}


\alias{rowCum}
\alias{rowCumsums}
\alias{rowCumsums,ANY-method}
\alias{rowCumsums,timeSeries-method}


\description{

    Compute cumulative row Statistics.

}


\usage{
\S4method{rowCumsums}{ANY}(x, na.rm = FALSE, \dots)
\S4method{rowCumsums}{timeSeries}(x, na.rm = FALSE, \dots)
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
## Simulated Monthly Return Data - 
   X = matrix(rnorm(24), ncol = 2)
     
## Compute cumulated Sums -
   rowCumsums(X)  
}


\keyword{univar}

