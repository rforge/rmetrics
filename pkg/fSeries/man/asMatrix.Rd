\name{asMatrix}


\title{Converts a Time Series into a Matrix}


\alias{asMatrix}
\alias{asMatrix.default}

\alias{asMatrix.timeSeries}
\alias{asMatrix.zoo}
\alias{asMatrix.ts}


\description{
    
    A collection and description of functions 
    and methods to convert a time series into 
    a numeric matrix.       
    \cr
    
    Functions to create 'timeSeries' objects from other objects:
    
    \tabular{ll}{
    \code{asMatrix.timeSeries} \tab Converts a 'timeSeries' object into a matrix, \cr
    \code{asMatrix.zoo} \tab Converts a 'zoo' object into a matrix, \cr
    \code{asMatrix.ts} \tab Converts a 'ts' object into a matrix. }          
        
}


\usage{   

\method{asMatrix}{default}(x)
\method{asMatrix}{timeSeries}(x)
\method{asMatrix}{zoo}(x)
\method{asMatrix}{ts}(x)

}


\arguments{
  
    \item{x}{ 
        an multivariate time series object, out of the following classes:
        \code{"timeSeries"}, \code{"zoo"}, \code{"ts"}.
        }

}


\value{

    returns a numeric matrix with attributes related to the input 
    object.
        
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## timeSeries - 
   # Create an artificial timeSeries object:
   myFinCenter <<- "GMT"
   charvec = timeCalendar()
   data = matrix(rnorm(24), ncol = 2)
   tS = timeSeries(data, charvec, units = c("RAND1", "RAND2"))
   tS

## Test for timeSeries:
   vec = asMatrix(tS)
   head(vec)
   class(vec)
}
   

\keyword{chron}

