\name{asVector}


\title{Converts a Time Series into a Vector}


\alias{asVector}
\alias{asVector.default}

\alias{asVector.timeSeries}
\alias{asVector.zoo}
\alias{asVector.ts}


\description{
    
    A collection and description of functions 
    and methods to convert a time series into 
    a numeric vector.       
    \cr
    
    Functions to create 'timeSeries' objects from other objects:
    
    \tabular{ll}{
    \code{asVector.timeSeries} \tab Converts a 'timeSeries' object into a vector, \cr
    \code{asVector.zoo} \tab Converts a 'zoo' object into a vector, \cr
    \code{asVector.ts} \tab Converts a 'ts' object into a vector. }          
        
}


\usage{   

\method{asVector}{default}(x)
\method{asVector}{timeSeries}(x)
\method{asVector}{zoo}(x)
\method{asVector}{ts}(x)

}


\arguments{
  
    \item{x}{ 
        an univariate time series object, out of the following classes:
        \code{"timeSeries"}, \code{"zoo"}, \code{"ts"}.
        }

}


\value{

    returns a numeric vector with attributes related to the input 
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
   data = matrix(rnorm(12))
   tS = timeSeries(data, charvec, units = "RAND")
   tS

## Test for timeSeries:
   vec = asVector(tS)
   print(vec)
   class(vec)
}
   

\keyword{chron}

