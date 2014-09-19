\name{merge}                     


\title{Merges two 'timeSeries' objects}

\alias{merge}

\alias{merge,ANY,timeSeries-method}

\alias{merge,timeSeries,ANY-method}
\alias{merge,timeSeries,missing-method}
\alias{merge,timeSeries,numeric-method}
\alias{merge,timeSeries,matrix-method}
\alias{merge,timeSeries,timeSeries-method}

\alias{merge,matrix,timeSeries-method}
\alias{merge,numeric,timeSeries-method}


\description{

    Merges several object types with 'timeSeries' objects.
    The number of rows must match.
    
}


\value{

    Returns a 'timeSeries' object of two merged time series.
    
}


\details{

    The following combinations are supported:
    
    \tabular{ll}{
    \code{timeSeries} \tab ANY \cr
    \code{timeSeries} \tab missing \cr
    \code{timeSeries} \tab numeric \cr
    \code{timeSeries} \tab matrix \cr
    \code{timeSeries} \tab timeSeries }
        
}


\examples{
## Load Series -
   x <- MSFT[1:12, ]

## Merge 'timeSeries' with missing Object -
   merge(x)

## Merge 'timeSeries' with numeric Object -
   y <- rnorm(12)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- matrix(rnorm(24), ncol=2)
   class(y)
   merge(x, y)

## Merge 'timeSeries' with matrix Object -
   y <- timeSeries(data=rnorm(12), charvec=time(x))
   class(y)
   merge(x, y)
}


\keyword{chron}
 
