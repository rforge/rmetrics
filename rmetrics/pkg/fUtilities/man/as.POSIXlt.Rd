\name{as.POSIXlt}


\alias{as.POSIXlt}
\alias{as.POSIXlt.default}


\title{Date-time Conversion Function}


\description{

    Adds date-time conversion function "as.POSIXlt"
    which is missing in R's base package.

}


\usage{
as.POSIXlt(x, tz = "")
}


\arguments{

    \item{tz}{
        time zone specification.
        }
    \item{x}{
        the object to be converted.
        }

}


\note{

    IMPORTANT NOTE: \code{as.POSIXlt} was introduced as generic function 
    in R 2.7.0. We will keep here the Rmetrics implementation to be downward 
    compatible with previous R/Rmetrics Versions. This has to be adapted.
    
}


\examples{
## as.POSIXlt -
   date = as.POSIXlt("2008-01-01")
   print(date)
   class(date)
}


\keyword{programming}

