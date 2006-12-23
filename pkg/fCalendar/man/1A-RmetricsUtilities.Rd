\name{RmetricsUtilities}

\alias{RmetricsUtilities}

\alias{as.POSIXlt}
\alias{as.POSIXlt.default}

\alias{as.matrix.ts}
\alias{as.matrix.mts}

\alias{modify}
\alias{modify.default}

\alias{atoms}
\alias{atoms.default}

\alias{currentYear}
\alias{myUnits}


\title{Some Rmetrics Utilities}


\description{
    
    A collection and description of functions which
    add to Rmetrics some additional generic functions 
    which may be useful.
    \cr
    
}


\usage{
as.POSIXlt(x, tz = "")

as.matrix.ts(x)
as.matrix.mts(x)

currentYear
myUnits
}


\arguments{

    \item{tz}{
        [as.POSIXlt] - \cr
        time zone specification.
        }
    \item{x}{
        [as.matrix.ts][as.matrix.mts] - \cr
        an univariate or multivariate time series object
        of class \code{"ts"} or \code{"mts"} which will be
        transformed into an one-column or multi-column rectangular 
        object of class \code{"matrix"}.\cr
        [as.POSIXlt] - \cr
        an object to be converted.
        }
 
}


\details{

    For details we refer to the original help pages.
    
}
    


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## SOURCE("fCalendar.1A-RmetricsUtilities")
}


\keyword{programming}

