\name{ModifyExtendCoerciens}

\alias{ModifyExtendCoerciens}

\alias{as.POSIXlt}
\alias{as.POSIXlt.default}

\alias{as.matrix.ts}
\alias{as.matrix.mts}

\title{Modify and Extend Coerciens}


\description{
    
    A collection and description of functions which
    modify and extend coercien functions.
    \cr
    
}


\usage{
as.POSIXlt(x, tz = "")

as.matrix.ts(x)
as.matrix.mts(x)
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
## SOURCE("fEcofin.3D-ModifyExtendCoerciens")
}


\keyword{programming}

