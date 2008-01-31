\name{as}


\alias{as}

\alias{as.POSIXlt}
\alias{as.POSIXlt.default}

\alias{as.matrix.ts}
\alias{as.matrix.mts}


\title{Basic Extensions}


\description{

    Adds methods to the function as, which are missing
    in R's base package.
    \cr
    
    The methods are:
    
    \tabular{ll}{
    \code{align} \tab adds align function, \cr
    \code{as.POSIXlt} \tab adds as.POSIXlt function, \cr
    \code{as.matrix.ts} \tab adds as.matrix.ts method, \cr
    \code{as.matrix.mts} \tab adds as.matrix.mts method. }

}


\usage{
as.POSIXlt(x, tz = "")

\method{as.matrix}{ts}(x, \dots)
\method{as.matrix}{mts}(x, \dots)
}


\arguments{

    \item{tz}{
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
    \item{\dots}{
        arguments to be passed.
    }

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.

}


\examples{
## 
}


\keyword{programming}

