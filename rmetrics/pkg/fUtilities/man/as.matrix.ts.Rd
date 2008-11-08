\name{as.matrix.ts}


\alias{as.matrix.ts}
\alias{as.matrix.mts}


\title{Convert Time Series to Matrix}


\description{

    Creates a matrix from the given "ts" or "mts"
    time series objects, a method missing in R's
    base package. 

}


\usage{
\method{as.matrix}{ts}(x, \dots)
\method{as.matrix}{mts}(x, \dots)
}


\arguments{

    \item{x}{
        an univariate or multivariate time series object
        of class \code{"ts"} or \code{"mts"} which will be
        transformed into an one-column or multi-column rectangular
        object of class \code{"matrix"}.
        }
    \item{\dots}{
        arguments to be passed.
    }

}



\examples{
## as.matrix -
   z <- ts(matrix(rnorm(300), 100, 3), start=c(1961, 1), frequency=12)
   class(z)
   z
   as.matrix(z)
}


\keyword{programming}

