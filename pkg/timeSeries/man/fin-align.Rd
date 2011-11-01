\name{align-methods}

\docType{methods}
\alias{align-methods}
\alias{align,timeSeries-method}


\title{timeSeries Class, Functions and Methods}

\description{

    Aligns a 'timeSeries' Object.

}


\usage{
\S4method{align}{timeSeries}(x, by = "1d", offset = "0s",
    method = c("before", "after", "interp", "fillNA",
    "fmm", "periodic", "natural", "monoH.FC"),
    include.weekends = FALSE, ...)

}

\arguments{

    \item{x}{
        an object of class \code{timeSeries}.
        }
    \item{by}{
        a character string denoting the period
        }
    \item{offset}{
        a character string denoting the offset
        }
    \item{method}{
        a character string denoting the alignment approach.
        }
    \item{include.weekends}{
        a logical flag, should weekend be included.
        }
    \item{\ldots}{
        Further arguments to be passed to the interpolating function.
        }
}


\value{

    returns an aligned S4 object of class \code{timeSeries}.

}


\examples{
## Use MSFT and Compute Sample Size -
   dim(MSFT)

## Align the Series -
   MSFT.AL <- align(MSFT)

## Show the Size of the Aligned Series -
   dim(MSFT.AL)
}


\keyword{methods}
\keyword{chron}

