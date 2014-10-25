\name{nigRobMoments}

\alias{nigRobMoments}

\alias{nigMED}
\alias{nigIQR}
\alias{nigSKEW}
\alias{nigKURT}


\title{Robust Moments for the NIG}


\description{

    Computes the first four robust moments for the
    Normal Inverse Gaussian Distribution.

}


\usage{
nigMED(alpha = 1, beta = 0, delta = 1, mu = 0)
nigIQR(alpha = 1, beta = 0, delta = 1, mu = 0)
nigSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)
nigKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{

    \item{alpha, beta, delta, mu}{
        are numeric values where
        \code{alpha} is the location parameter,
        \code{beta} is the location parameter,
        \code{delta} is the first shape parameter, and
        \code{mu} is the second shape parameter.
        }

}


\value{

    All values for the \code{*nig} functions are numeric vectors:
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.

    All values have attributes named \code{"param"} listing
    the values of the distributional parameters.

}


%\references{
%    go here ...
%}


\author{

    Diethelm Wuertz.

}


\examples{
## nigMED -
   # Median:
   nigMED(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigIQR -
   # Inter-quartile Range:
   nigIQR(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigSKEW -
   # Robust Skewness:
   nigSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)

## nigKURT -
   # Robust Kurtosis:
   nigKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\keyword{distribution}

