\name{normRobMoments}

\alias{normRobMoments}

\alias{normMED}
\alias{normIQR}
\alias{normSKEW}
\alias{normKURT}


\title{Robust Moments for the NORM}


\description{

    Computes the first four robust moments for the
    Normal Distribution.

}


\usage{
normMED(mean = 0, sd = 1)
normIQR(mean = 0, sd = 1)
normSKEW(mean = 0, sd = 1)
normKURT(mean = 0, sd = 1)
}


\arguments{

  \item{mean}{locaiton parameter}
  \item{sd}{scale parameter}

}


\value{

    All values for the \code{*norm} functions are numeric vectors:
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
## normMED -
   # Median:
   normMED(mean = 0, sd = 1)

## normIQR -
   # Inter-quartile Range:
   normIQR(mean = 0, sd = 1)

## normSKEW -
   # Robust Skewness:
   normSKEW(mean = 0, sd = 1)

## normKURT -
   # Robust Kurtosis:
   normKURT(mean = 0, sd = 1)

}


\keyword{distribution}
