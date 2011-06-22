\name{ghRobMoments}

\alias{ghRobMoments}

\alias{ghMED}
\alias{ghIQR}
\alias{ghSKEW}
\alias{ghKURT}


\title{Robust Moments for the GH}


\description{

  Computes the first four robust moments for the
  generalized hyperbolic distribution..

}


\usage{
ghMED(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghIQR(alpha= 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghSKEW(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
ghKURT(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)
}


\arguments{
  \item{alpha, beta, delta, mu, lambda}{
    numeric values.
    \code{alpha} is the first shape parameter;
    \code{beta} is the second shape parameter in the range \code{(0, alpha)};
    \code{delta} is the scale parameter, must be zero or positive;
    \code{mu} is the location parameter, by default 0; and
    \code{lambda} defines the sublclass, by default -1/2.
    These are the meanings of the parameters in the first
    parameterization \code{pm=1} which is the default
    parameterization.
    In the second parameterization, \code{pm=2} \code{alpha}
    and \code{beta} take the meaning of the shape parameters
    (usually named) \code{zeta} and \code{rho}.
    In the third parameterization, \code{pm=3} \code{alpha}
    and \code{beta} take the meaning of the shape parameters
    (usually named) \code{xi} and \code{chi}.
    In the fourth parameterization, \code{pm=4} \code{alpha}
    and \code{beta} take the meaning of the shape parameters
    (usually named) \code{a.bar} and \code{b.bar}.
  }
}


\value{

  All values for the \code{*gh} functions are numeric vectors:
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
## ghMED -
   # Median:
   ghMED(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghIQR -
   # Inter-quartile Range:
   ghIQR(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghSKEW -
   # Robust Skewness:
   ghSKEW(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

## ghKURT -
   # Robust Kurtosis:
   ghKURT(alpha = 1, beta = 0, delta = 1, mu = 0, lambda = -1/2)

}


\keyword{distribution}
