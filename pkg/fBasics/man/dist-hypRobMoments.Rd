\name{hypRobMoments}

\alias{hypRobMoments}

\alias{hypMED}
\alias{hypIQR}
\alias{hypSKEW}
\alias{hypKURT}


\title{Robust Moments for the HYP}


\description{

  Computes the first four robust moments for the
  hyperbolic distribution.

}


\usage{
hypMED(alpha = 1, beta = 0, delta = 1, mu = 0)
hypIQR(alpha = 1, beta = 0, delta = 1, mu = 0)
hypSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)
hypKURT(alpha = 1, beta = 0, delta = 1, mu = 0)
}


\arguments{

  \item{alpha, beta, delta, mu}{
    shape parameter \code{alpha};
    skewness parameter \code{beta}, \code{abs(beta)} is in the
    range (0, alpha);
    scale parameter \code{delta}, \code{delta} must be zero or
    positive;
    location parameter \code{mu}, by default 0.
    These is the meaning of the parameters in the first
    parameterization \code{pm=1} which is the default
    parameterization selection.
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

  All values for the \code{*hyp} functions are numeric vectors:
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
## hypMED -
   # Median:
   hypMED(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypIQR -
   # Inter-quartile Range:
   hypIQR(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypSKEW -
   # Robust Skewness:
   hypSKEW(alpha = 1, beta = 0, delta = 1, mu = 0)

## hypKURT -
   # Robust Kurtosis:
   hypKURT(alpha = 1, beta = 0, delta = 1, mu = 0)

}


\keyword{distribution}
