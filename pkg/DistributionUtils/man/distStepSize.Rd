\name{distStepSize}
\alias{distStepSize}

\title{
  Step Size for Calculating the Range of a Unimodal Distribution
}

\description{
  Given the parameter of a unimodal distribution and specify the root of
  the density function name, this function determines the step size when
  calculatinging the range of a specific unimodal distribution. The
  parameterization used is the one as for corresponding density function
  calculation.
}

\usage{
distStepSize(densFn, dist,
             param = NULL, side = c("right","left"), ...)
}
\arguments{
  \item{densFn}{Character. The name of the density function for which
    the step size needs to be calculated.}
  \item{dist}{Numeric. Current distance value, for Skew Hyperbolic
    distribution only}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
    are specified, then the default parameter values of each
    distribution are used instead.}  
  \item{side}{Character. \code{"right"} for a step to the right,
    \code{"left"} for a step to the right.}
  \item{\dots}{Passes arguments in particular the parameters of the
    distribution to random sample generation function.}
}

\details{
  The function is used for stepping to the right or the left to obtain
  an enclosing interval so \code{uniroot} can be used to search. The
  step size for right tail is the absolute difference between median and
  upper quantile and for left tail is the absolute difference between
  median and lower quantile. The skew hyperbolic distribution however
  needs special step size. When the tail is declining exponentially the
  step is just a linear function of the current distance from the
  mode. If the tail is declining only as a power of \eqn{x}, an
  exponential step is used.

  \code{distStepSize} is for internal use and is not expected to be
  called by users. It is documented here for completeness.
}

\value{
  The size of the step.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Joyce Li \email{xli053@aucklanduni.ac.nz}
}

\seealso{
  \code{\link{distCalcRange}}
}

\examples{
normRange <- distCalcRange("norm", tol = 10^(-7), mean = 4, sd = 1)
normRange
tRange <- distCalcRange("t", tol = 10^(-5), df = 4)
tRange
}

\keyword{distribution}
\keyword{univar}