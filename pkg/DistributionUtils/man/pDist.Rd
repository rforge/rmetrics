\name{pDist}
\alias{pDist}
\alias{qDist}

\title{
  Distribution and Quantile Functions for Unimodal Distributions
}

\description{
  Given the density function of a unimodal distribution specified by the root
  of the density function name, returns the distribution function and
  quantile function of the specified distribution.
}

\usage{
pDist(densFn = "norm", q, param = NULL, subdivisions = 100,
      lower.tail = TRUE, intTol = .Machine$double.eps^0.25,
      valueOnly = TRUE, ...)
qDist(densFn = "norm", p, param = NULL,
      lower.tail = TRUE, method = "spline", nInterpol = 501,
      uniTol = .Machine$double.eps^0.25,
      subdivisions = 100, intTol = uniTol, ...)
}
\arguments{
  \item{densFn}{Character. The name of the density function for which the
    distribution function or quantile function is required.}
  \item{q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
    are specified, then the default parameter values of each
    distribution are used instead.}
  \item{method}{Character. If \code{"spline"} quantiles are found from a
    spline approximation to the distribution function. If
    \code{"integrate"}, the distribution function used is always obtained
    by integration.}
  \item{lower.tail}{Logical. If \code{lower.tail = TRUE}, the cumulative
    density is taken from the lower tail.}
  \item{subdivisions}{The maximum number of subdivisions used to integrate
    the density and determine the accuracy of the distribution function
    calculation.}
  \item{intTol}{Value of \code{rel.tol} and hence \code{abs.tol} in calls
    to \code{integrate}. See \code{\link{integrate}}.}
  \item{valueOnly}{Logical. If \code{valueOnly = TRUE} calls to
    \code{pDist} only return the value obtained for the integral. If
    \code{valueOnly = FALSE} an estimate of the accuracy of the numerical
    integration is also returned.}
  \item{nInterpol}{Number of points used in \code{qDist} for cubic spline
    interpolation of the distribution function.}
  \item{uniTol}{Value of \code{tol} in calls to \code{uniroot}. See
    \code{\link{uniroot}}.}
  \item{\dots}{Passes additional arguments to \code{\link{integrate}},
    \code{\link{distMode}} or \code{\link{distCalcRange}}. In particular,
    the parameters of the distribution.}
}
\details{
The name of the unimodal density function must be supplied as the
characters of the root for that density (e.g. \code{norm}, \code{ghyp}).

\code{pDist} uses the function \code{\link{integrate}} to numerically
integrate the density function specified. The integration is from
\code{-Inf} to \code{x} if \code{x} is to the left of the mode, and from
\code{x} to \code{Inf} if \code{x} is to the right of the mode. The
probability calculated this way is subtracted from 1 if
required. Integration in this manner appears to make calculation of the
quantile function more stable in extreme cases.

\code{qDist} provides two methods to calculate quantiles both of which
use \code{uniroot} to find the value of \eqn{x} for which a given
\eqn{q} is equal to \eqn{F(x)} where \eqn{F(.)} denotes the distribution
function. The difference is in how the numerical approximation to
\eqn{F} is obtained. The more accurate method, which is specified as
\code{"integrate"}, is to calculate the value of \eqn{F(x)} whenever it
is required using a call to \code{pDist}. It is clear that the time
required for this approach is roughly linear in the number of quantiles
being calculated. The alternative (and default) method is that for the
major part of the distribution a spline approximation to \eqn{F(x)} is
calculated and quantiles found using \code{uniroot} with this
approximation. For extreme values of some heavy-tailed distributions
(where the tail probability is less than \eqn{10^(-7)}{10^(-7)}), the
integration method is still used even when the method specified as
\code{"spline"}.

If accurate probabilities or quantiles are required, tolerances
(\code{intTol} and \code{uniTol}) should be set to small values, i.e
\eqn{10^{-10}}{10^(-10)} or \eqn{10^{-12}}{10^(-12)} with
\code{method = "integrate"}. Generally then accuracy might be expected
to be at least \eqn{10^{-9}}{10^(-9)}. If the default values of the
functions are used, accuracy can only be expected to be around
\eqn{10^{-4}}{10^(-4)}. Note that on 32-bit systems
\code{.Machine$double.eps^0.25 = 0.0001220703} is a typical value.
}

\value{
\code{pDist} gives the distribution function, \code{qDist} gives the
quantile function.

An estimate of the accuracy of the approximation to the distribution
function can be found by setting \code{valueOnly = FALSE} in the call to
\code{pDist} which returns a list with components \code{value} and
\code{error}.
}

\author{
David Scott \email{d.scott@auckland.ac.nz}
Joyce Li \email{xli053@aucklanduni.ac.nz}}

\examples{
pDist("norm", q = 2, mean = 1, sd = 1)
pDist("t", q = 0.5, df = 4)
require(GeneralizedHyperbolic)
pDist("ghyp", q = 0.1)
require(SkewHyperbolic)
qDist("skewhyp", p = 0.4, param = c(0, 1, 0, 10))
qDist("t", p = 0.2, df = 4)
}

\keyword{distribution}
\keyword{univar}