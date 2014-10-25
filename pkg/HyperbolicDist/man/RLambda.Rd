\name{Functions for Moments}
\alias{RLambda}
\alias{SLambda}
\alias{MLambda}
\alias{WLambda1}
\alias{WLambda2}
\alias{WLambda3}
\alias{WLambda4}
\alias{gammaLambda1}
\alias{gammaLambda2}

\title{Functions for Calculating Moments}
\description{
  Functions used to calculate the mean, variance, skewness and kurtosis
  of a hyperbolic distribution. Not expected to be called directly by users.
}
\usage{
RLambda(zeta, lambda = 1)
SLambda(zeta, lambda = 1)
MLambda(zeta, lambda = 1)
WLambda1(zeta, lambda = 1)
WLambda2(zeta, lambda = 1)
WLambda3(zeta, lambda = 1)
WLambda4(zeta, lambda = 1)
gammaLambda1(hyperbPi, zeta, lambda = 1)
gammaLambda1(hyperbPi, zeta, lambda = 1)

}
\arguments{
  \item{hyperbPi}{Value of the parameter  \eqn{\pi}{pi} of the
    hyperbolic distribution.}
  \item{zeta}{Value of the parameter  \eqn{\zeta}{zeta} of the
    hyperbolic distribution.}
  \item{lambda}{Parameter related to order of Bessel functions.}
}

\value{
  The functions \code{RLambda} and \code{SLambda}
  are used in the calculation of the mean and variance. They are
  functions of the Bessel functions of the third kind,
  implemented in \R as \code{\link{besselK}}. The other functions are
  used in calculation of higher moments. See Barndorff-Nielsen, O. and
  Blaesild, P (1981) for details of the calculations.

  The parameterisation of the hyperbolic distribution used for this
  and other components of the \code{HyperbolicDist} package is the
  \eqn{(\pi,\zeta)}{(pi,zeta)} one. See \code{hyperbChangePars} to
  transfer between parameterizations.
}
\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1981).
  Hyperbolic distributions and ramifications: contributions to theory
  and application.
  In \emph{Statistical Distributions in Scientific Work},
  eds., Taillie, C., Patil, G. P., and Baldessari, B. A., Vol. 4,
  pp. 19--44. Dordrecht: Reidel.

  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Richard Trendall,
  Thomas Tran}

\seealso{
  \code{\link{dhyperb}},
  \code{\link{hyperbMean}},\code{\link{hyperbChangePars}},
  \code{\link{besselK}}
}


\keyword{distribution}
