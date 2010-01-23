\name{Specific Generalized Hyperbolic Moments and Mode}
\alias{ghypMean}
\alias{ghypVar}
\alias{ghypSkew}
\alias{ghypKurt}
\alias{ghypMode}
\title{Moments and Mode of the Generalized Hyperbolic Distribution}
\description{
  Functions to calculate the mean, variance, skewness, kurtosis and mode
  of a specific generalized hyperbolic distribution.
}
\usage{
ghypMean(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
         param = c(mu, delta, alpha, beta, lambda))
ghypVar(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
        param = c(mu, delta, alpha, beta, lambda))
ghypSkew(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
         param = c(mu, delta, alpha, beta, lambda))
ghypKurt(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
         param = c(mu, delta, alpha, beta, lambda))
ghypMode(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
         param = c(mu, delta, alpha, beta, lambda))
}
\arguments{
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{lambda}{\eqn{\lambda}{lambda} is the shape parameter and dictates the
    shape that the distribution shall take. Default value is 1.}
  \item{param}{Parameter vector of the generalized hyperbolic distribution.}
}

\value{
  \code{ghypMean} gives the mean of the generalized hyperbolic distribution,
  \code{ghypVar} the variance, \code{ghypSkew} the skewness,
  \code{ghypKurt} the kurtosis, and \code{ghypMode} the mode. The
  formulae used for the mean is given in Prause (1999). The variance,
  skewness and kurtosis are obtained using the recursive formula
  implemented in \code{\link{ghypMom}} which can calculate moments of
  all orders about any point.
  
  The mode is found by a numerical optimisation using
  \code{\link{optim}}. For the special case of the hyperbolic
  distribution a formula for the mode is available, see
  \code{\link{hyperbMode}}.
   
  The parameterization of the generalized hyperbolic distribution used
  for these functions is the \eqn{(\alpha, \beta)}{(alpha, beta)} one. See
  \code{\link{ghypChangePars}} to transfer between parameterizations.
}
\references{
  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg. 
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Thomas Tran}

\seealso{
  \code{\link{dghyp}}, \code{\link{ghypChangePars}}, 
  \code{\link{besselK}}, \code{\link{RLambda}}.
}

\examples{
param <- c(2, 2, 2, 1, 2)
ghypMean(param = param)
ghypVar(param = param)
ghypSkew(param = param)
ghypKurt(param = param)
ghypMode(param = param)
maxDens <- dghyp(ghypMode(param = param), param = param)
ghypRange <- ghypCalcRange(param = param, tol = 10^(-3) * maxDens)
curve(dghyp(x, param = param), ghypRange[1], ghypRange[2])
abline(v = ghypMode(param = param), col = "blue")
abline(v = ghypMean(param = param), col = "red")
}
\keyword{distribution}
