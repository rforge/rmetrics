\name{Specific normal inverse Gaussian Distribution Moments and Mode}
\alias{nigMean}
\alias{nigVar}
\alias{nigSkew}
\alias{nigKurt}
\alias{nigMode}

\title{Moments and Mode of the Normal Inverse Gaussian  Distribution}
\description{
  Functions to calculate the mean, variance, skewness, kurtosis and mode
  of a specific normal inverse Gaussian distribution.
}
\usage{
nigMean(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
nigVar(mu = 0, delta = 1, alpha = 1, beta = 0,
          param = c(mu, delta, alpha, beta))
nigSkew(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
nigKurt(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
nigMode(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
}
\arguments{
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{param}{Parameter vector of the nigolic distribution.}
}

\value{
  \code{nigMean} gives the mean of the normal inverse Gaussian distribution,
  \code{nigVar} the variance, \code{nigSkew} the skewness,
  \code{nigKurt} the kurtosis and \code{nigMode} the mode.
  
  Note that the kurtosis is the standardised fourth cumulant or what is
  sometimes called the kurtosis excess. (See
  \url{http://mathworld.wolfram.com/Kurtosis.html} for a discussion.)

  The parameterization of the normal inverse Gaussian distribution used for this
  and other components of the \code{GeneralizedHyperbolic} package is the
  \eqn{(\alpha, \beta)}{(alpha, beta)} one. See \code{\link{hyperbChangePars}}
  to transfer between parameterizations.
}
\details{
  
  The mean, variance, skewness, kurtosis and mode for the normal inverse Gaussian
  distribution can be obtained from the functions for the generalized 
  hyperbolic distribution as special cases (i.e., \eqn{\lambda}{lambda} = -1/2).
  Likewise other moments can be obtained from the function
  \code{\link{ghypMom}} which implements a recursive method to moments
  of any desired order. 
  
  The proper formulae for the mean, variance and skewness of the normal
  inverse Gaussian distribution can be found in Paolella, Marc S. (2007), 
  Chapter 9, p325.
  }
\references{
  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\seealso{
  \code{\link{dnig}}, \code{\link{hyperbChangePars}},
  \code{\link{besselK}}, \code{\link{ghypMom}}, \code{\link{ghypMean}},
  \code{\link{ghypVar}}, \code{\link{ghypSkew}}, \code{\link{ghypKurt}}
}

\examples{
param <- c(2, 2, 2, 1)
nigMean(param = param)
nigVar(param = param)
nigSkew(param = param)
nigKurt(param = param)
nigMode(param = param)
}
\keyword{distribution}
