\name{ghypScale}
\alias{ghypScale}

\title{
  Rescale a generalized hyperbolic distribution
}
\description{
  Given a specific mean and standard deviation will rescale any given
  generalized hyperbolic distribution to have the same shape but the
  specified mean and standard deviation. Can be used to standardize a
  generalized hyperbolic distribution to have mean zero and standard
  deviation one.

}
\usage{
ghypScale(newMean, newSD,
          mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
          param = c(mu, delta, alpha, beta, lambda))
}

\arguments{
  \item{newMean}{
    Numeric. The required mean of the rescaled distribution.
  }
  \item{newSD}{
    Numeric. The required standard deviation of the rescaled distribution.
  }
  \item{mu}{Numeric. Location parameter \eqn{\mu}{mu} of the starting
    distribution, default is0.
  }
  \item{delta}{Numeric. Scale parameter \eqn{\delta}{delta} of the starting
    distribution, default is 1.
  }
  \item{alpha}{Numeric. Tail parameter \eqn{\alpha}{alpha} of the starting
    distribution, default is 1.
  }
  \item{beta}{Numeric. Skewness parameter \eqn{\beta}{beta} of the starting
    distribution, default is 0.
  }
  \item{lambda}{Numeric. Shape parameter \eqn{\lambda}{lambda} of the starting
    distribution, default is 1.}
  \item{param}{Numeric. Specifying the parameters of the starting
    distribution as a vector of the form\cr
    \code{c(mu,delta,alpha,beta,lambda)}.
  }
}

\value{
    A numerical vector of length 5 giving the value of the parameters in
    the rescaled generalized hyperbolic distribution in the usual
    (\eqn{\alpha, \beta}{alpha, beta}) parameterization.
}
\references{
%% Add reference to paper with Diethelm Wuertz when completed
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\seealso{
%\link{\code{ghypStandPars}} for standardizing the generalized hyperbolic
%  distribution.
}
\examples{
param <- c(2,10,0.1,0.07,-0.5) # a normal inverse Gaussian
ghypMean(param = param)
ghypVar(param = param)
## convert to standardized parameters
(newParam <- ghypScale(0, 1, param = param))
ghypMean(param = newParam)
ghypVar(param = newParam)

## try some other mean and sd
(newParam <- ghypScale(1, 1, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
(newParam <- ghypScale(10, 2, param = param))
ghypMean(param = newParam)
sqrt(ghypVar(param = newParam))
}
\keyword{ distribution }
