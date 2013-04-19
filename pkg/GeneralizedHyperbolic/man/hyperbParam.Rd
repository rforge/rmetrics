\name{hyperbParam}
\docType{data}
\alias{hyperbParam}
\alias{hyperbSmallShape}
\alias{hyperbLargeShape}
\alias{hyperbSmallParam}
\alias{hyperbLargeParam}

\title{Parameter Sets for the Hyperbolic Distribution}

\description{
  These objects store different parameter sets of the hyperbolic
  distribution as matrices for testing or demonstration purposes.

  The parameter sets \code{hyperbSmallShape} and
  \code{hyperbLargeShape} have a constant location parameter of
  \eqn{\mu}{mu} = 0, and constant scale parameter \eqn{\delta}{delta} =
  1. In \code{hyperbSmallParam} and \code{hyperbLargeParam} the values of
  the location and scale parameters vary. In these parameter sets the
  location parameter \eqn{\mu}{mu} = 0 takes values from \{0, 1\} and
  \{-1, 0, 1, 2\}  respectively. For the scale parameter
  \eqn{\delta}{delta}, values are drawn from \{1, 5\} and \{1, 2, 5,
  10\} respectively.

  For the shape parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta} the
  approach is more complex. The values for these shape parameters were
  chosen by choosing values of \eqn{\xi}{xi} and \eqn{\chi}{chi} which
  range over the shape triangle, then the function
  \code{hyperbChangePars} was applied to convert them to the
  \eqn{\alpha, \beta}{alpha, beta} parameterization. The resulting
  \eqn{\alpha, \beta}{alpha, beta} values were then rounded to one
  decimal place. See the examples for the values of \eqn{\xi}{xi} and
  \eqn{\chi}{chi} for the large parameter sets.  }

\usage{
  hyperbSmallShape
  hyperbLargeShape
  hyperbSmallParam
  hyperbLargeParam
}

\format{
  \code{hyperbSmallShape}: a 7 by 4 matrix;
  \code{hyperbLargeShape}: a 15 by 4 matrix;
  \code{hyperbSmallParam}: a 28 by 4 matrix;
  \code{hyperbLargeParam}: a 240 by 4 matrix.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
data(hyperbParam)
plotShapeTriangle()
xis <- rep(c(0.1,0.3,0.5,0.7,0.9), 1:5)
chis <- c(0,-0.25,0.25,-0.45,0,0.45,-0.65,-0.3,0.3,0.65,
          -0.85,-0.4,0,0.4,0.85)
points(chis, xis, pch = 20, col = "red")


## Testing the accuracy of hyperbMean
for (i in 1:nrow(hyperbSmallParam)) {
  param <- hyperbSmallParam[i, ]
  x <- rhyperb(1000, param = param)
  sampleMean <- mean(x)
  funMean <- hyperbMean(param = param)
  difference <- abs(sampleMean - funMean)
  print(difference)
}

}

