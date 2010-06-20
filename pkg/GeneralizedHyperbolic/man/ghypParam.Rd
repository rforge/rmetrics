\name{ghypParam}
\docType{data}
\alias{ghypSmallShape}
\alias{ghypLargeShape}
\alias{ghypSmallParam}
\alias{ghypLargeParam}

\title{Parameter Sets for the Generalized Hyperbolic Distribution}

\description{
  These objects store different parameter sets of the generalized
  hyperbolic distribution as matrices for testing or demonstration
  purposes.

  The parameter sets \code{ghypSmallShape} and
  \code{ghypLargeShape} have a constant location parameter of
  \eqn{\mu}{mu} = 0, and constant scale parameter \eqn{\delta}{delta} =
  1. In \code{ghypSmallParam} and \code{ghypLargeParam} the values of
  the location and scale parameters vary. In these parameter sets the
  location parameter \eqn{\mu}{mu} = 0 takes values from \{0, 1\} and
  \{-1, 0, 1, 2\}  respectively. For the scale parameter
  \eqn{\delta}{delta}, values are drawn from \{1, 5\} and \{1, 2, 5,
  10\} respectively.

  For the shape parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta} the
  approach is more complex. The values for these shape parameters were
  chosen by choosing values of \eqn{\xi}{xi} and \eqn{\chi}{chi} which
  range over the shape triangle, then the function \code{ghypChangePars}
  was applied to convert them to the \eqn{\alpha, \beta}{alpha, beta}
  parameterization. See the examples for the values of \eqn{\xi}{xi} and
  \eqn{\chi}{chi} for the large parameter sets.

  The values of \eqn{\lambda}{lambda} are drawn from \{-0.5, 0, 1\} in
  \code{ghypSmallShape} and \{-1, -0.5, 0, 0.5, 1, 2\} in
  \code{ghypLargeShape}.


}

\usage{
  ghypSmallShape
  ghypLargeShape
  ghypSmallParam
  ghypLargeParam
}

\format{
  \code{ghypSmallShape}: a 22 by 5 matrix;
  \code{ghypLargeShape}: a 90 by 5 matrix;
  \code{ghypSmallParam}: a 84 by 5 matrix;
  \code{ghypLargeParam}: a 1440 by 5 matrix.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
data(ghypParam)
plotShapeTriangle()
xis <- rep(c(0.1,0.3,0.5,0.7,0.9), 1:5)
chis <- c(0,-0.25,0.25,-0.45,0,0.45,-0.65,-0.3,0.3,0.65,
          -0.85,-0.4,0,0.4,0.85)
points(chis, xis, pch = 20, col = "red")


## Testing the accuracy of ghypMean
for (i in 1:nrow(ghypSmallParam)) {
  param <- ghypSmallParam[i, ]
  x <- rghyp(1000, param = param)
  sampleMean <- mean(x)
  funMean <- ghypMean(param = param)
  difference <- abs(sampleMean - funMean)
  print(difference)
}

}

