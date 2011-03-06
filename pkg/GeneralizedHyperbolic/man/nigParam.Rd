\name{nigParam}
\docType{data}
\alias{nigParam}
\alias{nigSmallShape}
\alias{nigLargeShape}
\alias{nigSmallParam}
\alias{nigLargeParam}

\title{Parameter Sets for the Normal Inverse Gaussian Distribution}

\description{
  These objects store different parameter sets of the normal inverse
  Gaussian distribution as matrices for testing or demonstration
  purposes.

  The parameter sets \code{nigSmallShape} and
  \code{nigLargeShape} have a constant location parameter of
  \eqn{\mu}{mu} = 0, and constant scale parameter \eqn{\delta}{delta} =
  1. In \code{nigSmallParam} and \code{nigLargeParam} the values of
  the location and scale parameters vary. In these parameter sets the
  location parameter \eqn{\mu}{mu} = 0 takes values from \{0, 1\} and
  \{-1, 0, 1, 2\}  respectively. For the scale parameter
  \eqn{\delta}{delta}, values are drawn from \{1, 5\} and \{1, 2, 5,
  10\} respectively.

  For the shape parameters \eqn{\alpha}{alpha} and \eqn{\beta}{beta} the
  approach is more complex. The values for these shape parameters were
  chosen by choosing values of \eqn{\xi}{xi} and \eqn{\chi}{chi} which
  range over the shape triangle, then the function \code{nigChangePars}
  was applied to convert them to the \eqn{\alpha, \beta}{alpha, beta}
  parameterization. See the examples for the values of \eqn{\xi}{xi} and
  \eqn{\chi}{chi} for the large parameter sets.
}

\usage{
  nigSmallShape
  nigLargeShape
  nigSmallParam
  nigLargeParam
}

\format{
  \code{nigSmallShape}: a 7 by 4 matrix;
  \code{nigLargeShape}: a 15 by 4 matrix;
  \code{nigSmallParam}: a 28 by 4 matrix;
  \code{nigLargeParam}: a 240 by 4 matrix.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
data(nigParam)
plotShapeTriangle()
xis <- rep(c(0.1,0.3,0.5,0.7,0.9), 1:5)
chis <- c(0,-0.25,0.25,-0.45,0,0.45,-0.65,-0.3,0.3,0.65,
          -0.85,-0.4,0,0.4,0.85)
points(chis, xis, pch = 20, col = "red")


## Testing the accuracy of nigMean
for (i in 1:nrow(nigSmallParam)) {
  param <- nigSmallParam[i, ]
  x <- rnig(1000, param = param)
  sampleMean <- mean(x)
  funMean <- nigMean(param = param)
  difference <- abs(sampleMean - funMean)
  print(difference)
}

}

