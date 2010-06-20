\name{gigParam}
\docType{data}
\alias{gigSmallParam}
\alias{gigLargeParam}

\title{Parameter Sets for the Generalized Inverse Gaussian Distribution}

\description{
  These objects store different parameter sets of the generalized
  inverse Gaussian distribution as matrices for testing or demonstration
  purposes.

  The parameter sets \code{gigSmallParam} and \code{gigLargeParam} give
  combinations of values of the parameters \eqn{\chi}{chi},
  \eqn{\psi}{psi} and \eqn{\lambda}{lambda}. For \code{gigSmallParam},
  the values of \eqn{\chi}{chi} and \eqn{\psi}{psi} are chosen from \{0.1,
  0.5, 2, 5, 20, 50\}, and the values of \eqn{\lambda}{lambda} from
  \{-0.5, 0, 0.5, 1, 5\}.  For \code{gigLargeParam}, the values of
  \eqn{\chi}{chi} and \eqn{\psi}{psi} are chosen from \{0.1, 0.2, 0.5, 1,
  2, 5, 10, 20, 50, 100\}, and the values of \eqn{\lambda}{lambda} from
  \{-2, -1, -0.5, 0, 0.1, 0.2, 0.5, 1, 2, 5, 10\}.
}

\usage{
  gigSmallParam
  gigLargeParam
}

\format{
  \code{gigSmallParam}: a 125 by 3 matrix;
  \code{gigLargeParam}: a 1100 by 3 matrix.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
data(gigParam)
## Check values of chi and psi
plot(gigLargeParam[, 1], gigLargeParam[, 2])
### Check all three parameters
pairs(gigLargeParam,
  labels = c(expression(chi),expression(psi),expression(lambda)))

## Testing the accuracy of gigMean
for (i in 1:nrow(gigSmallParam)) {
  param <- gigSmallParam[i, ]
  x <- rgig(1000, param = param)
  sampleMean <- mean(x)
  funMean <- gigMean(param = param)
  difference <- abs(sampleMean - funMean)
  print(difference)
}

}

