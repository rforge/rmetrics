\name{skewhypParam}
\docType{data}
\alias{skewhypSmallShape}
\alias{skewhypLargeShape}
\alias{skewhypSmallParam}
\alias{skewhypLargeParam}

\title{Parameter Sets for the Skew Hyperbolic t-Distribution}

\description{
  These objects store different parameter sets of the skew hyperbolic
  \eqn{t} distribution for testing or demonstrating purpose as matrices.

  Specifically, the parameter sets \code{skewhypSmallShape} and
  \code{skewhypLargeShape} have constant location parameter of \eqn{\mu
    = 0}{mu = 0} and scale parameter of \eqn{\delta = 1}{delta = 1}.

  The skewness parameter \eqn{\beta}{beta} takes values from \{0,2\} in
  \code{skewhypSmallShape} and \code{skewhypSmallParam}, and from
  \{-5,0,1,2,5\} in \code{skewhypLargeShape} and
  \code{skewhypLargeParam}.

  The shape parameter \eqn{\nu}{nu} takes values from \{1,5\} in
  \code{skewhypSmallShape} and \code{skewhypSmallParam}, and from
  \{1,2,5,10,20\} in \code{skewhypLargeShape} and
  \code{skewhypLargeParam}.
}

\usage{
  skewhypSmallShape
  skewhypLargeShape
  skewhypSmallParam
  skewhypLargeParam
}

\format{
  \code{skewhypSmallShape}: a 4 by 4 matrix;

  \code{skewhypLargeShape}: a 25 by 4 matrix;

  \code{skewhypSmallParam}: a 16 by 4 matrix;

  \code{skewhypLargeParam}: a 400 by 4 matrix.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\examples{
data(skewhypParam)
### Testing the accuracy of skewhypMean
for (i in 1:nrow(skewhypSmallParam)) {
  param <- skewhypSmallParam[i, ]
  x <- rskewhyp(1000, param = param)
  sampleMean <- mean(x)
  distMean <- skewhypMean(param = param)
  difference <- abs(sampleMean - distMean)
  print(difference)
}

}

