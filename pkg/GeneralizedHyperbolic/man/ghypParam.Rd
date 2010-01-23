\name{ghypParam}
\docType{data}
\alias{ghypSmallShape}
\alias{ghypLargeShape}
\alias{ghypSmallParam}
\alias{ghypLargeParam}

\title{Parameter Sets for the Generalized Hyperbolic Distribution}

\description{
  These objects store different parameter sets of the Generalized 
  Hyperbolic distribution for testing or demonstrating purpose as
  matrices. Specifically, the parameter sets \code{ghypSmallShape} and
  \code{ghypLargeShape} have a constant location parameter of
  \eqn{\mu}{mu} = 0. The shape parameter \eqn{\lambda}{lambda} will
  vary from (-1, -0.5, 0, 1, 1.5) in \code{ghypSmallShape} and
  (-2, -1.5, -1, -0.5, 0, 0.5, 1, 1.5, 2) in \code{ghypLargeShape}.
  The parameter \eqn{\alpha}{alpha} varies from (0, 0.5, 1) in
  \code{ghypSmallShape} and (0, 0.5, 1, 1.5, 2) in \code{ghypLargeShape}.
  In \code{ghypSmallShape} the set of values for the \eqn{\beta}{beta}
  parameter are (0, 0.5, 1), while in \code{ghypLargeShape} they are
  (0, 0.5, 1, 1.5, 2). The \eqn{\delta}{delta} parameter will vary
  from (0, 0.5, 1) in \code{ghypSmallShape} and (0, 0.5, 1, 1.5, 2) in
  \code{ghypLargeShape}.

  The parameter sets \code{ghypSmallParam} and \code{ghypLargeParam} have varied
  values of all 5 parameters. The only difference between these datasets and
  those of \code{ghypSmallShape} and \code{ghypLargeShape} are that these also
  vary the \eqn{\mu}{mu} parameter. In \code{ghypSmallParam} this variation is
  between (-2, 0, 2) as is also the case in \code{ghypLargeParam}.

  Note that not all parameter variations may be apparent due to the
  validity of their combinations.
}

\usage{
  ghypSmallShape
  ghypLargeShape
  ghypSmallParam
  ghypLargeParam
}

\format{
  \code{ghypSmallShape}: a 24 by 5 matrix;
  \code{ghypLargeShape}: a 200 by 5 matrix;
  \code{ghypSmallParam}: a 72 by 5 matrix;
  \code{ghypLargeParam}: a 600 by 5 matrix.
}

\author{
  Simon Potter \email{spot023@aucklanduni.ac.nz}
}

\examples{
data(ghypParam)
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

