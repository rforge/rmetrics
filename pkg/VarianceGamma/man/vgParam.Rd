% --- Source file: vgParam.Rd ---
\name{vgParam}
\alias{vgSmallShape}
\alias{vgLargeShape}
\alias{vgSmallParam}
\alias{vgLargeParam}

\title{Parameter Sets for Variance Gamma Distribution}

\description{
  These objects store different parameter sets of the Variance Gamma
  distribution for testing or demonstrating purpose as
  matrixes. Specifically, the parameter sets \code{vgSmallShape} and
  \code{vgLargeShape} have constant (standard) location and spread
  parameters of \eqn{c}{c}=0 and \eqn{\sigma}{sigma}=1; where asymmetry
  and shape parameters vary from \eqn{\theta}{theta}=(-2, 0, 2) and
  \eqn{\nu}{nu}=(0.5, 1, 2) for \code{vgSmallShape} and
  \eqn{\theta}{theta}=(-4, -2, 0, 2, 4) and \eqn{\nu}{nu}=(0.25, 0.5, 1,
  2, 4) for \code{vgLargeShape}.

  The parameter sets \code{vgSmallParam} and \code{vgLargeParam} have varied
  values of all 4 parameters. \code{vgSmallParam} contains all of the parameter
  combinations from \eqn{c}{c}=(-2, 0, 2), \eqn{\sigma}{sigma}=(0.5, 1, 2),
  \eqn{\theta}{theta}=(-2, 0, 2) and \eqn{\nu}{nu}=(0.5, 1, 2).
  \code{vgLargeParam} contains all of the parameter combinations from
  \eqn{c}{c}=(-4, -2, 0, 2, 4), \eqn{\sigma}{sigma}=(0.25, 0.5, 1, 2, 4),
  \eqn{\theta}{theta}=(-4, -2, 0, 2, 4) and \eqn{\nu}{nu}=(0.25, 0.5, 1, 2, 4).
}

\usage{
  vgSmallShape
  vgLargeShape
  vgSmallParam
  vgLargeParam
}

\format{
  \code{vgSmallShape}: a 9 by 4 matrix;
  \code{vgLargeShape}: a 25 by 4 matrix;
  \code{vgSmallParam}: a 81 by 4 matrix;
  \code{vgLargeParam}: a 625 by 4 matrix.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\examples{
data(vgParam)
## Testing the accuracy of vgMean
for (i in 1:nrow(vgSmallParam)) {
    param <- vgSmallParam[i,]
    x <- rvg(10000,param = param)
    sampleMean <- mean(x)
    funMean <- vgMean(param = param)
    difference <- abs(sampleMean - funMean)
    print(difference)
}

}

