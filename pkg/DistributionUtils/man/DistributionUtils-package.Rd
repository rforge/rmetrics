\name{DistributionUtils-package}
\Rdversion{1.1}
\alias{DistributionUtils-package}
\alias{DistributionUtils}
\docType{package}
\title{Utilities for Distributions in Packages following Scott, Wuertz and Dong}
\description{
  Utility functions useful for all distributions in packages following
  the standard approach developed in Scott, Wuertz and Dong (2009).

  Functionality includes sample skewness and kurtosis, log-histogram,
  tail plots, moments by integration, changing the point about which a
  moment is calculated, functions for testing distributions using
  inversion tests and the Massart inequality. Also includes an
  implementation of the incomplete Bessel K function.
}
\details{
The DESCRIPTION file:
\packageDESCRIPTION{DistributionUtils}

Contains functions which are useful for packages implementing
distributions.  Designed to work with my packages \CRANpkg{GeneralizedHyperbolic},
\CRANpkg{VarianceGamma}, \CRANpkg{SkewHyperbolic} and \CRANpkg{NormalLaplace}.
}
\author{
\packageAuthor{DistributionUtils}

Maintainer: \packageMaintainer{DistributionUtils}
}
\references{
  Scott, David J. and \enc{Würtz}{Wuertz}, Diethelm and Dong, Christine (2009)
  \emph{Software for Distributions in R}.
  UseR: The R User Conference 2009
  \url{https://www.r-project.org/conferences/useR-2009/slides/Scott+Wuertz+Dong.pdf}
}
\seealso{
  \code{\link[GeneralizedHyperbolic]{GeneralizedHyperbolicDistribution}}
}
\keyword{package}
