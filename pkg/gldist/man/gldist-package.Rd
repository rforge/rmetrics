\name{gldist-package}

\alias{gldist-package}

\docType{package}

\title{

  An Asymmetry-Steepness Parameterization of the Generalized Lambda
  Distribution.

}

\description{

  The generalized lambda distribution (\acronym{GLD}) is a versatile
  distribution that can accommodate a wide range of shapes, including
  fat-tailed and asymmetric distributions. This package implements a
  more intuitive parameterization of the \acronym{GLD} that expresses
  the location and scale parameters directly as the median and
  inter-quartile range of the distribution. The remaining two shape
  parameters characterize the asymmetry and steepness of the
  distribution respectively. The fitting of the \acronym{GLD} to
  empirical data can be reduced to a two-parameter estimation problem
  where the location and scale parameters are estimated by their robust
  sample estimators. Moreover, the parameterization can be used to
  compare data sets in a convenient asymmetry and steepness shape
  plot. The underline C routines are written such that compilers that
  support vectorized mathematical operations can automatically vectorize
  the most time consuming loops (tested with icc 12.1.0).

  The package includes the usual distribution functions, fitting
  routines and a shape plot function.

  The other parametrization of the \acronym{GLD} (\acronym{RS} and
  \acronym{FKML}) are implemented in the packages \pkg{gld} and
  \pkg{GLDEX} and are available on CRAN.

}

\author{

  Yohan Chalabi and Diethelm Wuertz.

}

\references{

  Y. Chalabi, D. J. Scott and D. Wuertz, \emph{An Asymmetry-Steepness
  Parameterization of the Generalized Lambda Distribution}. Working
  paper, 2012.

}

\seealso{gldist, fitgl, glshapeplot, FKML2CSW, CSW2FKML}

\keyword{package}
