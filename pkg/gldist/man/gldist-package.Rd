\name{gldist-package}

\alias{gldist-package}

\docType{package}

\title{
  An Asymmetry-Steepness Parameterization of the Generalized
  Lambda Distribution.
}

\description{
  The generalized lambda distribution (GLD) is a versatile distribution
  that can accommodate a wide range of shapes, including fat-tailed and
  asymmetric distributions. This package implements a more intuitive
  parameterization of the GLD that expresses the location and scale
  parameters directly as the median and inter-quartile range of the
  distribution. The remaining two shape parameters characterize the
  asymmetry and steepness of the distribution respectively. The fitting
  of the GLD to empirical data can be reduced to a two-parameter
  estimation problem where the location and scale parameters are
  estimated by their robust sample estimators. Moreover, the
  parameterization can be used to compare data sets in a convenient
  asymmetry and steepness shape plot. The underline C routines are
  written such that compilers that support vectorized mathematical
  operations can automatically vectorize the most time consuming loops
  (tested with icc 12.1.0).
}

\author{
  Yohan Chalabi and Diethelm W\"urtz.
  Maintainer: Yohan Chalabi <yohan.chalabi@phys.ethz.ch>
}

\references{
  An Asymmetry-Steepness Parameterization of the Generalized Lambda
  Distribution. Y. Chalabi, D. J. Scott, D. W\"urtz, Working paper.
}

\seealso{gldist, fitgl, glshapeplot, FKML2CSW, CSW2FKML}

\keyword{package}
