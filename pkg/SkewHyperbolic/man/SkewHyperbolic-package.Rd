\name{SkewHyperbolic-package}
\alias{SkewHyperbolic-package}
\alias{SkewHyperbolic}
\alias{skewhyp}
\docType{package}
\title{
The Package 'SkewHyperbolic': Summary Information
}
\description{
This package provides a collection of functions for working with the
skew hyperbolic Student's t-distribution.

Functions are provided for the density function (\code{dskewhyp}),
distribution function (\code{pskewhyp}), quantiles (\code{qskewhyp}) and
random number generation (\code{rskewhyp}). There are also functions
that fit the distribution to data (\code{skewhypFit}). The mean,
variance, skewness, kurtosis and mode can be found usig the functions
\code{skewhypMean}, \code{skewhypVar}, \code{skewhypSkew},
\code{skewhypKurt} and \code{skewhypMode} respectively. To assess
goodness of fit, there are also functions to generate a Q-Q plot
(\code{qqskewhyp}) and a P-P plot (\code{ppskewhyp}). S3 methods
\code{print}, \code{plot} and \code{summary} are provided for the output
of \code{skwewhypFit}.

\tabular{ll}{
Package: \tab SkewHyperbolic\cr
Type: \tab Package\cr
Version: \tab 0.1-1\cr
Date: \tab 2009-08-28\cr
License: \tab GPL(>=2)\cr
LazyLoad: \tab yes\cr
}
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}

\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's \emph{t}-distribution,
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.
}
\keyword{ package }
\seealso{ \code{\link{dskewhyp}}, \code{\link{skewhypMean}},
  \code{\link{skewhypFit}}, \code{\link{skewhypFitStart}},
  \code{\link{skewhypBreaks}}, \code{\link{qqskewhyp}},
  \code{\link[HyperbolicDist]{HyperbolicDist}}.
}
\examples{
%%examples go here
}
