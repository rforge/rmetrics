\name{GeneralizedHyperbolicDistribution}
\docType{package}
\alias{GeneralizedHyperbolic-package}
\alias{GeneralizedHyperbolicDistribution}

\title{
  The Package `GeneralizedHyperbolic': Summary Information
}

\description{
This package provides a collection of functions for working with the generalized
hyperbolic and related distributions.

For the hyperbolic distribution functions are provided for the density
function, distribution function, quantiles, random number generation and
fitting the hyperbolic distribution to data (\code{hyperbFit}). The
function \code{hyperbChangePars} will interchange parameter values
between different parameterisations.  The mean, variance, skewness,
kurtosis and mode of a given hyperbolic distribution are given by
\code{hyperbMean}, \code{hyperbVar}, \code{hyperbSkew},
\code{hyperbKurt}, and \code{hyperbMode} respectively. For assessing the
fit of the hyperbolic distribution to a set of data, the log-histogram
is useful. See \code{\link[DistributionUtils]{logHist}}. Q-Q and P-P
plots are also provided for assessing the fit of a hyperbolic
distribution. A \enc{Crämer}{Cramer}-von~Mises test of the goodness of
fit of data to a hyperbolic distribution is given by
\code{hyperbCvMTest}. S3 \code{print}, \code{plot} and \code{summary}
methods are provided for the output of \code{hyperbFit}.

For the generalized hyperbolic distribution functions are provided for
the density function, distribution function, quantiles, and for random
number generation. The function \code{ghypChangePars} will interchange
parameter values between different parameterisations.  The mean, variance, and
mode of a given generalized hyperbolic distribution are given by
\code{ghypMean}, \code{ghypVar}, \code{ghypSkew}, \code{ghypKurt}, and
\code{ghypMode} respectively.  Q-Q and P-P plots are also provided for
assessing the fit of a generalized hyperbolic distribution.

For the generalized inverse Gaussian distribution functions are provided for
the density function, distribution function, quantiles, and for random
number generation. The function \code{gigChangePars} will interchange
parameter values between different parameterisations.  The mean,
variance, skewness, kurtosis and mode of a given generalized inverse
Gaussian distribution are given by \code{gigMean}, \code{gigVar},
\code{gigSkew}, \code{gigKurt}, and \code{gigMode} respectively.  Q-Q and
P-P plots are also provided for assessing the fit of a generalized
inverse Gaussian distribution.

For the skew-Laplace distribution functions are provided for
the density function, distribution function, quantiles, and for random
number generation. Q-Q and P-P plots are also provided for assessing the
fit of a skew-Laplace distribution.
}


\author{David Scott \email{d.scott@auckland.ac.nz}}

\section{Acknowledgements}{
  A number of students have worked on the package: Ai-Wei Lee, Jennifer Tso,
  Richard Trendall, and Thomas Tran.

  Thanks to Ross Ihaka and Paul Murrell for their willingness to answer
  my questions, and to all the core group for the development of R.
}
\references{
  Barndorff-Nielsen, O. (1977)
  Exponentially decreasing distributions for the logarithm of particle size,
  \emph{Proc. Roy. Soc. Lond.},
  A\bold{353}, 401--419.

  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.

  Fieller, N. J., Flenley, E. C. and Olbricht, W. (1992)
  Statistics of particle size data.
  \emph{Appl. Statist.},
  \bold{41}, 127--146.

  \enc{Jörgensen}{Jorgensen}, B. (1982). \emph{Statistical Properties of
  the Generalized Inverse Gaussian Distribution}. Lecture Notes in
  Statistics, Vol. 9, Springer-Verlag, New York.

  Prause, K. (1999)
  \emph{The generalized hyperbolic models: Estimation, financial
  derivatives and risk measurement.} PhD Thesis, Mathematics Faculty,
  University of Freiburg.
}


\section{LICENCE}{
This library and its documentation are usable under the terms of the
"GNU General Public License", a copy of which is distributed with the
package.
}


\keyword{distribution}
