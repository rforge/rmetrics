\name{Specific Hyperbolic Distribution Moments and Mode}
\alias{hyperbMean}
\alias{hyperbVar}
\alias{hyperbSkew}
\alias{hyperbKurt}
\alias{hyperbMode}

\title{Moments and Mode of the Hyperbolic Distribution}
\description{
  Functions to calculate the mean, variance, skewness, kurtosis and mode
  of a specific hyperbolic distribution.
}
\usage{
hyperbMean(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
hyperbVar(mu = 0, delta = 1, alpha = 1, beta = 0,
          param = c(mu, delta, alpha, beta))
hyperbSkew(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
hyperbKurt(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
hyperbMode(mu = 0, delta = 1, alpha = 1, beta = 0,
           param = c(mu, delta, alpha, beta))
}
\arguments{
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{param}{Parameter vector of the hyperbolic distribution.}
}

\value{
  \code{hyperbMean} gives the mean of the hyperbolic distribution,
  \code{hyperbVar} the variance, \code{hyperbSkew} the skewness,
  \code{hyperbKurt} the kurtosis and \code{hyperbMode} the mode.
  
  Note that the kurtosis is the standardised fourth cumulant or what is
  sometimes called the kurtosis excess. (See
  \url{http://mathworld.wolfram.com/Kurtosis.html} for a discussion.)

  The parameterization of the hyperbolic distribution used for this
  and other components of the \code{HyperbolicDist} package is the
  \eqn{(\alpha, \beta)}{(alpha, beta)} one. See \code{\link{hyperbChangePars}}
  to transfer between parameterizations.
}
\details{
  
  The formulae used for the mean, variance and mode are as given in
  Barndorff-Nielsen and \enc{Blæsild}{Blaesild} (1983), p. 702.  The
  formulae used for the skewness and kurtosis are those of
  Barndorff-Nielsen and \enc{Blæsild}{Blaesild} (1981), Appendix 2. 

  Note that the variance, skewness and kurtosis can be obtained from the
  functions for the generalized hyperbolic distribution as special
  cases. Likewise other moments can be obtained from the function
  \code{\link{ghypMom}} which implements a recursive method to moments
  of any desired order. Note that functions for the generalized
  hyperbolic distribution use a different parameterization, so care is
  required.
  }
\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1981).
  Hyperbolic distributions and ramifications: contributions to theory
  and application.
  In \emph{Statistical Distributions in Scientific Work},
  eds., Taillie, C., Patil, G. P., and Baldessari, B. A., Vol. 4,
  pp. 19--44. Dordrecht: Reidel.

  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Richard Trendall,
  Thomas Tran}

\seealso{
  \code{\link{dhyperb}}, \code{\link{hyperbChangePars}},
  \code{\link{besselK}}, \code{\link{ghypMom}}, \code{\link{ghypMean}},
  \code{\link{ghypVar}}, \code{\link{ghypSkew}}, \code{\link{ghypKurt}}
}

\examples{
param <- c(2, 2, 2, 1)
hyperbMean(param = param)
hyperbVar(param = param)
hyperbSkew(param = param)
hyperbKurt(param = param)
hyperbMode(param = param)
}
\keyword{distribution}
