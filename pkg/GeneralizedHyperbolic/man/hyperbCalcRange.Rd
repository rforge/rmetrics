\name{hyperbCalcRange}
\alias{hyperbCalcRange}
\title{Range of a Hyperbolic Distribution}
\description{
  Given the parameter vector param of a hyperbolic distribution,
  this function calculates the range outside of which the distribution
  has negligible probability, or the density function is negligible, to
  a specified tolerance. The parameterization used
  is the \eqn{(\alpha, \beta)}{(alpha, beta)} one (see
  \code{\link{dhyperb}}). To use another parameterization, use
  \code{\link{hyperbChangePars}}.
}

\usage{
hyperbCalcRange(mu = 0, delta = 1, alpha = 1, beta = 0,
                param = c(mu, delta, alpha, beta),
                tol = 10^(-5), density = TRUE, ...)
}
\arguments{
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{param}{Value of parameter vector specifying the hyperbolic
    distribution. This takes the form \code{c(mu, delta, alpha, beta)}.}
  \item{tol}{Tolerance.}
  \item{density}{Logical. If \code{FALSE}, the bounds are for the probability
    distribution. If \code{TRUE}, they are for the density function.}
  \item{\dots}{Extra arguments for calls to \code{\link{uniroot}}.}
}

\details{
  The particular hyperbolic distribution being considered is specified
  by the value of the parameter value \code{param}.

  If \code{density = FALSE}, the function calculates
  the effective range of the distribution, which is used in calculating
  the distribution function and quantiles, and may be used in determining
  the range when plotting the distribution. By effective range is meant that
  the probability of an observation being greater than the upper end is
  less than the specified tolerance \code{tol}. Likewise for being smaller
  than the lower end of the range. Note that this has not been implemented
  yet.

  If \code{density = TRUE}, the function gives a range, outside of which
  the density is less than the given tolerance. Useful for plotting the
  density.
}

\value{
  A two-component vector giving the lower and upper ends of the range.
}
\references{ 
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions. 
  In \emph{Encyclopedia of Statistical Sciences}, 
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley. 
}
\author{David Scott \email{d.scott@auckland.ac.nz},
  Jennifer Tso, Richard Trendall}

\seealso{
  \code{\link{dhyperb}}, \code{\link{hyperbChangePars}}
}

\examples{
par(mfrow = c(1, 2))
param <- c(0, 1, 3, 1)
hyperbRange <- hyperbCalcRange(param = param, tol = 10^(-3))
hyperbRange
curve(phyperb(x, param = param), hyperbRange[1], hyperbRange[2])
maxDens <- dhyperb(hyperbMode(param = param), param = param)
hyperbRange <- hyperbCalcRange(param = param, tol = 10^(-3) * maxDens, density = TRUE)
hyperbRange
curve(dhyperb(x, param = param), hyperbRange[1], hyperbRange[2])
}
\keyword{distribution}
