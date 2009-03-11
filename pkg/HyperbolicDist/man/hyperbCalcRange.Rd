\name{hyperbCalcRange}
\alias{hyperbCalcRange}
\title{Range of a Hyperbolic Distribution}
\description{
  Given the parameter vector Theta of a hyperbolic distribution,
  this function calculates the range outside of which the distribution
  has negligible probability, or the density function is negligible, to
  a specified tolerance. The parameterization used
  is the \eqn{(\pi,\zeta)}{(pi,zeta)} one (see
  \code{\link{dhyperb}}). To use another parameterization, use
  \code{\link{hyperbChangePars}}.
}

\usage{
hyperbCalcRange(Theta, tol = 10^(-5), density = FALSE)
}
\arguments{
  \item{Theta}{Value of parameter vector specifying the hyperbolic
    distribution.}
  \item{tol}{Tolerance.}
  \item{density}{Logical. If \code{FALSE}, the bounds are for the probability
    distribution. If \code{TRUE}, they are for the density function.}
}

\details{
  The particular hyperbolic distribution being considered is specified
  by the value of the parameter value \code{Theta}.

  If \code{density = FALSE}, the function calculates
  the effective range of the distribution, which is used in calculating
  the distribution function and quantiles, and may be used in determining
  the range when plotting the distribution. By effective range is meant that
  the probability of an observation being greater than the upper end is
  less than the specified tolerance \code{tol}. Likewise for being smaller
  than the lower end of the range.

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
par(mfrow = c(1,2))
Theta <- c(3,5,1,0)
hyperbRange <- hyperbCalcRange(Theta, tol = 10^(-3))
hyperbRange
curve(phyperb(x, Theta), hyperbRange[1], hyperbRange[2])
maxDens <- dhyperb(hyperbMode(Theta), Theta)
hyperbRange <- hyperbCalcRange(Theta, tol = 10^(-3)*maxDens, density = TRUE)
hyperbRange
curve(dhyperb(x, Theta), hyperbRange[1], hyperbRange[2])
}
\keyword{distribution}
