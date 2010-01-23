\name{ghypCalcRange}
\alias{ghypCalcRange}
\title{Range of a Generalized Hyperbolic Distribution}
\description{
  Given the parameter vector Theta of a generalized hyperbolic distribution,
  this function determines the range outside of which the density
  function is negligible, to a specified tolerance. The parameterization used
  is the \eqn{(\alpha, \beta)}{(alpha, beta)} one (see
  \code{\link{dghyp}}). To use another parameterization, use
  \code{\link{ghypChangePars}}.
}

\usage{
ghypCalcRange(mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
              param = c(mu, delta, alpha, beta, lambda),
              tol = 10^(-5), density = TRUE, ...)
}
\arguments{
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{lambda}{\eqn{\lambda}{lambda} is the shape parameter and dictates the
    shape that the distribution shall take. Default value is 1.}
  \item{param}{Value of parameter vector specifying the generalized
    hyperbolic distribution. This takes the form \code{c(mu, delta,
    alpha, beta, lambda)}.}
  \item{tol}{Tolerance.}
  \item{density}{Logical. If \code{TRUE}, the bounds are for the density
    function. If \code{FALSE}, they should be for the probability
    distribution, but this has not yet been implemented.}
  \item{\dots}{Extra arguments for calls to \code{\link{uniroot}}.}
}

\details{
  The particular generalized hyperbolic distribution being considered is
  specified by the value of the parameter value \code{param}.

  If \code{density = TRUE}, the function gives a range, outside of which
  the density is less than the given tolerance. Useful for plotting the
  density. Also used in determining break points for the separate
  sections over which numerical integration is used to determine the
  distribution function. The points are found by using
  \code{\link{uniroot}} on the density function.

  If \code{density = FALSE}, the function returns the message:
  "\code{Distribution function bounds not yet implemented}".
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
\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{
  \code{\link{dghyp}}, \code{\link{ghypChangePars}}
}

\examples{
param <- c(0, 1, 5, 3, 1)
maxDens <- dghyp(ghypMode(param = param), param = param)
ghypRange <- ghypCalcRange(param = param, tol = 10^(-3) * maxDens)
ghypRange
curve(dghyp(x, param = param), ghypRange[1], ghypRange[2])
\dontrun{ghypCalcRange(param = param, tol = 10^(-3), density = FALSE)}
}
\keyword{distribution}
