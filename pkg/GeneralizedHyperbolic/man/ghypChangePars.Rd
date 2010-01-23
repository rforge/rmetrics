\name{ghypChangePars}
\alias{ghypChangePars}

\title{Change Parameterizations of the Generalized Hyperbolic Distribution}

\description{
  This function interchanges between the following 4 parameterizations
  of the generalized hyperbolic distribution:

  1. \eqn{\mu, \delta, \alpha, \beta, \lambda}{mu, delta, alpha, beta, lambda}

  2. \eqn{\mu, \delta, \zeta, \rho, \lambda}{mu, delta, zeta, rho, lambda}

  3. \eqn{\mu, \delta, \xi, \chi, \lambda}{mu, delta, xi, chi, lambda}

  4. \eqn{\mu, \delta, \bar\alpha, \bar\beta, \lambda}{%
    mu, delta, alpha bar, beta bar, lambda}

  These are the parameterizations given in Prause (1999)
}

\usage{
ghypChangePars(from, to, param, noNames = FALSE)
}

\arguments{
  \item{from}{The set of parameters to change from.}
  \item{to}{The set of parameters to change to.}
  \item{param}{"from" parameter vector consisting of 5 numerical elements.}
  \item{noNames}{Logical. When \code{TRUE}, suppresses the parameter
    \code{names} in the output.}
}

\details{
  In the 4 parameterizations, the following must be positive:

  1. \eqn{\alpha, \delta}{alpha, delta}

  2. \eqn{\zeta, \delta}{zeta, delta}

  3. \eqn{\xi, \delta}{xi, delta}

  4. \eqn{\bar\alpha, \delta}{alpha bar, delta}

  Furthermore, note that in the first parameterization
  \eqn{\alpha}{alpha} must be greater than the absolute value of
  \eqn{\beta}{beta}; in the third parameterization, \eqn{\xi}{xi}
  must be less than one, and the absolute value of \eqn{\chi}{chi} must
  be less than \eqn{\xi}{xi}; and in the fourth parameterization,
  \eqn{\bar\alpha}{alpha bar} must be greater than the absolute value of
  \eqn{\bar\beta}{beta bar}.
}

\value{
  A numerical vector of length 5 representing \code{param} in the
  \code{to} parameterization.
}

\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P. (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.

  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
Jennifer Tso, Richard Trendall}

\seealso{\code{\link{dghyp}}}

\examples{
param1 <- c(0, 3, 2, 1, 2)               # Parameterization 1
param2 <- ghypChangePars(1, 2, param1)   # Convert to parameterization 2
param2                                   # Parameterization 2
ghypChangePars(2, 1, as.numeric(param2)) # Convert back to parameterization 1
}

\keyword{distribution}
