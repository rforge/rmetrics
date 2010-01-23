\name{hyperbChangePars}
\alias{hyperbChangePars}

\title{Change Parameterizations of the Hyperbolic Distribution}

\description{
  This function interchanges between the following 4 parameterizations
  of the hyperbolic distribution:

  1. \eqn{\mu, \delta, \pi, \zeta}{mu, delta, pi, zeta}

  2. \eqn{\mu, \delta, \alpha, \beta}{mu, delta, alpha, beta}

  3. \eqn{\mu, \delta, \phi, \gamma}{mu, delta, phi, gamma}

  4. \eqn{\mu, \delta, \xi, \chi}{mu, delta, xi, chi}

  The first three are given in Barndorff-Nielsen and
  \enc{Blæsild}{Blaesild} (1983), and the fourth in Prause (1999)
}

\usage{
hyperbChangePars(from, to, param, noNames = FALSE)
}

\arguments{
  \item{from}{The set of parameters to change from.}
  \item{to}{The set of parameters to change to.}
  \item{param}{"from" parameter vector consisting of 4 numerical elements.}
  \item{noNames}{Logical. When \code{TRUE}, suppresses the parameter
    \code{names} in the output.}
}

\details{
  In the 4 parameterizations, the following must be positive:

  1. \eqn{\zeta, \delta}{zeta, delta}

  2. \eqn{\alpha, \delta}{alpha, delta}

  3. \eqn{\phi, \gamma,\delta}{phi, gamma, delta}

  4. \eqn{\xi, \delta}{xi, delta}

  Furthermore, note that in the second parameterization
  \eqn{\alpha}{alpha} must be greater than the absolute value of
  \eqn{\beta}{beta}, while in the fourth parameterization, \eqn{\xi}{xi}
  must be less than one, and the absolute value of \eqn{\chi}{chi} must
  be less than \eqn{\xi}{xi}.
}

\value{
  A numerical vector of length 4 representing \code{param} in the
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

\seealso{\code{\link{dhyperb}}}

\examples{
param1 <- c(2, 1, 3, 1)                   # Parameterization 1
param2 <- hyperbChangePars(1, 2, param1)   # Convert to parameterization 2
param2                                     # Parameterization 2
hyperbChangePars(2, 1, as.numeric(param2)) # Convert back to parameterization 1
}

\keyword{distribution}
