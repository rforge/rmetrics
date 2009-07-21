% --- Source file: vgChangePars.Rd ---
\name{vgChangePars}
\alias{vgChangePars}

\title{Change Parameterizations of the Variance Gamma Distribution}

\description{
  This function interchanges between the following 4 parameterizations
  of the variance gamma distribution:

  1. \eqn{c, \sigma, \theta, \nu}{c, sigma, theta, nu}

  2. \eqn{\theta, \sigma, \mu, \tau}{theta, sigma, mu, tau}

  3. \eqn{\theta, \sigma, \kappa, \tau}{theta, sigma, kappa, tau}

  4. \eqn{\lambda, \alpha, \beta, \mu}{lambda, alpha, beta, mu}


  The first set of parameterizations is given in Seneta (2004). The second and
  third ones are the parameterizations given in Kotz \eqn{et al}. (2001). The
  last set takes the form of the generalized hyperbolic distribution
  parameterization. \eqn{\delta}{delta} is not included since the
  variance gamma distribution is a limiting case of generalized
  hyperbolic distribution with \eqn{\delta}{delta} always equal to 0.
}

\usage{
vgChangePars(from, to, param, noNames = FALSE)
}

\arguments{
  \item{from}{The set of parameters to change from.}
  \item{to}{The set of parameters to change to.}
  \item{param}{"from" parameter vector consisting of 4 numerical elements.}
  \item{noNames}{Logical. When \code{TRUE}, suppresses the parameter
    \code{names} in the output.}
}

\details{
  In the 3 parameterizations, the following must be positive:

  1. \eqn{\sigma, \nu}{sigma, nu}

  2. \eqn{\sigma, \tau}{sigma, tau}

  3. \eqn{\sigma, \tau}{sigma, tau}

  4. \eqn{\lambda, \alpha}{lambda, alpha}

  In addition in the 4th parameterization, the absolute value of
  \eqn{\beta}{beta} must be less than \eqn{\alpha}{alpha}.
}

\value{
  A numerical vector of length 4 representing \code{param} in the
  \code{to} parameterization.
}

\references{
  Seneta, E. (2004). Fitting the variance-gamma model to financial data.
  \emph{J. Appl. Prob.}, 41A:177--187.
  Kotz, S, Kozubowski, T. J., and \enc{Podgórski}{Podgorski},
  K. (2001).
  The Laplace Distribution and Generalizations. \emph{Birkhauser},
  Boston, 349 p.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{\code{\link{dvg}}, code{\link{vgMom}}}

\examples{
param1 <- c(2,2,1,3)                   # Parameterization 1
param2 <- vgChangePars(1, 2, param1)   # Convert to parameterization 2
param2                                 # Parameterization 2
vgChangePars(2, 1, as.numeric(param2)) # Convert back to parameterization 1

param3 <- c(1,2,0,0.5)                 # Parameterization 3
param1 <- vgChangePars(3, 1, param3)   # Convert to parameterization 1
param1                                 # Parameterization 1
vgChangePars(1, 3, as.numeric(param1)) # Convert back to parameterization 3
}
