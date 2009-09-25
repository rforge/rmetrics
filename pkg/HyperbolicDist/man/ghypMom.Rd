\name{ghypMom}
\alias{ghypMom}

\title{Calculate Moments of the Generalized Hyperbolic Distribution}

\description{
  This function can be used to calculate raw moments, mu moments,
  central moments and moments about any other given location for the
  generalized hyperbolic distribution.
}

\usage{
ghypMom(order, Theta, momType = "raw", about = 0)
}

\arguments{
  \item{order}{Numeric. The order of the moment to be calculated. Not
    permitted to be a vector. Must be a positive whole number except for
    moments about zero.}
  \item{Theta}{Numeric. The parameter vector specifying the GIG
    distribution. Of the form \code{c(lambda, alpha, beta, delta, mu)}
    (see \code{\link{dghyp}}).}
  \item{momType}{Common types of moments to be calculated, default is "raw".
    See \bold{Details}.}
  \item{about}{Numeric. The point around which the moment is to be
    calculated, default is 0. See \bold{Details}.}
}

\details{
  Checking whether \code{order} is a whole number is carried out using
  the function \code{\link{is.wholenumber}}.

  \code{momType} can be either "raw" (moments about zero), "mu" (moments
  about mu), or "central" (moments about mean). If one of these moment
  types is specified, then there is no need to specify the \code{about}
  value. For moments about any other location, the \code{about} value
  must be specified. In the case that both \code{momType} and
  \code{about} are specified and contradicting, the function will always
  calculate the moments based on \code{about} rather than
  \code{momType}.

  To calculate moments of the generalized hyperbolic distribution, the
  function first calculates mu moments by the formula defined below and
  then transforms mu moments to central moments or raw moments or
  moments about any other location as required by calling
  \code{momChangeAbout}.

  The mu moments are obtained from the recursion formula given in Scott,
  \enc{Würtz}{Wuertz} and Tran (2008).
}

\value{
  The moment specified.
}

\references{
  Scott, D. J., \enc{Würtz}{Wuertz}, D. and Tran, T. T. (2008)
  Moments of the Generalized Hyperbolic Distribution. Preprint.
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{\code{\link{ghypChangePars}}, \code{\link{is.wholenumber}},
  \code{\link{momChangeAbout}}, \code{\link{momIntegrated}},
  \code{\link{ghypMean}}, \code{\link{ghypVar}}, \code{\link{ghypSkew}},
  \code{\link{ghypKurt}}.
}

\examples{
Theta <- c(2,2,1,2,1)
mu <- Theta[5]
### mu moments
(m1 <- ghypMean(Theta))
m1 - mu
ghypMom(1, Theta, momType = "mu")
momIntegrated("ghyp", order = 1, param = Theta, about = mu)
ghypMom(2, Theta, momType = "mu")
momIntegrated("ghyp", order = 2, param = Theta, about = mu)
ghypMom(10, Theta, momType = "mu")
momIntegrated("ghyp", order = 10, param = Theta, about = mu)

### raw moments
ghypMean(Theta)
ghypMom(1, Theta, momType = "raw")
momIntegrated("ghyp", order = 1, param = Theta, about = 0)
ghypMom(2, Theta, momType = "raw")
momIntegrated("ghyp", order = 2, param = Theta, about = 0)
ghypMom(10, Theta, momType = "raw")
momIntegrated("ghyp", order = 10, param = Theta, about = 0)

### central moments
ghypMom(1, Theta, momType = "central")
momIntegrated("ghyp", order = 1, param = Theta, about = m1)
ghypVar(Theta)
ghypMom(2, Theta, momType = "central")
momIntegrated("ghyp", order = 2, param = Theta, about = m1)
ghypMom(10, Theta, momType = "central")
momIntegrated("ghyp", order = 10, param = Theta, about = m1)

}
\keyword{distribution}
