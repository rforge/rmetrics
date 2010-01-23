\name{ghypMom}

\alias{ghypMom}


\title{Calculate Moments of the Generalized Hyperbolic Distribution}
\description{
  Function to calculate raw moments, mu moments, central moments and
  moments about any other given location for the generalized hyperbolic
  distribution. 
}
\usage{
ghypMom(order, mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
        param = c(mu, delta, alpha, beta, lambda),
        momType = c("raw", "central", "mu"), about = 0)
}

\arguments{
  \item{order}{Numeric. The order of the moment to be calculated. Not
    permitted to be a vector. Must be a positive whole number except for
    moments about zero.}
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
  \item{lambda}{\eqn{\lambda}{lambda} is the shape parameter and dictates the
    shape that the distribution shall take. Default value is 1.}
  \item{param}{Numeric. The parameter vector specifying the GHyp
    distribution. Of the form \code{c(mu, delta, alpha, beta, lambda)}
    (see \code{\link{dghyp}}).}
  \item{momType}{Common types of moments to be calculated, default is "raw". 
    See \bold{Details}.}  
  \item{about}{Numeric. The point around which the moment is to be
    calculated.}}
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
  function firstly calculates mu moments by formula defined below and
  then transforms mu moments to central moments or raw moments or
  moments about any other locations as required by calling
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
  \code{\link{ghypKurt}}.}


\examples{
param <- c(1, 2, 2, 1, 2)
mu <- param[1]
### mu moments
m1 <- ghypMean(param = param)
m1 - mu
ghypMom(1, param = param, momType = "mu")
momIntegrated("ghyp", order = 1, param = param, about = mu)
ghypMom(2, param = param, momType = "mu")
momIntegrated("ghyp", order = 2, param = param, about = mu)
ghypMom(10, param = param, momType = "mu")
momIntegrated("ghyp", order = 10, param = param, about = mu)

### raw moments
ghypMean(param = param)
ghypMom(1, param = param, momType = "raw")
momIntegrated("ghyp", order = 1, param = param, about = 0)
ghypMom(2, param = param, momType = "raw")
momIntegrated("ghyp", order = 2, param = param, about = 0)
ghypMom(10, param = param, momType = "raw")
momIntegrated("ghyp", order = 10, param = param, about = 0)

### central moments
ghypMom(1, param = param, momType = "central")
momIntegrated("ghyp", order = 1, param = param, about = m1)
ghypVar(param = param)
ghypMom(2, param = param, momType = "central")
momIntegrated("ghyp", order = 2, param = param, about = m1)
ghypMom(10, param = param, momType = "central")
momIntegrated("ghyp", order = 10, param = param, about = m1)
}
\keyword{distribution}
