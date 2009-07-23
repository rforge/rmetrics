\name{gigMom}
\alias{gigMom}
\alias{gigRawMom}
\alias{gammaRawMom}

\title{Calculate Moments of the Generalized Inverse Gaussian Distribution}
\description{
  Functions to calculate raw moments and moments about a given location
  for the generalized inverse Gaussian (GIG) distribution, including the
  gamma and inverse gamma distributions as special cases.
}
\usage{
gigRawMom(order, Theta)
gigMom(order, Theta, about = 0)
gammaRawMom(order, shape = 1, rate = 1, scale = 1/rate)
}

\arguments{
  \item{order}{Numeric. The order of the moment to be calculated. Not
    permitted to be a vector. Must be a positive whole number except for
    moments about zero.}
  \item{Theta}{Numeric. The parameter vector specifying the GIG
    distribution. Of the form \code{c(lambda, chi, psi)} (see
    \code{\link{dgig}}).}
  \item{about}{Numeric. The point around which the moment is to be
    calculated.}
  \item{shape}{Numeric. The shape parameter, must be non-negative, not
    permitted to be a vector.}
  \item{scale}{Numeric. The scale parameter, must be positive, not
    permitted to be a vector.}
  \item{rate}{Numeric. The rate parameter, an alternative way to specify
    the scale.}
}
\details{
  The vector \code{Theta} of parameters is examined using
  \code{\link{gigCheckPars}} to see if the paramters are valid for the GIG
  distribution and if they correspond to the special cases which are the
  gamma and inverse gamma distributions. Checking of special cases and
  valid parameter vector values is carried out using the function
  \code{\link{gigCheckPars}}. Checking whether \code{order}
  is a whole number is carried out using the function
  \code{\link{is.wholenumber}}.

  Raw moments (moments about zero) are calculated using the
  functions \code{gigRawMom} or \code{gammaRawMom}. For moments not
  about zero, the function \code{\link{momChangeAbout}} is used to
  derive moments about another point from raw moments. Note that raw moments
  of the inverse gamma distribution can be obtained from the raw moments
  of the gamma distribution because of the relationship between the two
  distributions. An alternative implementation of raw moments of the gamma
  and inverse gamma distributions may be found in the package
  \pkg{actuar} and these may be faster since they are written in C.

  To calculate the raw moments of the GIG distribution it is convenient to
  use the alternative parameterization of the GIG in terms of
  \eqn{\omega}{omega} and \eqn{\eta}{eta}, given as parameterization 3
  in \code{\link{gigChangePars}}. Then the raw moment of the GIG
  distribution of order \eqn{k}{k} is given by
  \deqn{\eta^k K_{\lambda+k}(\omega)/K_{\lambda}(\omega)}{%
        eta^k K_(lambda+k)(omega)/K_lambda(omega)}

  where \eqn{K_\lambda()}{K_lambda()} is the modified Bessel function of
  the third kind of order \eqn{\lambda}{lambda}.

  The raw moment of the gamma distribution of order \eqn{k}{k} with
  shape parameter \eqn{\alpha}{alpha} and rate parameter
  \eqn{\beta}{beta} is given by
  \deqn{\beta^{-k}\Gamma(\alpha+k)/\Gamma(\alpha)}{%
        beta^(-k)Gamma(alpha+k)/Gamma(alpha)}

  The raw moment of order \eqn{k}{k} of the inverse gamma distribution
  with shape parameter \eqn{\alpha}{alpha} and rate parameter
  \eqn{\beta}{beta} is the raw moment of order \eqn{-k}{-k} of the gamma
  distribution with shape parameter \eqn{\alpha}{alpha} and rate parameter
  \eqn{1/\beta}{1/beta}.
}
\value{
  The moment specified. In the case of raw moments, \code{Inf} is
  returned if the moment is infinite.
}
\references{
  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley
}
\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{\code{\link{gigCheckPars}}, \code{\link{gigChangePars}},
  \code{\link{is.wholenumber}}, \code{\link{momChangeAbout}},
  \code{\link{momIntegrated}}, \code{\link{gigMean}},
  \code{\link{gigVar}}, \code{\link{gigSkew}}, \code{\link{gigKurt}}.}


\examples{
### Raw moments of the generalized inverse Gaussian distribution
Theta <- c(-0.5,5,2.5)
gigRawMom(1, Theta)
momIntegrated("gig", order = 1, param = Theta, about = 0)
gigRawMom(2, Theta)
momIntegrated("gig", order = 2, param = Theta, about = 0)
gigRawMom(10, Theta)
momIntegrated("gig", order = 10, param = Theta, about = 0)
gigRawMom(2.5, Theta)

### Moments of the generalized inverse Gaussian distribution
Theta <- c(-0.5,5,2.5)
(m1 <- gigRawMom(1, Theta))
gigMom(1, Theta)
gigMom(2, Theta, m1)
(m2 <- momIntegrated("gig", order = 2, param = Theta, about = m1))
gigMom(1, Theta, m1)
gigMom(3, Theta, m1)
momIntegrated("gig", order = 3, param = Theta, about = m1)

### Raw moments of the gamma distribution
shape <- 2
rate <- 3
Theta <- c(shape, rate)
gammaRawMom(1, shape, rate)
momIntegrated("gamma", order = 1, param = Theta, about = 0)
gammaRawMom(2, shape, rate)
momIntegrated("gamma", order = 2, param = Theta, about = 0)
gammaRawMom(10, shape, rate)
momIntegrated("gamma", order = 10, param = Theta, about = 0)

### Moments of the inverse gamma distribution
Theta <- c(-0.5,5,0)
gigRawMom(2, Theta)              # Inf
gigRawMom(-2, Theta)
momIntegrated("invgamma", order = -2,
              param = c(-Theta[1],Theta[2]/2), about = 0)

### An example where the moment is infinite: inverse gamma
Theta <- c(-0.5,5,0)
gigMom(1, Theta)
gigMom(2, Theta)
}
\keyword{distribution}
