\name{momIntegrated}
\alias{momIntegrated}

\title{Moments Using Integration}
\description{
  Calculates moments and absolute moments about a given location for the
  generalized hyperbolic and related distributions.
}
\usage{
momIntegrated(densFn, order, param = NULL, about = 0, absolute = FALSE)
}

\arguments{
  \item{densFn}{Character. The name of the density function whose
    moments are to be calculated. See \bold{Details}.}
  \item{order}{Numeric. The order of the moment or absolute moment to be
    calculated.}
  \item{param}{Numeric. A vector giving the parameter values for the
    distribution specified by \code{densFn}. If no \code{param} values
  are specified, then the default parameter values of each distribution
  are used instead.}
  \item{about}{Numeric. The point about which the moment is to be calculated.}
  \item{absolute}{Logical. Whether absolute moments or ordinary moments
    are to be calculated. Default is \code{FALSE}.}
}
\details{
  Denote the density function by \eqn{f}{f}. Then if
  \code{order}\eqn{=k}{=k} and \code{about}\eqn{=a}{=a},
  \code{momIntegrated} calculates
  \deqn{\int_{-\infty}^\infty (x - a)^k f(x) dx}{%
    integral_{-infinity}^infinity (x - a)^k f(x) dx}
  when \code{absolute = FALSE} and
  \deqn{\int_{-\infty}^\infty |x - a|^k f(x) dx}{%
    integral_{-infinity}^infinity |x - a|^k f(x) dx}
  when \code{absolute = TRUE}.

  Only certain density functions are permitted.

  When \code{densFn="ghyp"} or \code{"generalized hyperbolic"} the
  density used is \code{dghyp}. The default value for \code{param} is
  \code{c(1,1,0,1,0)}.

  When \code{densFn="hyperb"} or \code{"hyperbolic"} the density used is
  \code{dhyperb}. The default value for \code{param} is
  \code{c(0,1,1,0)}.

  When \code{densFn="gig"} or \code{"generalized inverse gaussian"} the
  density used is \code{dgig}. The default value for \code{param} is
  \code{c(1,1,1)}.

  When \code{densFn="gamma"} the density used is \code{dgamma}. The
  default value for \code{param} is \code{c(1,1)}.

  When \code{densFn="invgamma"} or \code{"inverse gamma"} the
  density used is the density of the inverse gamma distribution given by
   \deqn{f(x) = \frac{u^\alpha e^{-u}}{x \Gamma(\alpha)}, %
    \quad u = \theta/x}{%
      f(x) = u^alpha exp(-u)/(x Gamma(alpha)), u = theta/x}

  for \eqn{x > 0}, \eqn{\alpha > 0}{alpha > 0} and
  \eqn{\theta > 0}{theta > 0}.  The parameter vector
  \code{param = c(shape,rate)} where \code{shape} \eqn{=\alpha}{=alpha} and
  \code{rate}\eqn{=1/\theta}{=1/theta}. The default value for
  \code{param} is \code{c(-1,1)}.

  When \code{densFn="vg"} or \code{"variance gamma"} the density used is
  \code{dvg} from the package \pkg{VarianceGamma}. In this case, the package
  \pkg{VarianceGamma} must be loaded or an error will result. The
  default value for \code{param} is \code{c(0,1,0,1)}.

}
\value{
  The value of the integral as specified in \bold{Details}.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, 
        Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{\code{\link{dghyp}}, \code{\link{dhyperb}},
  \code{\link{dgamma}}, \code{\link{dgig}},
  \code{\link[VarianceGamma]{dvg}}
}
\examples{
### Calculate the mean of a generalized hyperbolic distribution
### Compare the use of integration and the formula for the mean
m1 <- momIntegrated("ghyp", param = c(1/2,3,1,1,0), order = 1, about = 0)
m1
ghypMean(c(1/2,3,1,1,0))
### The first moment about the mean should be zero
momIntegrated("ghyp", order = 1, param = c(1/2,3,1,1,0), about = m1)
### The variance can be calculated from the raw moments
m2 <- momIntegrated("ghyp", order = 2, param = c(1/2,3,1,1,0), about = 0)
m2
m2 - m1^2
### Compare with direct calculation using integration
momIntegrated("ghyp", order = 2, param = c(1/2,3,1,1,0), about = m1)
momIntegrated("generalized hyperbolic", param = c(1/2,3,1,1,0), order = 2,
              about = m1)
### Compare with use of the formula for the variance
ghypVar(c(1/2,3,1,1,0))
}
\keyword{distribution}
\keyword{univar}
