\name{vgMom}
\alias{vgMom}

\title{Calculate Moments of the Variance Gamma Distribution}

\description{
  This function can be used to calculate raw moments, mu moments, central
  moments and moments about any other given location for the variance gamma
  (VG) distribution.
}

\usage{
  vgMom(order, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), momType = "raw", about = 0)
}

\arguments{
  \item{order}{Numeric. The order of the moment to be calculated. Not
    permitted to be a vector. Must be a positive whole number except for
    moments about zero.}
  \item{vgC}{The location parameter \eqn{c}{c}, default is 0.}
  \item{sigma}{The spread parameter \eqn{\sigma}{sigma}, default is 1,
    must be positive.}
  \item{theta}{The asymmetry parameter \eqn{\theta}{theta}, default is 0.}
  \item{nu}{The shape parameter \eqn{\nu}{nu}, default is 1, must be
    positive.}
  \item{param}{Specifying the parameters as a vector which takes the form
    \code{c(vgC,sigma,theta,nu)}.}
  \item{momType}{Common types of moments to be calculated, default is "raw".
    See \bold{Details}.}
  \item{about}{Numeric. The point around which the moment is to be
    calculated, default is 0. See \bold{Details}.}
}

\details{
  For the parameters of the variance gamma distribution, users may
  either specify the values individually or as a vector. If both forms
  are specified but with different values, then the values specified by
  vector \code{param} will always overwrite the other ones.  In
  addition, the parameters values are examined by calling the function
  \code{vgCheckPars} to see if they are valid for the VG distribution.

  \code{order} is also checked by calling the function
  \code{is.wholenumber} in \code{DistributionUtils} package to see whether
  a whole number is given.

  \code{momType} can be either "raw" (moments about zero), "mu"
  (moments about vgC), or "central" (moments about mean). If one of
  these moment types is specified, then there is no need to specify the
  \code{about} value.  For moments about any other location, the
  \code{about} value must be specified. In the case that both
  \code{momType} and \code{about} are specified and contradicting, the
  function will always calculate the moments based on \code{about}
  rather than \code{momType}.

  To calculate moments of the VG distribution, the function first
  calculates mu moments by the formula defined below and then transforms mu
  moments to central moments or raw moments or moments about any other
  location as required by calling \code{momChangeAbout} in
  \code{DistributionUtils} package.

  To calculate mu moments of the variance gamma distribution, the function
  first transforms the parameterization of
  \eqn{c,\sigma,\theta,\nu}{c,sigma,theta,nu} to the generalized hyperbolic
  distribution's parameterization of
  \eqn{\lambda, \alpha, \beta, \mu}{lambda,alpha,beta,mu}
  (see \code{\link{vgChangePars}} for details).
  Then, the mu moments of the variance gamma distribution are given by
  \deqn{\sum_{\ell = \lfloor(k+1)/2\rfloor}^{k}
        a_{k, \ell}
        \beta^{2\ell - k}
        \lfloor\Gamma(\lambda+\ell)/\Gamma(\lambda)
        2^\ell/(\alpha^2-\beta^2)^\ell\rfloor}{%
        sum_{l = [(k+1)/2]}^{k}
        a_{k, ell}
        beta^{2l - k}
        [Gamma(lambda+l)/Gamma(lambda)
        2^l/(alpha^2-beta^2)^l]}

  where \eqn{k = \code{order}}{k = order} and \eqn{k > 0}{k > 0} and
  \eqn{a_{k, \ell}}{a_{k, l}} is the recursive coefficient
  (see \code{\link[GeneralizedHyperbolic]{momRecursion}} for details).

  This formula is developed from the mu moments formula of the
  generalized hyperbolic distribution given in Scott,
  \enc{Würtz}{Wuertz} and Tran (2008).  Note that the part in \code{[]}
  of this equation is actually equivalent to the formula of raw moments
  of the gamma distribution. So the function calls \code{gammaRawMom} in
  \code{GeneralizedHyperbolic} package when implementing the computations.
}

\value{
  The moment specified. In the case of raw moments, \code{Inf} is
  returned if the moment is infinite.
}

\references{
  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley
  Scott, D. J., \enc{Würtz}{Wuertz}, D. and Tran, T. T. (2008)
  Moments of the Generalized Hyperbolic Distribution. Preprint.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{\code{\link{vgCheckPars}}, \code{\link{vgChangePars}},
  \code{\link{vgMean}},
  \code{\link{vgVar}}, \code{\link{vgSkew}}, \code{\link{vgKurt}},
  \code{\link[GeneralizedHyperbolic]{is.wholenumber}},
  \code{\link[GeneralizedHyperbolic]{momRecursion}},
  \code{\link[GeneralizedHyperbolic]{momChangeAbout}} and
  \code{\link[GeneralizedHyperbolic]{momIntegrated}}.}


\examples{
  ### Raw moments of the VG distribution
  vgMom(3, param=c(2,1,2,1), momType = "raw")

  ### Mu moments of the VG distribution
  vgMom(2, param=c(2,1,2,1), momType = "mu")

  ### Central moments of the VG distribution
  vgMom(4, param=c(2,1,2,1), momType = "central")

  ### Moments about any locations
  vgMom(4, param=c(2,1,2,1), about = 1)
}
\keyword{distribution}
