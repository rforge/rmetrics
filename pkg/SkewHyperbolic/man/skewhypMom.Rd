\name{skewhypMom}
\alias{skewhypMom}
\title{Calculate Moments of the Skew Hyperbolic Stuent t-Distribution.}
\description{
  This function can be used to calculate the raw moments, mu moments,
  central moments, and moments about any other given location for the
  skew hyperbolic \emph{t}-distribution.
}
\usage{
skewhypMom(order, mu = 0, delta = 1, beta = 1, nu = 1,
           param = c(mu,delta,beta,nu), momType = "raw", about = 0)
}
\arguments{
  \item{order}{Numeric. The order of the moment to be calculated. Not
    permitted to be a vector. Must be a positive integer, except for
    moments about 0.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Scale parameter \eqn{\delta}{delta}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
  \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form
    \code{c(mu,delta,beta,nu)}.}
  \item{momType}{Common types of moments to be calculated, default is
    \code{"raw"}, see \bold{Details}.}
  \item{about}{Numeric. The point around which the moment is to be
    calculated, default is zero. See \bold{Details}.}
}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones. In addition the
  parameter values are examined by calling the function
  \code{skewhypCheckPars} to see if they are valid.

  \code{order} is also checked by calling the function
  \code{is.wholenumber} in the \code{HyperbolicDist} package to
  see whether a whole number is given.

  \code{momType} can be either \code{"raw"} (moments about zero),
  \code{"mu"} (moments about mu), or \code{"central"} (moments
  about the mean). If one of these types of moments is required there is
  no need to specify a value for \code{about}. For moments about any
  other location \code{about} must be specified. In the case that both
  \code{momType} and \code{about} are specified and contradicting, the
  function will calculate the moments based on the value of
  \code{about}.

  To calculate the moments of the skew hyperbolic \emph{t}-distribution,
  the function first calculates the mu moments by the formula defined
  below, and then transforms them to any of the other types of moment by
  calling \code{momChangeAbout} in the \code{HyperbolicDist}
  package.

  The mu moments of the skew hyperbolic \emph{t}-distribution are given
  by:

  \deqn{\bar{M}_k = \sum\limits_{\ell = \lfloor(k+1)/2\rfloor}^{k}
        a_{k, \ell} \beta^{2\ell - k} \left[\frac{\delta^{2\ell}
          \Gamma(\nu/2 - \ell)}{ \Gamma(\nu/2) 2^{\ell} }\right]}{%
	M_k = sum_{l = [(k+1)/2]}^{k}
	a_{k,l} beta^{2l - k} [delta^{2l}Gamma(nu/2 - l) )/
	(Gamma(nu/2) 2^{l})]}

  where \eqn{k = \code{order}}{k = order} and \eqn{k > 0}{k > 0} and
  \eqn{a_{k, \ell}}{a_{k, l}} is the recursive coefficient (see
  \code{\link[HyperbolicDist]{momRecursion}} for details).

  This formula is given in Scott, \enc{Würtz}{Wuertz} and Tran
  (2008). Note that the [.] part of this formula is actually equivalent
  to the formula for the raw moments of the inverse gamma distribution,
  so the function calls \code{gammaRawMom} in the \code{HyperbolicDist}
  package when implementing the computations.
}
\value{ 
  The function returns the moment specified. In the case of raw moments,
  \code{Inf} is returned if the moment is infinite.
}
\references{

  Paolella, Marc S. (2007) Intermediate Probability: A Computational
  Approach, Chichester: Wiley

  Scott, D. J., \enc{Würtz}{Wuertz}, D. and Tran, T. T. (2008)
  Moments of the Generalized Hyperbolic Distribution. Preprint.
}
\seealso{ \code{\link{skewhypCheckPars}}, \code{\link{skewhypMean}},
  \code{\link[HyperbolicDist]{is.wholenumber}},
  \code{\link[HyperbolicDist]{momRecursion}},
  \code{\link[HyperbolicDist]{momChangeAbout}} and
  \code{\link[HyperbolicDist]{gigMom}}.
}
\examples{
param = c(1,2,3,10)
##Raw moments of the skew hyperbolic t distribution
skewhypMom(3, param = param, momType = "raw")
##Mu moments
skewhypMom(3, param = param, momType = "mu")
##Central moments
skewhypMom(3, param = param, momType = "central")
##Moments about any location
skewhypMom(3, param = param, about = 5)
}
\author{David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson}
\keyword{distribution}
