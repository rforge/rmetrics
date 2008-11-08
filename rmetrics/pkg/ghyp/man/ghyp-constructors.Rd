\name{ghyp-constructors}
\alias{ghyp}
\alias{hyp}
\alias{NIG}
\alias{student.t}
\alias{VG}
\alias{gauss}
\alias{ghyp.ad}
\alias{hyp.ad}
\alias{NIG.ad}
\alias{student.t.ad}
\alias{VG.ad}
\title{Create generalized hyperbolic distribution objects}
\description{

  Constructor functions for univariate and multivariate generalized
  hyperbolic distribution objects and their special cases in one of the
  parametrizations \dQuote{chi/psi}, \dQuote{alpha.bar} and
  \dQuote{alpha/delta}.

}
% <---------------------------------------------------------------------->
\usage{
ghyp(lambda = 0.5, chi = 0.5, psi = 2, mu = 0, sigma = diag(rep(1, length(mu))),
     gamma = rep(0, length(mu)), alpha.bar = NULL, data = NULL)

ghyp.ad(lambda = 0.5, alpha = 1.5, delta = 1, beta = rep(0, length(mu)),
        mu = 0, Delta = diag(rep(1, length(mu))), data = NULL)


hyp(chi = 0.5, psi = 2, mu = 0, sigma = diag(rep(1, length(mu))),
    gamma = rep(0, length(mu)), alpha.bar = NULL, data = NULL)

hyp.ad(alpha = 1.5, delta = 1, beta = rep(0, length(mu)), mu = 0,
       Delta = diag(rep(1, length(mu))), data = NULL)


NIG(chi = 2, psi = 2, mu = 0, sigma = diag(rep(1, length(mu))),
    gamma = rep(0, length(mu)), alpha.bar = NULL, data = NULL)

NIG.ad(alpha = 1.5, delta = 1, beta = rep(0, length(mu)), mu = 0,
       Delta = diag(rep(1, length(mu))), data = NULL)


student.t(nu = 3.5, mu = 0, sigma = diag(rep(1, length(mu))),
          gamma = rep(0, length(mu)), data = NULL)

student.t.ad(lambda = -2, delta = 1, beta = rep(0, length(mu)), mu = 0,
             Delta = diag(rep(1, length(mu))), data = NULL)


VG(lambda = 1, psi = 2*lambda, mu = 0, sigma = diag(rep(1, length(mu))),
   gamma = rep(0, length(mu)), data = NULL)

VG.ad(lambda = 2, alpha = 1.5, beta = rep(0, length(mu)), mu = 0,
      Delta = diag(rep(1, length(mu))), data = NULL)

gauss(mu = 0, sigma = diag(rep(1, length(mu))), data = NULL)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{lambda}{Shape parameter. Common for all parametrizations.}
  \item{nu}{Shape parameter only used in case of a Student-t distribution in the \dQuote{chi/psi} and \dQuote{alpha.bar} parametrization . It determines
            the degree of freedom.}
  \item{chi}{Shape parameter of the \dQuote{chi/psi} parametrization.}
  \item{psi}{Shape parameter of the \dQuote{chi/psi} parametrization.}
  \item{alpha}{Shape parameter of the \dQuote{alpha/delta} parametrization.}
  \item{delta}{Shape parameter of the \dQuote{alpha/delta} parametrization.}
  \item{alpha.bar}{Shape parameter of the \dQuote{alpha.bar} parametrization.
                   Supplying \dQuote{alpha.bar} makes the parameters \dQuote{chi} and \dQuote{psi}
                   redundant.}
  \item{mu}{Location parameter. Either a scalar or a vector. Common for all parametrizations.}
  \item{sigma}{Dispersion parameter of the \dQuote{chi/psi} parametrization. Either a scalar or a matrix.}
  \item{Delta}{Dispersion parameter. Must be a matrix with a determinant of 1.
               This parameter is only used in the multivariate case of the \dQuote{alpha.beta} parametrization.}
  \item{gamma}{Skewness parameter of the \dQuote{chi/psi} parametrization. Either a scalar or a vector.}
  \item{beta}{Skewness parameter of the \dQuote{alpha/delta} parametrization. Either a scalar or a vector.}
  \item{data}{An object coercible to a \code{vector} (univariate case)
              or \code{matrix} (multivariate case).}
}
% <---------------------------------------------------------------------->
\details{

  These functions serve as constructors for univariate and multivariate
  generalized hyperbolic distribution objects and the special cases of
  the generalized hyperbolic distribution. \cr


  \code{ghyp}, \code{hyp} and \code{NIG} can be called either with the
  \dQuote{chi/psi} or the \dQuote{alpha.bar} parametrization.  Whenever
  \code{alpha.bar} is not \code{NULL} it is assumed that the
  \dQuote{alpha.bar} parametrization is used and the parameters
  \dQuote{chi} and \dQuote{psi} become redundant. \cr


  \code{ghyp.ad}, \code{hyp.ad}, \code{NIG.ad}, \code{student.t.ad} and
  \code{VG.ad} use the \dQuote{alpha/delta} parametrization.  Have a
  look on the vignette of this package in the \code{doc} folder for
  further information regarding the parametrization and for the domains
  of variation of the parameters.

}
% <---------------------------------------------------------------------->
\value{
 An object of class \code{\link[=ghyp-class]{ghyp}}.
}
% <---------------------------------------------------------------------->
\author{ David Luethi }
% <---------------------------------------------------------------------->
\references{
  \code{ghyp}-package vignette in the \code{doc} folder or on \url{http://cran.r-project.org/web/packages/ghyp/}
}
% <---------------------------------------------------------------------->
\note{

  The Student-t parametrization obtained via the \dQuote{alpha.bar}
  parametrization slightly differs from the common Student-t
  parametrization: The parameter \code{sigma} denotes the standard
  deviation in the univariate case and the variance in the multivariate
  case.  Thus, set \eqn{\sigma = \sqrt{\nu /(\nu - 2)}}{sigma = sqrt(nu
  /(nu - 2)} in the univariate case to get the same results as with the
  standard \R implementation of the Student-t distribution (see
  \bold{Examples}).  Have a look on the vignette of this package in the
  \code{doc} folder for further information. \cr

  Once an object of class \code{\link[=ghyp-class]{ghyp}} is created the
  methods \code{Xghyp} have to be used even when the distribution is a
  special case of the generalized hyperbolic distribution.  E.g. do not
  use \code{dVG}.  Use \code{\link{dghyp}} and submit a variance gamma
  distribution created with \code{VG()}.

}
% <---------------------------------------------------------------------->
\seealso{
  \code{\link{ghyp-class}} for a summary of generic methods assigned to \code{ghyp} objects,
  \code{\link[=coef.ghyp]{coef}} for switching between different parametrizations,
  \code{\link[=dghyp]{d/p/q/r/ES/gyhp}} for density, distribution function et cetera,
  \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}} for fitting routines.
}
% <---------------------------------------------------------------------->
\examples{
  ## alpha.bar parametrization of a univariate generalized hyperbolic distribution
  ghyp(lambda=2, alpha.bar=0.1, mu=0, sigma=1, gamma=0)
  ## lambda/chi parametrization of a univariate generalized hyperbolic distribution
  ghyp(lambda=2, chi=1, psi=0.5, mu=0, sigma=1, gamma=0)
  ## alpha/delta parametrization of a univariate generalized hyperbolic distribution
  ghyp.ad(lambda=2, alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a multivariate generalized hyperbolic distribution
  ghyp(lambda=1, alpha.bar=0.1, mu=2:3, sigma=diag(1:2), gamma=0:1)
  ## lambda/chi parametrization of a multivariate generalized hyperbolic distribution
  ghyp(lambda=1, chi=1, psi=0.5, mu=2:3, sigma=diag(1:2), gamma=0:1)
  ## alpha/delta parametrization of a multivariate generalized hyperbolic distribution
  ghyp.ad(lambda=1, alpha=2.5, delta=1, mu=2:3, Delta=diag(c(1,1)), beta=0:1)

  ## alpha.bar parametrization of a univariate hyperbolic distribution
  hyp(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate hyperbolic distribution
  hyp(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate hyperbolic distribution
  hyp.ad(alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate normal inverse gaussian distribution
  NIG(alpha.bar=0.3, mu=1, sigma=0.1, gamma=0)
  ## lambda/chi parametrization of a univariate normal inverse gaussian distribution
  NIG(chi=1, psi=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate normal inverse gaussian distribution
  NIG.ad(alpha=0.5, delta=1, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate variance gamma distribution
  VG(lambda=2, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate variance gamma distribution
  VG.ad(lambda=2, alpha=0.5, mu=0, beta=0)

  ## alpha.bar parametrization of a univariate Student-t distribution
  student.t(nu = 3, mu=1, sigma=0.1, gamma=0)
  ## alpha/delta parametrization of a univariate Student-t distribution
  student.t.ad(lambda=-2, delta=1, mu=0, beta=1)

  ## Obtain equal results as in the R-core parametrization of the Student-t distribution
  nu <- 2.1
  t.obj <- student.t(nu = nu, sigma = sqrt(nu / (nu - 2)))
  dat <- 0.1 * 1:9
  dt(dat, nu)
  dghyp(dat, t.obj)
  pt(dat, nu)
  pghyp(dat, t.obj)
}
% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{multivariate}
\keyword{models}
