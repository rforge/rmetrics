\name{ghyp-internal}
\alias{ghyp.internal}
\title{Internal ghyp functions}
% <---------------------------------------------------------------------->
\description{

  Internal ghyp functions. These functions are not to be called by the
  user.

}
\usage{
.abar2chipsi(alpha.bar, lambda, eps = .Machine$double.eps)

.besselM3(lambda = 9/2, x = 2, logvalue = FALSE)

.check.data(data, case = c("uv", "mv"), na.rm = TRUE, fit = TRUE, dim = NULL)

.check.gig.pars(lambda, chi, psi)

.check.norm.pars(mu, sigma, gamma, dimension)

.check.opt.pars(opt.pars, symmetric)

.fit.ghyp(object, llh = 0, n.iter = 0, converged = FALSE, error.code = 0,
         error.message = "", parameter.variance, fitted.params, aic,
         trace.pars = list())

.ghyp.model(lambda, chi, psi, gamma)

.t.transform(lambda)

.inv.t.transform(lambda.transf)

.integrate.moment.gig(x, moment = 1, ...)

.integrate.moment.ghypuv(x, moment = 1, ...)

.dghypuv(x, lambda = 1, chi = 1, psi = 1, alpha.bar = NULL,
         mu = 1, sigma = 1, gamma = 0, logvalue = FALSE)

.dghypmv(x, lambda, chi, psi, mu, sigma, gamma, logvalue = FALSE)

.mle.default(data, pdf, vars, opt.pars = rep(TRUE, length(vars)),
            transform = NULL, se = FALSE,
            na.rm = FALSE, silent = FALSE, ...)

.p.default(q, pdf, pdf.args, lower, upper, ...)

.q.default(p, pdf, pdf.args, interval, p.lower, ...)

.test.ghyp(object, case = c("ghyp", "univariate", "multivariate"))

.is.gaussian(object)

.is.univariate(object)

.is.symmetric(object)

.is.student.t(object, symmetric = NULL)

.get.stepAIC.ghyp(stepAIC.obj, dist = c("ghyp", "hyp", "NIG", "VG", "t", "gauss"),
                 symmetric = FALSE)
}
% <---------------------------------------------------------------------->
\details{
\code{.abar2chipsi} \cr
  Convert \dQuote{alpha.bar} to \dQuote{chi} and \dQuote{psi} when using the
  \dQuote{alpha.bar} parametrization.

\code{.besselM3} \cr
  Wrapper function for \code{\link{besselK}}.

\code{.check.data} \cr
  This function checks \code{data} for consistency.
  Only data objects of typ \code{data.frame},
  \code{matrix} or \code{numeric} are accepted.

\code{.check.gig.pars} \cr
  Some combinations of the GIG parameters are not allowed. This
  function checks whether this is the case or not.

\code{.check.norm.pars} \cr
This function simply checks if the dimensions match.

\code{.check.opt.pars} \cr When calling the fitting routines
  (\code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}}) a named vector
  containing the parameters which should not be fitted can be passed.
  By default all parameters will be fitted.

\code{.fit.ghyp} \cr
  This function is called by the functions
  \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}} to create
  objects of class \code{\link[=mle.ghyp-class]{mle.ghyp}} and
  \code{\link[=mle.ghyp-class]{mle.ghyp}}.

\code{.ghyp.model} \cr
  Check if the parameters denote a special case of the generalized hyperbolic
  distribution.

\code{.t.transfrom} \cr
  Transformation  function used in \code{\link{fit.ghypuv}} for
  parameter nu belonging to the Student-t distribution.

\code{.inv.t.transfrom} \cr
  The inverse of \code{t.transfrom}.

\code{.integrate.moment.gig} \cr
  This function is used when computing the conditional expectation
  of a generalized inverse gaussian distribution.

\code{.integrate.moment.ghypuv} \cr
  This function is used when computing the conditional expectation
  of a univariate generalized hyperbolic distribution.

\code{.dghypuv} \cr This function is used during the fitting
  procedure. Use \code{\link{dghyp}} to compute the density of
  generalized hyperbolic distribution objects.

\code{.dghypmv} \cr This function is used during the fitting
  procedure. Use \code{\link{dghyp}} to compute the density of
  generalized hyperbolic distribution objects.

\code{.mle.default} \cr This function serves as a generic function for
  maximum likelihood estimation.  It is for internal use only. See
  \code{\link{fit.ghypuv}} which wraps this function.

\code{.p.default} \cr
  A generic distribution function integrator given a density function.
  See \code{\link{pghyp}} for a wrapper of this
  function.

\code{.q.default} \cr
  A generic quantile function calculator given a density function.
  See \code{\link{qghyp}} for a wrapper of this   function.

\code{.test.ghyp} \cr This function tests whether the object is of class
  \code{\link[=ghyp-class]{ghyp}} and sometimes whether it is univariate
  or multivariate according to the argument \code{case} and states a
  corresponding error if not.

\code{.is.gaussian} \cr
  Tests whether the object is of a gaussian type.

\code{.is.symmetric} \cr
  Tests whether the object is symmetric.

\code{.is.student.t} \cr
  Tests whether the object describes a Student-t distribution.

\code{.is.univariate} \cr
  Tests whether the object is a univariate ghyp-distribution.

\code{.get.stepAIC.ghyp} \cr
  Returns a specific model from a list returned by \code{stepAIC.ghyp}
}
% <---------------------------------------------------------------------->
\author{Wolfgang Breymann, David Luethi}
% <---------------------------------------------------------------------->
\keyword{internal}
