\name{ghyp-get}
\alias{ghyp.data}
\alias{ghyp.name}
\alias{ghyp.fit.info}
\alias{ghyp.dim}
\title{Get methods for objects inheriting from class ghyp}
% <---------------------------------------------------------------------->
\description{

  These functions simply return data stored within generalized
  hyperbolic distribution objects, i.e. slots of the classes \code{ghyp}
  and \code{mle.ghyp}.  \code{ghyp.fit.info} extracts information about
  the fitting procedure from objects of class
  \code{\link[=mle.ghyp-class]{mle.ghyp}}. \code{ghyp.data} returns the
  data slot of a \code{gyhp} object. \code{ghyp.dim} returns the
  dimension of a \code{gyhp} object. \code{ghyp.name} returns the
  name of the distribution of a \code{gyhp} object.

}
% <---------------------------------------------------------------------->
\usage{
ghyp.fit.info(object)

ghyp.data(object)

ghyp.name(object, abbr = FALSE, skew.attr = TRUE)

ghyp.dim(object)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{An object inheriting from class
               \code{\link[=ghyp-class]{ghyp}}.}
  \item{abbr}{If \code{TRUE} the abbreviation of the \code{ghyp} distribution will be returned.}
  \item{skew.attr}{If \code{TRUE} an attribute will be added to the name of the \code{ghyp} distribution stating whether the distribution is symmetric or not.}
}
% <---------------------------------------------------------------------->
\value{
  \code{ghyp.fit.info} returns list with components:
  \tabular{rl}{
  \code{logLikelihood} \tab  The maximized log-likelihood value.\cr
  \code{aic} \tab  The Akaike information criterion.\cr
  \code{fitted.params} \tab  A boolean vector stating which parameters were fitted.\cr
  \code{converged} \tab  A boolean whether \code{\link{optim}} converged or not.\cr
  \code{n.iter} \tab The number of iterations. \cr
  \code{error.code} \tab Error code from \code{\link{optim}}.\cr
  \code{error.message} \tab Error message from \code{\link{optim}}.\cr
  \code{parameter.variance} \tab Parameter variance (only for univariate fits).\cr
  \code{trace.pars} \tab Trace values of the parameters during the fitting procedure.\cr
  }

  \code{ghyp.data} returns \code{NULL} if no data is stored within the
  object, a \code{vector} if it is an univariate generalized hyperbolic
  distribution and \code{matrix} if it is an multivariate generalized
  hyperbolic distribution.\cr

  \code{ghyp.name} returns the name of the \code{ghyp} distribution which can be the name of a special case.
  Depending on the arguments \code{abbr} and \code{skew.attr} one of the following is returned.
  \tabular{ll}{
  \code{abbr == FALSE & skew.attr == TRUE} \tab  \code{abbr == TRUE & skew.attr == TRUE} \cr
  (A)symmetric Generalized Hyperbolic \tab   (A)symm ghyp \cr
  (A)symmetric Hyperbolic \tab   (A)symm hyp \cr
  (A)symmetric Normal Inverse Gaussian \tab   (A)symm NIG \cr
  (A)symmetric Variance Gamma \tab   (A)symm VG \cr
  (A)symmetric Student-t \tab   (A)symm t \cr
  Gaussian \tab   Gauss \cr
  \code{abbr == FALSE & skew.attr == FALSE} \tab  \code{abbr == TRUE & skew.attr == FALSE} \cr
  Generalized Hyperbolic \tab ghyp  \cr
  Hyperbolic \tab   hyp \cr
  Normal Inverse Gaussian \tab   NIG \cr
  Variance Gamma \tab   VG \cr
  Student-t \tab   t \cr
  Gaussian \tab   Gauss \cr
  }

  \code{ghyp.dim} returns the dimension of a \code{ghyp} object.\cr

}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\note{

  \code{ghyp.fit.info} requires an object of class
  \code{\link[=mle.ghyp-class]{mle.ghyp}}. In the univariate case the
  parameter variance is returned as well. The parameter variance is
  defined as the inverse of the negative hesse-matrix computed by
  \code{\link{optim}}. Note that this makes sense only in the case that
  the estimates are asymptotically normal distributed. \cr

  The class \code{\link[=ghyp-class]{ghyp}} contains a \code{data} slot.
  Data can be stored either when an object is initialized or via the
  fitting routines and the argument \code{save.data}.

}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=coef.ghyp]{coef}}, \code{\link[=mean.ghyp]{mean}}, \code{\link[=vcov.ghyp]{vcov}},
         \code{\link[=logLik.mle.ghyp]{logLik}}, \code{\link[=AIC.mle.ghyp]{AIC}} for other accessor functions,
         \code{\link{fit.ghypmv}}, \code{\link{fit.ghypuv}}, \code{\link{ghyp}} for constructor functions,
         \code{\link{optim}} for possible error messages.}
% <---------------------------------------------------------------------->
\examples{
  ## multivariate generalized hyperbolic distribution
  ghyp.mv <- ghyp(lambda = 1, alpha.bar = 0.1, mu = rep(0, 2), sigma = diag(rep(1, 2)),
                  gamma = rep(0, 2), data = matrix(rt(1000, df = 4), ncol = 2))

  ## Get data
  ghyp.data(ghyp.mv)

  ## Get the dimension
  ghyp.dim(ghyp.mv)

  ## Get the name of the ghyp object
  ghyp.name(ghyp(alpha.bar = 0))
  ghyp.name(ghyp(alpha.bar = 0, lambda = -4), abbr = TRUE)

  ## 'ghyp.fit.info' does only work when the object is of class 'mle.ghyp',
  ## i.e. is created by 'fit.ghypuv' etc.
  mv.fit <- fit.tmv(data = ghyp.data(ghyp.mv), control = list(abs.tol = 1e-3))
  ghyp.fit.info(mv.fit)
}
% <---------------------------------------------------------------------->
\keyword{utilities}
