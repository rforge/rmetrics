\name{coef-method}
\alias{coef.ghyp}
\alias{coef,ghyp-method}
\alias{coefficients,ghyp-method}
\title{Extract parameters of generalized hyperbolic distribution objects}
\description{

  The function \code{coef} returns the parameters of a generalized
  hyperbolic distribution object as a list. The user can choose between
  the \dQuote{chi/psi}, the \dQuote{alpha.bar} and the
  \dQuote{alpha/delta} parametrization. The function \code{coefficients}
  is a synonym for \code{coef}.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{coef}{ghyp}(object, type = c("chi.psi", "alpha.bar", "alpha.delta"))

\S4method{coefficients}{ghyp}(object, type = c("chi.psi", "alpha.bar", "alpha.delta"))
}
% <---------------------------------------------------------------------->
\arguments{

  \item{object}{An object inheriting from class \code{\link[=ghyp-class]{ghyp}}.}

  \item{type}{According to \code{type} the parameters of either the
              \dQuote{chi/psi}, the \dQuote{alpha.bar} or the
              \dQuote{alpha/delta} parametrization will be returned.  If
              \code{type} is missing, the parameters belonging to the
              parametrization of the construction are returned.}

}
% <---------------------------------------------------------------------->
\value{
  If \code{type} is \dQuote{chi.psi} a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{chi} \tab Shape parameter. \cr
  \code{psi} \tab Shape parameters. \cr
  \code{mu} \tab Location parameter. \cr
  \code{sigma}\tab Dispersion parameter.\cr
  \code{gamma}\tab Skewness parameter.\cr
  }

  If \code{type} is \dQuote{alpha.bar} a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{alpha.bar} \tab Shape parameter. \cr
  \code{mu} \tab Location parameter. \cr
  \code{sigma}\tab Dispersion parameter.\cr
  \code{gamma}\tab Skewness parameter.\cr
  }

  If \code{type} is \dQuote{alpha.delta} a list with components:
  \tabular{rl}{
  \code{lambda} \tab Shape parameter. \cr
  \code{alpha} \tab Shape parameter. \cr
  \code{delta}\tab Shape parameter.\cr
  \code{mu}\tab Location parameter.\cr
  \code{Delta}\tab Dispersion matrix with a determinant of 1 (only returned in the multivariate case).\cr
  \code{beta} \tab Shape and skewness parameter. \cr
  }
}
% <---------------------------------------------------------------------->
\details{

  Internally, the \dQuote{chi/psi} parametrization is used. However,
  fitting is only possible in the \dQuote{alpha.bar} parametrization as
  it provides the most convenient parameter constraints.

}
% <---------------------------------------------------------------------->
\note{

  A switch from either the \dQuote{chi/psi} to the \dQuote{alpha.bar} or
  from the \dQuote{alpha/delta} to the \dQuote{alpha.bar}
  parametrization is not yet possible.

}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\seealso{

  \code{\link{ghyp}}, \code{\link{fit.ghypuv}},
         \code{\link{fit.ghypmv}}, \code{\link{ghyp.fit.info}},
         \code{\link[=transform.ghyp]{transform}}, \code{\link{[.ghyp}}

}
% <---------------------------------------------------------------------->
\examples{
  ghyp.mv <- ghyp(lambda = 1, alpha.bar = 0.1, mu = rep(0,2), sigma = diag(rep(1,2)),
                  gamma = rep(0,2), data = matrix(rt(1000, df = 4), ncol = 2))
  ## Get parameters
  coef(ghyp.mv, type = "alpha.bar")
  coefficients(ghyp.mv, type = "chi.psi")

  ## Simple modification (do not modify slots directly e.g. object@mu <- 0:1)
  param <- coef(ghyp.mv, type = "alpha.bar")
  param$mu <- 0:1
  do.call("ghyp", param) # returns a new 'ghyp' object


}
% <---------------------------------------------------------------------->
\keyword{methods}
\keyword{utilities}
