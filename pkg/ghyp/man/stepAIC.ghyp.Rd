\name{stepAIC.ghyp}
\alias{stepAIC.ghyp}
% <---------------------------------------------------------------------->
\title{Perform a model selection based on the AIC}
% <---------------------------------------------------------------------->
\description{

  This function performs a model selection in the scope of the
  generalized hyperbolic distribution class based on the Akaike
  information criterion. \code{stepAIC.ghyp} can be used for the
  univariate as well as for the multivariate case.

}
% <---------------------------------------------------------------------->
\usage{
stepAIC.ghyp(data, dist = c("ghyp", "hyp", "NIG", "VG", "t", "gauss"),
             symmetric = NULL, ...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{data}{A \code{vector}, \code{matrix} or \code{data.frame}.}
  \item{dist}{A character vector of distributions from where the best fit will be identified.}
  \item{symmetric}{Either \code{NULL}, \code{TRUE} or \code{FALSE}. \code{NULL} means that
                   both symmetric and asymmetric models will be fitted. For symmetric models
                   select \code{TRUE} and for asymmetric models select \code{FALSE}.}
  \item{\dots}{Arguments passed to \code{\link{fit.ghypuv}} or \code{\link{fit.ghypmv}}.}
}
% <---------------------------------------------------------------------->
\value{
  A list with components:
  \item{best.model}{The model minimizing the AIC.}
  \item{all.models}{All fitted models.}
  \item{fit.table}{A \code{data.frame} with columns \code{model}, \code{symmetric},
                   \code{lambda}, \code{alpha.bar}, \code{aic}, \code{llh} (log-Likelihood),
                   \code{converged}, \code{n.iter} (number of iterations) sorted according to the
                   aic. In the univariate case three additional columns containing
                   the parameters \code{mu}, \code{sigma} and \code{gamma} are added.}
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\seealso{
  \code{\link{lik.ratio.test}}, \code{\link{fit.ghypuv}} and \code{\link{fit.ghypmv}}.
}
% <---------------------------------------------------------------------->
\examples{

  data(indices)

  # Multivariate case:
  aic.mv <- stepAIC.ghyp(indices, dist = c("ghyp", "hyp", "t", "gauss"),
                         symmetric = NULL, control = list(maxit = 500),
                         silent = TRUE, nit = 500)

  summary(aic.mv$best.model)

  # Univariate case:
  aic.uv <- stepAIC.ghyp(indices[, "stock"], dist = c("ghyp", "NIG", "VG", "gauss"),
                         symmetric = TRUE, control = list(maxit = 500), silent = TRUE)


  # Test whether the ghyp-model provides a significant improvement with
  # respect to the VG-model:
  lik.ratio.test(aic.uv$all.models[[1]], aic.uv$all.models[[3]])

}
% <---------------------------------------------------------------------->
\keyword{utilities}
