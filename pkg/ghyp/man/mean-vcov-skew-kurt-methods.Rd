\name{mean-vcov-skew-kurt-methods}
\alias{mean.ghyp}
\alias{mean-methods}
\alias{mean,ghyp-method}
\alias{vcov.ghyp}
\alias{vcov-methods}
\alias{vcov,ghyp-method}
\alias{ghyp.skewness}
\alias{ghyp.kurtosis}
% <---------------------------------------------------------------------->
\title{Expected value, variance-covariance, skewness and kurtosis
  of generalized hyperbolic distributions}
% <---------------------------------------------------------------------->
\description{

  The function \code{mean} returns the expected value. The function
  \code{vcov} returns the variance in the univariate case and the
  variance-covariance matrix in the multivariate case. The functions
  \code{ghyp.skewness} and \code{ghyp.kurtosis} only work for univariate
  generalized hyperbolic distributions.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{mean}{ghyp}(x)

\S4method{vcov}{ghyp}(object)

ghyp.skewness(object)

ghyp.kurtosis(object)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{x, object}{An object inheriting from class
                   \code{\link[=ghyp-class]{ghyp}}.}
}
% <---------------------------------------------------------------------->
\details{
  The functions \code{ghyp.skewness} and \code{ghyp.kurtosis} are based
  on the function \code{\link{ghyp.moment}}. Numerical integration will
  be used in case a Student.t or variance gamma distribution is
  submitted.
}
% <---------------------------------------------------------------------->
\value{
  Either the expected value, variance, skewness or kurtosis.
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\seealso{\code{\link{ghyp}}, \code{\link{ghyp-class}}, \code{\link{Egig}} to
compute the expected value and the variance of the generalized inverse gaussian
mixing distribution distributed and its special cases.}
% <---------------------------------------------------------------------->
\examples{
  ## Univariate: Parametric
  vg.dist <- VG(lambda = 1.1, mu = 10, sigma = 10, gamma = 2)
  mean(vg.dist)
  vcov(vg.dist)
  ghyp.skewness(vg.dist)
  ghyp.kurtosis(vg.dist)

  ## Univariate: Empirical
  vg.sim <- rghyp(10000, vg.dist)
  mean(vg.sim)
  var(vg.sim)

  ## Multivariate: Parametric
  vg.dist <- VG(lambda = 0.1, mu = c(55, 33), sigma = diag(c(22, 888)), gamma = 1:2)
  mean(vg.dist)
  vcov(vg.dist)

  ## Multivariate: Empirical
  vg.sim <- rghyp(50000, vg.dist)
  colMeans(vg.sim)
  var(vg.sim)
}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{methods}
