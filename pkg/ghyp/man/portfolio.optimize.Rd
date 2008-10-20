\name{portfolio.optimize}
\alias{portfolio.optimize}
% <---------------------------------------------------------------------->
\title{Portfolio optimization with respect to alternative risk measures}
% <---------------------------------------------------------------------->
\description{

  This function performs a optimization of a portfolio with respect to
  one of the risk measures \dQuote{variance}, \dQuote{quantile} or
  \dQuote{expected-shortfall}, a level of risk and the requested
  portfolio return, given a multivariate generalized hyperbolic
  distribution.

}
% <---------------------------------------------------------------------->
\usage{
portfolio.optimize(object, ptf.mean = 0.01,
           risk.measure = c("variance", "quantile", "expected-shortfall"),
           level = 0.95,...)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{A multivariate \code{\link[=ghyp-class]{ghyp}} object.}
  \item{ptf.mean}{The required expected return of the portfolio.}
  \item{risk.measure}{The risk measure to which the portfolio should be optimized.
                      Must be one of \dQuote{variance}, \dQuote{quantile} or
                      \dQuote{expected-shortfall}.}
  \item{level}{The level of the \code{risk.measure}. Not used when \code{risk.measure}
               is \dQuote{variance}.}
  \item{\dots}{Arguments passed to \code{\link{optim}}.}
}
% <---------------------------------------------------------------------->
\value{
  A list with components:
  \item{portfolio}{An univariate generalized hyperbolic object of class \code{\link[=ghyp-class]{ghyp}} which
                   represents the generalized hyperbolic distribution of the optimal portfolio.}
  \item{risk.measure}{The optimization criterion.}
  \item{value}{The value of the risk measure.}
  \item{opt.weights}{The optimal weights.}
  \item{convergence}{Convergence returned from \code{\link{optim}}.}
  \item{message}{A possible error message returned from \code{\link{optim}}.}
  \item{n.iter}{The number of iterations returned from \code{\link{optim}}.}
}
% <---------------------------------------------------------------------->
\author{David Lüthi}
% <---------------------------------------------------------------------->
\note{

  The ghyp-object must define a return (not a loss) distribution. That
  is, the left tail of the distribution is optimized.\cr

  Setting of constraints like avoid short-selling is not supported
  yet. \cr

  If the ghyp-object defines a symmetric distribution the
  \code{risk.measure} does not matter since the distribution is then
  elliptical. \cr

  If the risk measure is \dQuote{variance} the returned portfolio is
  simply an efficient frontier. \cr

}
% <---------------------------------------------------------------------->
\seealso{\code{\link[=transform.ghyp]{transform}}, \code{\link{fit.ghypmv}}}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  mv.fit <- fit.ghypmv(data = smi.stocks[, 2:6], opt.pars = c(lambda = FALSE),
                       lambda = 2, control = list(rel.tol = 1e-5, abs.tol = 1e-5))

  optimal.portfolio <- portfolio.optimize(mv.fit, ptf.mean = 1e-3,
                                          risk.measure = "expected-shortfall",
                                          level = 0.99)

  plot(optimal.portfolio$portfolio, type = "l" , col = "red")
\dontrun{
  lines(density(smi.stocks[, 2:6] %*% optimal.portfolio$opt.weights))
}
}
% <---------------------------------------------------------------------->
\keyword{optimize}
\keyword{multivariate}
\keyword{iteration}
