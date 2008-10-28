\name{logLik-AIC-methods}
\alias{logLik.mle.ghyp}
\alias{logLik,mle.ghyp-method}
\alias{AIC.mle.ghyp}
\alias{AIC,mle.ghyp-method}
% <---------------------------------------------------------------------->
\title{Extract Log-Likelihood and Akaike's Information Criterion}
% <---------------------------------------------------------------------->
\description{

  The functions \code{logLik} and \code{AIC} extract the Log-Likelihood
  and the Akaike's Information Criterion from fitted generalized
  hyperbolic distribution objects.  The Akaike information criterion is
  calculated according to the formula \eqn{-2 \cdot
  \mbox{log-likelihood} + k \cdot n_{par}}{-2 * log-likelihood + k *
  npar}, where \eqn{n_{par}}{npar} represents the number of parameters
  in the fitted model, and \eqn{k = 2} for the usual AIC.

}
% <---------------------------------------------------------------------->
\usage{
\S4method{logLik}{mle.ghyp}(object, \dots)

\S4method{AIC}{mle.ghyp}(object, ..., k = 2)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{object}{An object of class \code{\link[=mle.ghyp-class]{mle.ghyp}}.}
  \item{k}{The \dQuote{penalty} per parameter to be used; the default k = 2 is the classical AIC.}
  \item{\dots}{An arbitrary number of objects of class \code{\link[=mle.ghyp-class]{mle.ghyp}}.}
}
% <---------------------------------------------------------------------->
\value{
  Either the Log-Likelihood or the Akaike's Information Criterion.
}
% <---------------------------------------------------------------------->
\author{David Luethi}
% <---------------------------------------------------------------------->
\note{
The Log-Likelihood as well as the Akaike's Information Criterion can be obtained from
the function \code{\link{ghyp.fit.info}}. However, the benefit of \code{logLik} and \code{AIC}
is that these functions allow a call with an arbitrary number of objects and are better known
because they are generic.
}
% <---------------------------------------------------------------------->
\seealso{\code{\link{fit.ghypuv}}, \code{\link{fit.ghypmv}}, \code{\link{lik.ratio.test}},
         \code{\link{ghyp.fit.info}}, \code{\link{mle.ghyp-class}}}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  ## Multivariate fit
  fit.mv <- fit.hypmv(smi.stocks, nit = 10)
  AIC(fit.mv)
  logLik(fit.mv)

  ## Univariate fit
  fit.uv <- fit.tuv(smi.stocks[, "CS"], control = list(maxit = 10))
  AIC(fit.uv)
  logLik(fit.uv)

  # Both together
  AIC(fit.uv, fit.mv)
  logLik(fit.uv, fit.mv)
}
% <---------------------------------------------------------------------->
\keyword{utilities}
\keyword{methods}
