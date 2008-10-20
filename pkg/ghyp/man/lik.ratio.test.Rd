\name{lik.ratio.test}
\alias{lik.ratio.test}
\title{Likelihood-ratio test}
% <---------------------------------------------------------------------->
\description{

  This function performs a likelihood-ratio test on fitted generalized
  hyperbolic distribution objects of class \code{mle.ghyp}.

}
% <---------------------------------------------------------------------->
\usage{
lik.ratio.test(x, x.subclass, conf.level = 0.95)
}
% <---------------------------------------------------------------------->
\arguments{
  \item{x}{An object of class \code{mle.ghyp}.}
  \item{x.subclass}{An object of class \code{mle.ghyp} whose parameters form a subset
                    of those of \code{x}.}
  \item{conf.level}{Confidence level of the test.}
}
% <---------------------------------------------------------------------->
\value{
  A list with components:
  \item{statistic}{The value of the L-statistic.}
  \item{p.value}{The p-value for the test.}
  \item{df}{The degrees of freedom for the L-statistic.}
  \item{H0}{A boolean stating whether the null hypothesis is \code{TRUE} or \code{FALSE}.}
}
% <---------------------------------------------------------------------->
\details{

  The likelihood-ratio test can be used to check whether a special case
  of the generalized hyperbolic distribution is the \dQuote{true}
  underlying distribution.

  The likelihood-ratio is defined as \deqn{\Lambda = \frac{sup\{L(\theta
  | \mathbf{X}) : \theta \in \Theta_0\}} { sup\{L(\theta | \mathbf{X}) :
  \theta \in \Theta\}}.}{ Lambda = (sup\{L(theta | X) : theta in
  Theta_0\})/ (sup\{L(theta | X) : theta in Theta\}).}  Where \eqn{L}
  denotes the likelihood function with respect to the parameter
  \eqn{\theta} and data \eqn{\mathbf{X}}{X}, and \eqn{\Theta_0} is a
  subset of the parameter space \eqn{\Theta}.  The null hypothesis
  \code{H0} states that \eqn{\theta \in \Theta_0}. Under the null
  hypothesis and under certain regularity conditions it can be shown
  that \eqn{-2 \log(\Lambda)} is asymtotically chi-squared distributed
  with \eqn{\nu} degrees of freedom.  \eqn{\nu} is the number of free
  parameters specified by \eqn{\Theta} minus the number of free
  parameters specified by \eqn{\Theta_0}.

  The null hypothesis is rejected if \eqn{-2 \log(\Lambda)} exceeds the
  \code{conf.level}-quantile of the chi-squared distribution with
  \eqn{\nu} degrees of freedom.

}
% <---------------------------------------------------------------------->
\references{
  \emph{Linear Statistical Inference and Its Applications} by C. R. Rao\cr
  Wiley, New York, 1973
}
% <---------------------------------------------------------------------->
\author{
  David Lüthi
}
% <---------------------------------------------------------------------->
\seealso{
  \code{\link{fit.ghypuv}}, \code{\link[=logLik.mle.ghyp]{logLik}}, \code{\link[=AIC.mle.ghyp]{AIC}} and
  \code{\link{stepAIC.ghyp}}.
}
% <---------------------------------------------------------------------->
\examples{
  data(smi.stocks)

  sample <- smi.stocks[, "SMI"]

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  t.asymmetric <- fit.tuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric Student-t in case
  # of SMI log-returns
  lik.ratio.test(t.asymmetric, t.symmetric, conf.level = 0.95)
  # -> keep the null hypothesis

  set.seed(1000)
  sample <- rghyp(1000, student.t(gamma = 0.1))

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  t.asymmetric <- fit.tuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric Student-t in case of
  # data simulated according to a slightly skewed Student-t distribution
  lik.ratio.test(t.asymmetric, t.symmetric, conf.level = 0.95)
  # -> reject the null hypothesis

  t.symmetric <- fit.tuv(sample, silent = TRUE, symmetric = TRUE)
  ghyp.asymmetric <- fit.ghypuv(sample, silent = TRUE)

  # Test symmetric Student-t against asymmetric generalized
  # hyperbolic using the same data as in the example above
  lik.ratio.test(ghyp.asymmetric, t.symmetric, conf.level = 0.95)
  # -> keep the null hypothesis
}
% <---------------------------------------------------------------------->
\keyword{utilities}
