\name{summary.hyperbFit}
\alias{summary.hyperbFit}
\alias{print.summary.hyperbFit}
\title{Summarizing Hyperbolic Distribution Fit}
\usage{
\method{summary}{hyperbFit}(object, ...)

\method{print}{summary.hyperbFit}(object, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{In \code{summary.hyperbFit} an object of class
    \code{"hyperbFit"}, resulting from a call to
    \code{\link{hyperbFit}}. In \code{summary.hyperbFit} an object of
      class \code{"summary.hyperbFit"}, resulting from a call to
      \code{summary.hyperbFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
\code{summary} Method for class \code{"hyperbFit"}.
}
\details{
  \code{summary.hyperbFit} calculates standard errors for the estimates
  of \eqn{\pi}{pi}, \eqn{\zeta}{zeta}, \eqn{\delta}{delta}, and
  \eqn{\mu}{mu} of the hyperbolic distribution parameter vector Theta if
  the Hessian from the call to \code{\link{optim}} or \code{\link{nlm}}
  is available. Because the parameters in the call to the optimiser are
  \eqn{\pi}{pi}, \eqn{\log(\zeta)}{log(zeta)},
  \eqn{\log(\delta)}{log(delta)}, and \eqn{\mu}{mu}, the delta method is
  used to obtain the standard errors for \eqn{\zeta}{zeta} and
  \eqn{\delta}{delta}.
}
\value{
  If the Hessian is available, \code{summary.hyperbFit} computes
  standard errors for the estimates of \eqn{\pi}{pi}, \eqn{\zeta}{zeta},
  \eqn{\delta}{delta}, and \eqn{\mu}{mu}, and adds them to \code{object}
  as \code{object$sds}. Otherwise, no calculations are performed and the
  composition of \code{object} is unaltered.

  \code{summary.hyperbFit} invisibly returns \code{object}
  with class  changed to \code{summary.hyperbFit}.

  See \code{\link{hyperbFit}} for the composition of an object of class
  \code{hyperbFit}.

	\code{print.summary.hyperbFit} prints a summary in the same format as
	\code{\link{print.hyperbFit}} when the Hessian is not available from
	the fit. When the Hessian is available, the standard errors for the
	parameter estimates are printed in parentheses beneath the parameter
	estimates, in the manner of \code{fitdistr} in the package
	\code{MASS}.

}
\seealso{
  \code{\link{hyperbFit}}, \code{\link{summary}}.
}
\examples{
### Continuing the  hyperbFit(.) example:
Theta <- c(2,2,2,2)
dataVector <- rhyperb(500, Theta)
fit <- hyperbFit(dataVector, method = "BFGS", hessian = TRUE)
print(fit)
summary(fit)

}
\keyword{distribution}
