\name{summary.nigFit}
\alias{summary.nigFit}
\alias{print.summary.nigFit}
\title{Summarizing normal inverse Gaussian Distribution Fit}
\usage{
\method{summary}{nigFit}(object, ...)

\method{print}{summary.nigFit}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{An object of class \code{"nigFit"}, resulting from a call to
    \code{\link{nigFit}}.}
  \item{x}{An object of class \code{"summary.nigFit"}, resulting from
    a call to \code{summary.nigFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"nigFit"}.
}
\details{
  \code{summary.nigFit} calculates standard errors for the estimates
  of \eqn{\mu}{mu}, \eqn{\delta}{delta}, \eqn{\alpha}{alpha}, and
  \eqn{\beta}{beta} of the normal inverse Gaussian distribution parameter vector 
  param if the Hessian from the call to \code{\link{nigFit}} is available.
}
\value{
  If the Hessian is available, \code{summary.nigFit} computes
  standard errors for the estimates of \eqn{\mu}{mu}, \eqn{\delta}{delta},
  \eqn{\alpha}{alpha}, and \eqn{\beta}{beta}, and adds them to \code{object}
  as \code{object$sds}. Otherwise, no calculations are performed and the
  composition of \code{object} is unaltered.

  \code{summary.nigFit} invisibly returns \code{x}
  with class changed to \code{summary.nigFit}.

  See \code{\link{nigFit}} for the composition of an object of class
  \code{nigFit}.

  \code{print.summary.nigFit} prints a summary in the same format as
  \code{\link{print.nigFit}} when the Hessian is not available from
  the fit. When the Hessian is available, the standard errors for the
  parameter estimates are printed in parentheses beneath the parameter
  estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.

}
\seealso{
  \code{\link{nigFit}}, \code{\link{summary}}.
}
\examples{
### Continuing the  nigFit(.) example:
param <- c(2, 2, 2, 1)
dataVector <- rnig(500, param = param)
fit <- nigFit(dataVector, method = "BFGS", hessian = TRUE)
print(fit)
summary(fit)
}
\keyword{distribution}
