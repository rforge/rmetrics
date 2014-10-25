\name{summary.nlFit}
\alias{summary.nlFit}
\alias{print.summary.nlFit}

\title{Summarizing Normal Laplace Distribution Fit}
\description{
  \code{summary} Method for class \code{"nlFit"}.
}

\usage{
  \method{summary}{nlFit}(object, ...)
  \method{print}{summary.nlFit}(x,
        digits = max(3, getOption("digits") - 3), ...)
}

\arguments{
  \item{object}{An object of class \code{"nlFit"},
    resulting from a call to \code{\link{nlFit}}.}
  \item{x}{An object of class \code{"summary.nlFit"}
    resulting from a call to \code{summary.nlFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}

\details{
  \code{summary.nlFit} calculates standard errors for the estimates
  of \eqn{\mu}{mu}, \eqn{\sigma}{sigma}, \eqn{\alpha}{alpha}, and
  \eqn{\beta}{beta} of the normal laplace distribution parameter vector
  param if the Hessian from the call to \code{\link{nlFit}} is available.
}

\value{
  If the Hessian is available, \code{summary.nlFit} computes
  standard errors for the estimates of \eqn{\mu}{mu}, \eqn{\sigma}{sigma},
  \eqn{\alpha}{alpha}, and \eqn{\beta}{beta}, and adds them to \code{object}
  as \code{object$sds}. Otherwise, no calculations are performed and the
  composition of \code{object} is unaltered.

  \code{summary.nlFit} invisibly returns \code{object}
  with class changed to \code{summary.nlFit}.

  See \code{\link{nlFit}} for the composition of an object of class
  \code{nlFit}.

  \code{print.summary.nlFit} prints a summary in the same format as
  \code{\link{print.nlFit}} when the Hessian is not available from
  the fit. When the Hessian is available, the standard errors for the
  parameter estimates are printed in parentheses beneath the parameter
  estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.
}

\seealso{
  \code{\link{nlFit}}, \code{\link{summary}}.
}

\examples{
## Continuing the  nlFit() example:
param <- c(2, 2, 1, 1)
dataVector <- rnl(500, param = param)
fit <- nlFit(dataVector, hessian = TRUE)
print(fit)
summary(fit)
}

\keyword{distribution}
