% --- Source file: summary.vgFit.Rd ---
\name{summary.vgFit}
\alias{summary.vgFit}
\alias{print.summary.vgFit}
\title{Summarizing Variance Gamma Distribution Fit}
\usage{
\method{summary}{vgFit}(object, ...)

\method{print}{summary.vgFit}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{An object of class \code{"vgFit"}, resulting from a call to
    \code{\link{vgFit}}.}
  \item{x}{An object of class \code{"summary.vgFit"}, resulting from a call to
    \code{\link{summary.vgFit}}.}

  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"vgFit"}.
}
\details{ 
  \code{summary.vgFit} calculates standard errors for the estimates of
  \eqn{c}{c}, \eqn{\sigma}{sigma}, \eqn{\theta}{theta}, and
  \eqn{\nu}{nu} of the variance gamma distribution parameter vector
  \code{param} if the Hessian from the call to \code{\link{optim}} or
  \code{\link{nlm}} is available. Because the parameters in the call to
  the optimiser are \eqn{c}{c}, \eqn{\log(\sigma)}{log(sigma)},
  \eqn{\theta}{theta} and \eqn{\log(\nu)}{log(nu)}, the delta method is
  used to obtain the standard errors for \eqn{\sigma}{sigma} and
  \eqn{\nu}{nu}.
}
\value{
  If the Hessian is available, \code{summary.vgFit} computes
  standard errors for the estimates of \eqn{c}{c}, \eqn{\sigma}{sigma},
  \eqn{\theta}{theta}, and \eqn{\nu}{nu}, and adds them to \code{object}
  as \code{object$sds}. Otherwise, no calculations are performed and the
  composition of \code{object} is unaltered.

  \code{summary.vgFit} invisibly returns \code{x} with class changed to
  \code{summary.vgFit}.

  See \code{\link{vgFit}} for the composition of an object of class
  \code{vgFit}.

  \code{print.summary.vgFit} prints a summary in the same format as
  \code{\link{print.vgFit}} when the Hessian is not available from
  the fit. When the Hessian is available, the standard errors for the
  parameter estimates are printed in parentheses beneath the parameter
  estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.
}
\seealso{
  \code{\link{vgFit}}, \code{\link{summary}}.
}
\examples{
### Continuing the  vgFit(.) example:
param <- c(0,0.5,0,0.5)
dataVector <- rvg(500, param = param)
fit <- vgFit(dataVector)
print(fit)
summary(fit)          
}