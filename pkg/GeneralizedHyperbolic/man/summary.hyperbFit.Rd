\name{summary.hyperbFit}
\alias{summary.hyperbFit}
\alias{print.summary.hyperbFit}
\title{Summarizing Hyperbolic Distribution Fit}
\usage{
\method{summary}{hyperbFit}(object, hessian = FALSE,
        hessianMethod = c("exact", "tsHessian"), ...)

\method{print}{summary.hyperbFit}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{An object of class \code{"hyperbFit"}, resulting from a call to
    \code{\link{hyperbFit}}.}
  \item{hessian}{Logical. If \code{TRUE} the Hessian is printed.}
  \item{hessianMethod}{Either the exact Hessian is used (the default) or
    the two-sided Hessian approximation given by \code{tsHessian} from
    the package \code{DistributionUtils} is used.}
  \item{x}{An object of class \code{"summary.hyperbFit"}, resulting from
    a call to \code{summary.hyperbFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"hyperbFit"}.
}
\details{
  If \code{hessian = FALSE} no calculations are performed, the class of
  \code{object} is simply changed from \code{hyperbFit} to
  \code{summary.hyperbFit} so that it can be passed to
  \code{print.summary.hyperbFit} for printing in a convenient form.

  If \code{hessian = TRUE} the Hessian is calculated via a call to
  \code{\link{hyperbHessian}} and the standard errors of the parameter
  estimates are calculated using the Hessian and these are added to the
  original list \code{object}. The class of the object
  returned is again changed to \code{summary.hyperbFit}.
}
\value{
  \code{summary.hyperbFit} returns a list comprised of the original
  object \code{object} and additional elements \code{hessian} and
  \code{sds} if \code{hessian = TRUE}, otherwise it returns the original
  object. The class of the object returned is changed to
  \code{summary.hyperbFit}.

  See \code{\link{hyperbFit}} for the composition of an object of class
  \code{hyperbFit}.

  If the Hessian and standard errors have not been added to the object
  \code{x}, \code{print.summary.hyperbFit} prints a summary in the same
  format as \code{\link{print.hyperbFit}}. When the Hessian and standard
  errors are available, the Hessian is printed and the standard errors
  for the parameter estimates are printed in parentheses beneath the
  parameter estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.

}
\seealso{
  \code{\link{hyperbFit}}, \code{\link{summary}},
  \code{\link{hyperbHessian}}, \code{\link[DistributionUtils]{tsHessian}}.
}
\examples{
### Continuing the  hyperbFit(.) example:
param <- c(2, 2, 2, 1)
dataVector <- rhyperb(500, param = param)
fit <- hyperbFit(dataVector, method = "BFGS")
print(fit)
summary(fit, hessian = TRUE)
}
\keyword{distribution}
