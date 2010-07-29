\name{summary.nigFit}
\alias{summary.nigFit}
\alias{print.summary.nigFit}
\title{Summarizing normal inverse Gaussian Distribution Fit}
\usage{
\method{summary}{nigFit}(object, hessian = FALSE,
        hessianMethod = c("exact", "tsHessian"), ...)

\method{print}{summary.nigFit}(x, digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{An object of class \code{"nigFit"}, resulting from a call to
    \code{\link{nigFit}}.}
  \item{hessian}{Logical. If \code{TRUE} the Hessian is printed.}
  \item{hessianMethod}{Either the exact Hessian is used (the default) or
    the two-sided Hessian approximation given by \code{tsHessian} from
    the package \code{DistributionUtils} is used.}
  \item{x}{An object of class \code{"summary.nigFit"}, resulting from
    a call to \code{summary.nigFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"nigFit"}.
}
\details{
  If \code{hessian = FALSE} no calculations are performed, the class of
  \code{object} is simply changed from \code{nigFit} to
  \code{summary.nigFit} so that it can be passed to
  \code{print.summary.nigFit} for printing in a convenient form.

  If \code{hessian = TRUE} the Hessian is calculated via a call to
  \code{\link{nigHessian}} and the standard errors of the parameter
  estimates are calculated using the Hessian and these are added to the
  original list \code{object}. The class of the object
  returned is again changed to \code{summary.nigFit}.
}
\value{
  \code{summary.nigFit} returns a list comprised of the original
  object \code{object} and additional elements \code{hessian} and
  \code{sds} if \code{hessian = TRUE}, otherwise it returns the original
  object. The class of the object returned is changed to
  \code{summary.nigFit}.

  See \code{\link{nigFit}} for the composition of an object of class
  \code{nigFit}.

  If the Hessian and standard errors have not been added to the object
  \code{x}, \code{print.summary.nigFit} prints a summary in the same
  format as \code{\link{print.nigFit}}. When the Hessian and standard
  errors are available, the Hessian is printed and the standard errors
  for the parameter estimates are printed in parentheses beneath the
  parameter estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.
}
\seealso{
  \code{\link{nigFit}}, \code{\link{summary}}.
}
\examples{
### Continuing the  nigFit(.) example:
param <- c(2, 2, 2, 1)
dataVector <- rnig(500, param = param)
fit <- nigFit(dataVector, method = "BFGS")
print(fit)
summary(fit, hessian = TRUE)
}
\keyword{distribution}
