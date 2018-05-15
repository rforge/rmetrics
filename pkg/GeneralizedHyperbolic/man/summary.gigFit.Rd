\name{summary.gigFit}
\alias{summary.gigFit}
\alias{print.summary.gigFit}
\title{Summarizing Normal Inverse Gaussian Distribution Fit}
\usage{
\method{summary}{gigFit}(object, hessian = FALSE,
        hessianMethod = "tsHessian", ...)

\method{print}{summary.gigFit}(x,
                               digits = max(3, getOption("digits") - 3), ...)
}
\arguments{
  \item{object}{An object of class \code{"gigFit"}, resulting from a call to
    \code{\link{gigFit}}.}
  \item{hessian}{Logical. If \code{TRUE} the Hessian is printed.}
  \item{hessianMethod}{The two-sided Hessian approximation given by
    \code{tsHessian} from the package \code{DistributionUtils} is the
    only method implemented so far.}
  \item{x}{An object of class \code{"summary.gigFit"}, resulting from
    a call to \code{summary.gigFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"gigFit"}.
}
\details{
  If \code{hessian = FALSE} no calculations are performed, the class of
  \code{object} is simply changed from \code{gigFit} to
  \code{summary.gigFit} so that it can be passed to
  \code{print.summary.gigFit} for printing in a convenient form.

  If \code{hessian = TRUE} the Hessian is calculated via a call to
  \code{\link{gigHessian}} and the standard errors of the parameter
  estimates are calculated using the Hessian and these are added to the
  original list \code{object}. The class of the object
  returned is again changed to \code{summary.gigFit}.
}
\value{
  \code{summary.gigFit} returns a list comprised of the original
  object \code{object} and additional elements \code{hessian} and
  \code{sds} if \code{hessian = TRUE}, otherwise it returns the original
  object. The class of the object returned is changed to
  \code{summary.gigFit}.

  See \code{\link{gigFit}} for the composition of an object of class
  \code{gigFit}.

  If the Hessian and standard errors have not been added to the object
  \code{x}, \code{print.summary.gigFit} prints a summary in the same
  format as \code{\link{print.gigFit}}. When the Hessian and standard
  errors are available, the Hessian is printed and the standard errors
  for the parameter estimates are printed in parentheses beneath the
  parameter estimates, in the manner of \code{fitdistr} in the package
  \code{MASS}.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz},
  Christine Yang Dong \email{c.dong@auckland.ac.nz}
}

\seealso{
  \code{\link{gigFit}}, \code{\link{summary}}, \code{gigHessian}.
}
\examples{
### Continuing the  gigFit(.) example:
param <- c(1,1,1)
dataVector <- rgig(500, param = param)
fit <- gigFit(dataVector)
print(fit)
summary(fit, hessian = TRUE, hessianMethod = "tsHessian")
}
\keyword{distribution}
