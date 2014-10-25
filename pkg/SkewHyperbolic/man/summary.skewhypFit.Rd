\name{summary.skewhypFit}
\alias{summary.skewhypFit}
\alias{print.summary.skewhypFit}
\title{Summarising the Skew Hyperbolic Student t-Distribution Fit}
\usage{
\method{summary}{skewhypFit}(object, ...)
\method{print}{summary.skewhypFit}(x, digits = max(3,
getOption("digits") - 3), ...)

}
\arguments{
  \item{object}{An object of class \code{"skewhypFit"}, resulting from a
    call to \code{\link{skewhypFit}}.}
  \item{x}{An object of class \code{"summary.skewhypFit"}, resulting
    from a call to\cr
    \code{summary.skewhypFit}.}
  \item{digits}{The number of significant digits to use when printing.}
  \item{\dots}{Further arguments passed to or from other methods.}
}
\description{
  \code{summary} Method for class \code{"skewhypFit"}.
}
\details{
  \code{summary.skewhypFit} calculates standard errors for errors for
  the estimates of \eqn{\mu}{mu}, \eqn{\delta}{delta}, \eqn{\beta}{beta}
  and \eqn{\nu}{nu} of the skew hyperbolic Student
  \emph{t}-distribution parameter vector \code{param}, if the Hessian
  from the call to \code{\link{optim}} or \code{\link{nlm}} is
  available. Because the parameters in the call to the optimiser are
  \eqn{\mu}{mu}, \eqn{\log(\delta)}{log(delta)}, \eqn{\beta}{beta} and
  \eqn{\log(\nu)}{log(nu)} the delta method is used to obtain standard
  errors for \eqn{\delta}{delta} and \eqn{\nu}{nu}
}
\value{
  If the Hessian is available \code{summary.skewhyhpFit} computes
  standard errors of \eqn{\mu}{mu}, \eqn{\delta}{delta}, \eqn{\beta}{beta}
  and \eqn{\nu}{nu}, and adds them to \code{object} as
  \code{object$sds}. Otherwise, no calculations are performed and the
  composition \code{object} is unaltered.

  \code{summary.skewhypFit} invisibly returns \code{x} with class
  changed to \code{summary.skewhypFit}.

  See \code{\link{skewhypFit}} for the composition of an object of class
  \code{skewhypFit}.

  \code{print.summary.skewhypFit} prints a summary in the same format as
  \code{\link{print.skewhypFit}} when the Hessian is not available from
  the fit. When the Hessian is available, the standard errors for the
  parameter estimates are printed in parentheses beneath the parameter
  estimates, in the manner of \code{fitdistr} in the package \code{MASS}.
}
\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's \emph{t}-distribution,
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.
}
\seealso{
  \code{\link{skewhypFit}}, \code{\link{dskewhyp}}, \code{\link{summary}}
}
\examples{
## Continuing the skewhypFit(.) example:
data(lrdji)
djfit <- skewhypFit(lrdji, print = FALSE, plot = FALSE, hessian = TRUE)
print(djfit)
summary(djfit)
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}
\keyword{distribution}
