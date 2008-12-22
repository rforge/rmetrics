\name{safeIntegrate}
\alias{safeIntegrate}
\alias{print.integrate}
\title{Safe Integration of One-Dimensional Functions}
\description{
  Adaptive quadrature of functions of one variable over a finite or
  infinite interval.
}
\usage{
safeIntegrate(f, lower, upper, subdivisions=100,
              rel.tol = .Machine$double.eps^0.25, abs.tol = rel.tol,
              stop.on.error = TRUE, keep.xy = FALSE, aux = NULL, \dots)
}
\arguments{
  \item{f}{An \R function taking a numeric first argument and returning
    a numeric vector of the same length.  Returning a non-finite element will
    generate an error.}
  \item{lower, upper}{The limits of integration.  Can be infinite.}
  \item{subdivisions}{The maximum number of subintervals.}
  \item{rel.tol}{Relative accuracy requested.}
  \item{abs.tol}{Absolute accuracy requested.}
  \item{stop.on.error}{Logical. If true (the default) an error stops the
    function.  If false some errors will give a result with a warning in
    the \code{message} component.}
  \item{keep.xy}{Unused.  For compatibility with S.}
  \item{aux}{Unused.  For compatibility with S.}
  \item{\dots}{Additional arguments to be passed to \code{f}.  Remember
    to use argument names \emph{not} matching those of
    \code{safeIntegrate(.)}!}
}
\details{
  This function is just a wrapper around
  \code{\link[integrate]{integrate}} to check for equality of
  \code{upper} and \code{lower}. A check is made using
  \code{\link[integrate]{all.equal}}. When numerical equality is
  detected, if \code{lower} (and hence \code{upper}) is infinite, the
  value of the integral and the absolute error are both set to 0. When
  \code{lower} is finite, the value of the integral is set to 0, and the
  absolute error to the average of the function values at \code{upper} and
  \code{lower} times the difference between \code{upper} and \code{lower}.

  When \code{upper} and \code{lower} are determined to be different, the
  result is exactly as given by  \code{\link[integrate]{integrate}}.
}
\value{
  A list of class \code{"integrate"} with components:
  \item{value}{The final estimate of the integral.}
  \item{abs.error}{Estimate of the modulus of the absolute error.}
  \item{subdivisions}{The number of subintervals produced in the
    subdivision process.}
  \item{message}{\code{"OK"} or a character string giving the error message.}
  \item{call}{The matched call.}
}
\seealso{
  The function \code{\link[integrate]{integrate}} and
  \code{\link[integrate]{all.equal}}.
}
\examples{
integrate(dnorm, -1.96, 1.96)
safeIntegrate(dnorm, -1.96, 1.96)  # Same as for integrate()
integrate(dnorm, -Inf, Inf)
safeIntegrate(dnorm, -Inf, Inf)    # Same as for integrate()
integrate(dnorm, 1.96, 1.96)       # OK here but can give an error
safeIntegrate(dnorm, 1.96, 1.96)
integrate(dnorm, -Inf, -Inf)
safeIntegrate(dnorm, -Inf, -Inf)   # Avoids nonsense answer
integrate(dnorm, Inf, Inf)
safeIntegrate(dnorm, Inf, Inf)     # Avoids nonsense answer
}
\keyword{math}
\keyword{utilities}
