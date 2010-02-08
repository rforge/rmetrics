\name{integrateDens}
\Rdversion{1.1}
\alias{integrateDens}

\title{
 Integrates a density function over a range
}
\description{
  Given a density function specified by the root of the density function
  name, returns the integral over a specified range, usually the whole
  real line. Used for checking that the integral over the whole real line
  is 1.
}
\usage{
integrateDens(densFn = "norm", lower = -Inf, upper = Inf,
              subdivisions = 100, ...)
}

\arguments{
  \item{densFn}{Character. The name of the density function to be
    integrated.}
  \item{lower}{Numeric. The lower limit of the integration.
    Defaulty is \code{-Inf}.}
  \item{upper}{Numeric. The upper limit of the integration.
    Defaulty is \code{Inf}.}
  \item{subdivisions}{Numeric. The number of subdivisions to be passed
  to \code{integrate}.}
  \item{\dots}{Additional arguments to be passed to \code{integrate}. In
  particular, the parameters of the distribution.}
}
\details{
 The name of the density function to be integrated must be supplied as
  the characters of the root for that density (e.g. \code{norm},
  \code{gamma}). The density function specified is integrated
  numerically over the range specified via a call to
  \code{\link{integrate}}. The parameters of the distribution can be
  specified, otherwise the default parameters will be used.
}
\value{
  A list of class \code{integrate} with components:
  \item{value}{The final estimate of the integral.}
  \item{abs.error}{Estimate of the modulus of the absolute error.}
  \item{subdivisions}{The number of subintervals produced in the
    subdivision process.}
  \item{message}{\code{OK} or a character string giving the
    error message.}
  \item{call}{The matched call to the \code{integrate} function.}
}

\author{David Scott \email{d.scott@auckland.ac.nz}}

\seealso{\code{\link{momIntegrated}}}
\examples{
integrateDens("norm", mean = 1, sd = 1)
integrateDens("t", df = 4)
integrateDens("exp", rate = 2)
integrateDens("weibull", shape = 1)
}
\keyword{distribution}
\keyword{univar}
