\name{skewhypCalcRange}
\alias{skewhypCalcRange}
\alias{skewhypStepSize}
\title{
Range of a Skew Hyperbolic Student t-Distribuiton
}
\description{
  Given the parameter vector \code{param}, or parameter values of a skew
  hyperbolic Student \emph{t}-distribution, this function determines the
  range outside of which the density function is negligible, to a
  specified tolerance. 
}
\usage{
skewhypCalcRange(mu = 0, delta = 1, beta = 1, nu = 1,
                 param = c(mu,delta,beta,nu), tol= 10^(-5), ...)
skewhypStepSize(dist, nu, beta, side = c("right","left"))
}
\arguments{
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Scale parameter \eqn{\delta}{delta}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
  \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,delta,beta,nu)}.}
  \item{tol}{Density function value at the endpoints of the range
    returned by the function.}
  \item{dist}{Numeric. Current distance value.}
  \item{side}{Character. \code{"right"} for a step to the right,
    \code{"left"} for a step to the right.}
  \item{\dots}{Passes additional arguments to \code{\link{uniroot}}.}
}
\details{
  The particular skew hyperbolic distribution being considered is
  specified by either the individual parameter values, or the parameter
  vector \code{param}. If both are specified, the values in \code{param}
  will overwriete the other ones.  In addition the parameter values are
  examined by calling the function \code{skewhypCheckPars} to see if
  they are valid.

  The function \code{skewhypCalcRange} returns the range outside of
  which the distribution is less than the given tolerance. Useful for
  plotting, and is called by \code{\link{skewhypBreaks}} when
  determining suitable breakpoints to be used when integrating the
  density function numerically to find the distribution function. The
  points are found by using \code{\link{uniroot}} on the distribution
  function.

  The function \code{skewhypStepSize} is used for stepping to the right
  or the left to obtain an enclosing interval so \code{uniroot} can be
  used to search. When the tail is declining exponentially the step is
  just a linear function of the current distance from the mode. If the
  tail is declining only as a power of \eqn{x}, an exponential step is
  used.

  \code{skewhypStepSize} is for internal use and is not expected to be
  called by users. It is documented here for completeness.
}
\value{
  
  The function \code{skewhypCalcRange} returns a two component vector
  giving the lower and upper limits of the range.

  \code{skewhypStepSize} returns the size of the step.
  
}
\references{
  Aas, K. and Haff, I. H. (2006).
  The Generalised Hyperbolic Skew Student's \emph{t}-distribution,
  \emph{Journal of Financial Econometrics}, \bold{4}, 275--309.
}
\author{
  David Scott \email{d.scott@auckland.ac.nz}, Fiona Grimson
}

\seealso{
  \code{\link{uniroot}}, \code{\link{dskewhyp}}, \code{\link{skewhypCheckPars}}
}
\examples{
param <- c(0,1,10,10)
range <- skewhypCalcRange(param = param, tol = 10^(-2))
range
curve(dskewhyp(x, param = c(0,1,5,10), range[1], range[2]))

param <- c(0,1,20,1)
(range <- skewhypCalcRange(param = param))
round(integrate(dskewhyp, -Inf, range[1], param = param)$value,7)
round(integrate(dskewhyp, range[2], Inf, param = param)$value,7)
}
\keyword{distribution}

