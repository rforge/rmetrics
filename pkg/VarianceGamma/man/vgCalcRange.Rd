% --- Source file: vgCalcRange.Rd ---
\name{vgCalcRange}
\alias{vgCalcRange}
\title{Range of a Variance Gamma Distribution}

\description{
  Given the parameter vector \code{param} or the idividual parameter values  
  \eqn{(c,\sigma,\theta,\nu)}{(c,sigma,theta,nu)} of a variance gamma 
  distribution, this function determines the range outside of which the density
  function is negligible, to a specified tolerance. The parameterization used
  is the \eqn{(c,\sigma,\theta,\nu)}{(c,sigma,theta,nu)} one (see
  \code{\link{dvg}}). To use another parameterization, use
  \code{\link{vgChangePars}}.
}

\usage{
  vgCalcRange(vgC = 0, sigma = 1, theta = 0, nu = 1, 
    param = c(vgC, sigma, theta, nu), tol = 10^(-5), density = TRUE, ...)
}

\arguments{
  \item{vgC}{The location parameter \eqn{c}{c}, default is 0.}
  \item{sigma}{The spread parameter \eqn{\sigma}{sigma}, default is 1, 
    must be positive.}
  \item{theta}{The asymmetry parameter \eqn{\theta}{theta}, default is 0.}
  \item{nu}{The shape parameter \eqn{\nu}{nu}, default is 1, must be 
    positive.}
  \item{param}{Specifying the parameters as a vector which takes the form 
    \code{c(vgC,sigma,theta,nu)}.}
  \item{tol}{Tolerance.}
  \item{density}{Logical. If \code{TRUE}, the bounds are for the density
    function. If \code{FALSE}, they should be for the probability
    distribution, but this has not yet been implemented.}
  \item{\dots}{Extra arguments for calls to \code{\link{uniroot}}.}
}

\details{
  Users may either specify the values of the parameters individually or as a
  vector. If both forms are specifed but with different values, then the 
  values specified by vector \code{param} will always overwrite the other ones.
  
  The particular variance gamma distribution being considered is
  specified by the value of the parameter \code{param}.

  If \code{density = TRUE}, the function gives a range, outside of which
  the density is less than the given tolerance. Useful for plotting the
  density. Also used in determining break points for the separate
  sections over which numerical integration is used to determine the
  distribution function. The points are found by using
  \code{\link{uniroot}} on the density function.

  If \code{density = FALSE}, the function returns the message:
  "\code{Distribution function bounds not yet implemented}".
}

\value{
  A two-component vector giving the lower and upper ends of the range.
}
\references{
  Seneta, E. (2004). Fitting the variance-gamma model to financial data.
  \emph{J. Appl. Prob.}, 41A:177--187.
  Kotz, S, Kozubowski, T. J., and \enc{Podgórski}{Podgorski},
  K. (2001). 
  The Laplace Distribution and Generalizations. \emph{Birkhauser},
  Boston, 349 p.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, 
  Christine Yang Dong \email{c.dong@auckland.ac.nz}}

\seealso{
  \code{\link{dvg}}, \code{\link{vgChangePars}}
}
\examples{

## Use the following rules for vgCalcRange when plotting graphs for dvg,
## ddvg and pvg.
## if nu < 2, use:
##   maxDens <- dvg(vgMode(param = c(vgC, sigma, theta, nu)),
##   param = c(vgC, sigma, theta, nu), log = FALSE)
##   vgRange <- vgCalcRange(param = c(vgC, sigma, theta, nu),
##     tol = 10^(-2)*maxDens, density = TRUE)

## if nu >= 2 and theta < 0, use:
##   vgRange <- c(vgC-2,vgC+6)
## if nu >= 2 and theta > 0, use:
##   vgRange <- c(vgC-6,vgC+2)
## if nu >= 2 and theta = 0, use:
##   vgRange <- c(vgC-4,vgC+4)

param <- c(0,0.5,0,0.5)
maxDens <- dvg(vgMode(param = param), param = param)
vgRange <- vgCalcRange(param = param, tol = 10^(-2)*maxDens)
vgRange
curve(dvg(x, param = param), vgRange[1], vgRange[2])
curve(dvg(x, param = param), vgRange[1], vgRange[2])

param <- c(2,2,0,3)
vgRange <- c(2-4,2+4)
vgRange
curve(dvg(x, param = param), vgRange[1], vgRange[2])
\dontrun{vgCalcRange(param = param, tol = 10^(-3), density = FALSE)}
}