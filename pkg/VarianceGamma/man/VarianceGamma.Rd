% --- Source file: dvg.Rd ---
\name{VarianceGammaDistribution}
\alias{dvg}
\alias{pvg}
\alias{qvg}
\alias{rvg}
\alias{ddvg}
\alias{vgBreaks}

\title{The Variance Gamma Distribution}

\description{
  Density function, distribution function, quantiles and
  random number generation for the variance gamma distribution
  with parameters \eqn{c}{c} (location), \eqn{\sigma}{sigma} (spread),
  \eqn{\theta}{theta} (asymmetry) and \eqn{\nu}{nu} (shape).
  Utility routines are included for the derivative of the density function and
  to find suitable break points for use in determining the distribution
  function.
}

\usage{
  dvg(x, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), log = FALSE,
    tolerance = .Machine$double.eps ^ 0.5, \dots)
  pvg(q, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), lower.tail = TRUE, log.p = FALSE,
    small = 10^(-6), tiny = 10^(-10), deriv = 0.3, subdivisions = 100,
    accuracy = FALSE, \dots)
  qvg(p, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), lower.tail = TRUE, log.p = FALSE,
    small = 10^(-6), tiny = 10^(-10), deriv = 0.3, nInterpol = 100,
    subdivisions = 100, \dots)
  rvg(n, vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu))
  ddvg (x,  vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), log = FALSE,
    tolerance = .Machine$double.eps ^ 0.5, \dots)
  vgBreaks (vgC = 0, sigma = 1, theta = 0, nu = 1,
    param = c(vgC,sigma,theta,nu), small = 10^(-6), tiny = 10^(-10),
    deriv = 0.3, \dots)
}

\arguments{
  \item{x,q}{Vector of quantiles.}
	\item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.}
  \item{vgC}{The location parameter \eqn{c}{c}, default is 0.}
  \item{sigma}{The spread parameter \eqn{\sigma}{sigma}, default is 1,
    must be positive.}
  \item{theta}{The asymmetry parameter \eqn{\theta}{theta}, default is 0.}
  \item{nu}{The shape parameter \eqn{\nu}{nu}, default is 1, must be
    positive.}
  \item{param}{Specifying the parameters as a vector which takes the form
    \code{c(vgC,sigma,theta,nu)}.}
  \item{log, log.p}{Logical; if TRUE, probabilities p are given as log(p);
    not yet implemented.}
  \item{lower.tail}{If TRUE (default), probabilities are \eqn{P[X <= x]},
    otherwise, \eqn{P[X > x]}; not yet implemented.}
  \item{small}{Size of a small difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{tiny}{Size of a tiny difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{deriv}{Value between 0 and 1. Determines the point where the
    derivative becomes substantial, compared to its maximum value. See
    \bold{Details}.}
  \item{accuracy}{Uses accuracy calculated by~\code{\link{integrate}}
    to try and determine the accuracy of the distribution function
    calculation.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density returning the distribution function.}
  \item{nInterpol}{The number of points used in \code{qvg} for cubic spline
    interpolation (see \code{splinefun}) of the distribution function.}
  \item{tolerance}{Size of a machine difference between two values.
    See \bold{Details}.}
  \item{\dots}{Passes arguments to \code{uniroot}.  See \bold{Details}.}
}

\details{
  Users may either specify the values of the parameters individually or as a
  vector. If both forms are specifed but with different values, then the
  values specified by vector \code{param} will always overwrite the other ones.


  The variance gamma distribution has density

  \deqn{f(x)=c(c,\sigma,\theta,\nu)\times%
    {e^{[\theta(x-c)/\sigma^2]}}%
    {|x-c|^{1/\nu-1/2}}%
    {K_{1/\nu-1/2}}%
    \left(\frac{|x-c|\sqrt{2\sigma^2/\nu+\theta^2}}%
    {\sigma^2}\right)}{%
    f(x)=c(c,sigma,theta,nu)%
      exp(theta(x-c)/sigma^2)%
      |x-c|^(1/nu-1/2)%
      K_(1/nu-1/2)((|x-c|sqrt(2simga^2/nu+theta^2))/%
      sigma^2)}

  where \eqn{K_\nu()}{K_nu()} is the modified Bessel function of the
  third kind of order \eqn{\nu}{nu}, and
	\deqn{c(c,\sigma,\theta,\nu)=%
    \frac{2}%
    {\sigma\sqrt{2\pi}\nu^{1/\nu}\Gamma(1/\nu)}%
    \left(\frac{1}%
    {\sqrt{2\sigma^2/\nu+\theta^2}}\right)
    ^{1/\nu-1/2}}{%
    c(c,sigma,theta,nu)=%
      2/%
      sigma sqrt(2\pi) nu^(1/nu) Gamma(1/nu)%
      (1/%
      sqrt(2sigma^2/nu+theta^2))^(1/nu-1/2)}

  Special cases:

  1. If \eqn{\nu < 2}{nu < 2} and \eqn{x = c}{x = c}, then the density
  function is approximate to
  \deqn{f(x)= \frac{\Gamma(1/\nu-1/2)}%
    {\sigma\sqrt{2\pi}\nu^{1/\nu}\Gamma(1/\nu)}%
    \left(\frac{2\sigma^2}%
    {\sqrt{2\sigma^2/\nu+\theta^2}}\right)
    ^{1/\nu-1/2}}{%
      Gamma(1/nu-1/2)/%
      sigma sqrt(2\pi) nu^(1/nu) Gamma(1/nu)%
      (2sigma^2/%
      sqrt(2sigma^2/nu+theta^2))^(1/nu-1/2)}

  2. If \eqn{\nu\geq2}{nu >= 2} and \eqn{x = c}{x = c},
  then the density function is taken the value Inf.

  Use \code{vgChangePars} to convert from the
  \eqn{(\mu, \sigma, \theta, \tau)}{(mu, sigma, theta, tau)},
  or \eqn{(\theta, \sigma, \kappa, \tau)}{(theta, sigma, kappa, tau)}
  parameterisations given in Kotz \emph{et al}. (2001)
  to the \eqn{(c, \sigma, \theta, \nu)}{(c, sigma, theta, nu)}
  parameterisation used above.

  \code{pvg} breaks the real line into eight regions in order to
  determine the integral of \code{dvg}. The break points determining
  the regions are found by \code{vgBreaks}, based on the values of
  \code{small}, \code{tiny}, and \code{deriv}. In the extreme tails of
  the distribution where the probability is \code{tiny} according to
  \code{vgCalcRange}, the probability is taken to be zero.  In the
  inner part of the distribution, the range is divided in 6 regions, 3
  above the mode, and 3 below. On each side of the mode, there are two
  break points giving the required three regions. The outer break point
  is where the probability in the tail has the value given by the variable
  \code{small}. The inner break point is where the derivative of the
  density function is \code{deriv} times the maximum value of the
  derivative on that side of the mode. In each of the 6 inner regions
  the numerical integration routine
  \code{\link[HyperbolicDist]{safeIntegrate}} (which is a wrapper for
  \code{\link{integrate}}) is used to integrate the density \code{dvg}.

  \code{qvg} uses the breakup of the real line into the same 8
  regions as \code{pvg}. For quantiles which fall in the 2 extreme
  regions, the quantile is returned as \code{-Inf} or \code{Inf} as
  appropriate. In the 6 inner regions \code{splinefun} is used to fit
  values of the distribution function generated by \code{pvg}. The
  quantiles are then found using the \code{uniroot} function.

  \code{pvg} and \code{qvg} may generally be expected to be
  accurate to 5 decimal places.

  The variance gamma distribution is discussed in Kotz \emph{et al} (2001).
  It can be seen to be the weighted difference of two i.i.d. gamma variables
  shifted by the value of \eqn{\theta}{theta}. \code{rvg} uses this
  representation to generate oberservations from the variance gamma
  distribution.
  }

  \value{
  \code{dvg} gives the density function, \code{pvg} gives the distribution
  function, \code{qvg} gives the quantile function and \code{rvg}
  generates random variates. An estimate of the accuracy of the
  approximation to the distribution function may be found by setting
  \code{accuracy=TRUE} in the call to \code{pvg} which then returns
  a list with components \code{value} and \code{error}.

  \code{ddvg} gives the derivative of \code{dvg}.

  \code{vgBreaks} returns a list with components:
   \item{xTiny}{Value such that probability to the left is less than
     \code{tiny}.}
   \item{xSmall}{Value such that probability to the left is less than
     \code{small}.}
   \item{lowBreak}{Point to the left of the mode such that the
     derivative of the density is \code{deriv} times its maximum value
     on that side of the mode.}
   \item{highBreak}{Point to the right of the mode such that the
     derivative of the density is \code{deriv} times its maximum value
     on that side of the mode.}
   \item{xLarge}{Value such that probability to the right is less than
     \code{small}.}
   \item{xHuge}{Value such that probability to the right is less than
     \code{tiny}.}
   \item{modeDist}{The mode of the given variance gamma distribution.}
  }

\references{
  Seneta, E. (2004). Fitting the variance-gamma model to financial data.
  \emph{J. Appl. Prob.}, 41A:177--187.
  Kotz, S, Kozubowski, T. J., and \enc{Podgórski}{Podgorski},
  K. (2001). 
  The Laplace Distribution and Generalizations. \emph{Birkhauser},
  Boston, 349 p.
}

 \author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

 \seealso{
  \code{\link{vgChangePars}},
  \code{\link{vgCalcRange}}
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

# Example 1 (nu < 2)
## For dvg and pvg
param <- c(0,0.5,0,0.5)
maxDens <- dvg(vgMode(param = param), param = param, log = FALSE)
## Or to specify parameter values individually, use:
maxDens <- dvg(vgMode(0,0.5,0,0.5), 0,0.5,0,0.5, log = FALSE)

vgRange <- vgCalcRange(param = param, tol = 10^(-2)*maxDens, density = TRUE)
par(mfrow = c(1,2))
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2], n = 1000)
title("Density of the Variance Gamma Distribution")
curve(pvg(x, param = param), from = vgRange[1], to = vgRange[2], n = 1000)
title("Distribution Function of the Variance Gamma Distribution")

## For rvg
dataVector <- rvg(500, param = param)
curve(dvg(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram of the Variance Gamma Distribution")
logHist(dataVector, main =
   "Log-Density and Log-Histogramof the Generalized Hyperbolic Distribution")
curve(log(dvg(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)

## For dvg and ddvg
par(mfrow = c(2,1))
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Density of the Variance Gamma Distribution")
curve(ddvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Derivative of the Density of the Variance Gamma Distribution")

# Example 2 (nu > 2 and theta = 0)
## For dvg and pvg
param <- c(0,0.5,0,3)
vgRange <- c(0-4,0+4)
par(mfrow = c(1,2))
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Density of the Variance Gamma Distribution")
curve(pvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Distribution Function of the Variance Gamma Distribution")

## For rvg
dataVector <- rvg(500, param = param)
curve(dvg(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram of the Variance Gamma Distribution")
logHist(dataVector, main =
   "Log-Density and Log-Histogramof the Generalized Hyperbolic Distribution")
curve(log(dvg(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)

## For dvg and ddvg
par(mfrow = c(2,1))
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Density of the Variance Gamma Distribution")
curve(ddvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
title("Derivative of the Density of the Variance Gamma Distribution")



## Use the following rules for vgCalcRange when plotting graphs for vgBreaks.
## if (nu < 2), use:
##   maxDens <- dvg(vgMode(param =c(vgC, sigma, theta, nu)),
##     param = c(vgC, sigma, theta, nu), log = FALSE)
##   vgRange <- vgCalcRange(param = param, tol = 10^(-6)*maxDens, density = TRUE)
## if (nu >= 2) and theta < 0, use:
##    vgRange <- c(vgC-2,vgC+6)
## if (nu >= 2) and theta > 0, use:
##    vgRange <- c(vgC-6,vgC+2)
## if (nu >= 2) and theta = 0, use:
##    vgRange <- c(vgC-4,vgC+4)

## Example 3 (nu < 2)
## For vgBreaks
param <- c(0,0.5,0,0.5)
maxDens <- dvg(vgMode(param = param), param = param, log = FALSE)
vgRange <- vgCalcRange(param = param, tol = 10^(-6)*maxDens, density = TRUE)
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
bks <- vgBreaks(param = param)
abline(v = bks)
title("Density of the Variance Gamma Distribution with breaks")

## Example 4 (nu > 2 and theta = 0)
## For vgBreaks
param <- c(0,0.5,0,3)
vgRange <- c(0-4,0+4)
curve(dvg(x, param = param), from = vgRange[1], to = vgRange[2],
      n = 1000)
bks <- vgBreaks(param = param)
abline(v = bks)
title("Density of the Variance Gamma Distribution with breaks")

}
