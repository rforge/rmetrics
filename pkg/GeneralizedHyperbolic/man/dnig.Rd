\name{NIG}
\alias{dnig}
\alias{pnig}
\alias{qnig}
\alias{rnig}
\alias{ddnig}
\alias{nigBreaks}

\title{Normal Inverse Gaussian Distribution}

\description{
  Density function, distribution function, quantiles and
  random number generation for the normal inverse Gaussian distribution
  with parameter vector \code{param}. Utility routines are included for
  the derivative of the density function and to find suitable break
  points for use in determining the distribution function.
}

\usage{
dnig(x, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta))
pnig(q, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta),
        small = 10^(-6), tiny = 10^(-10),
        deriv = 0.3, subdivisions = 100, accuracy = FALSE, ...)
qnig(p, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta),
        small = 10^(-6), tiny = 10^(-10),
        deriv = 0.3, nInterpol = 100, subdivisions = 100, ...)
rnig(n, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta))
ddnig(x, mu = 0, delta = 1, alpha = 1, beta = 0,
         param = c(mu, delta, alpha, beta))
nigBreaks(mu = 0, delta = 1, alpha = 1, beta = 0,
             param = c(mu, delta, alpha, beta),
             small = 10^(-6), tiny = 10^(-10), deriv = 0.3, ...)
}

\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.} 
  \item{mu}{\eqn{\mu}{mu} is the location parameter. By default this is
    set to 0.}
  \item{delta}{\eqn{\delta}{delta} is the scale parameter of the distribution.
    A default value of 1 has been set.}
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default
    value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default
    this is 0.}
  \item{param}{Parameter vector taking the form
    \code{c(mu, delta, alpha, beta)}.}
  \item{small}{Size of a small difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{tiny}{Size of a tiny difference between the distribution
    function and zero or one. See \bold{Details}.}
  \item{deriv}{Value between 0 and 1. Determines the point where the
    derivative becomes substantial, compared to its maximum value. See
    \bold{Details}.}
  \item{accuracy}{Uses accuracy calculated by \code{\link{integrate}}
    to try and determine the accuracy of the distribution function
    calculation.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density returning the distribution function.}
  \item{nInterpol}{The number of points used in \code{qnig} for cubic spline
    interpolation (see \code{splinefun}) of the distribution function.}
  \item{\dots}{Passes arguments to \code{uniroot}. See \bold{Details}.}
}

\details{
  The normal inverse Gaussian distribution has density
  \deqn{e^{\delta \sqrt{\alpha^2 - \beta^2}}%
        \frac{\alpha \delta}{\pi \sqrt{\delta^2 + (x - \mu)^2}}%
        K_1(\alpha \sqrt{\delta^2 + (x - \mu)^2})%
        e^{\beta (x - \mu)}}{ %
        f(x) = exp(delta sqrt(alpha^2 - beta^2)%
        alpha delta/pi sqrt(delta^2 + (x - mu)^2)%
        K_1(alpha sqrt(delta^2 + (x - mu)^2))%
        exp(beta (x - mu))} 


  where \eqn{K_1()}{K_1()} is the modified Bessel function of the
  third kind with order 1.

  A succinct description of the normal inverse Gaussian distribution is given in
  {Paolella} (2007). Because both the normal inverse Gaussian distribution and 
  the hybperbolic distribution are special cases of the generalized hyperbolic
  distribution (with different values of \eqn{\lambda}{lambda}, the normal 
  inverse Gaussian distribution has the same different
  parameterisations as the hyperbolic distribution and it uses 
  \code{hyperbChangePars} to interchange between different parameterisations as 
  well (see \code{hyperbChangePars} for details). 

  Each of the functions are wrapper functions for their equivalent
  generalized hyperbolic. For example, \code{dnig} calls \code{dghyp}.

  \code{pnig} breaks the real line into eight regions in order to
  determine the integral of \code{dnig}. The break points determining
  the regions are found by \code{nigBreaks}, based on the values of
  \code{small}, \code{tiny}, and \code{deriv}. In the extreme tails of
  the distribution where the probability is \code{tiny} according to
  \code{nigCalcRange}, the probability is taken to be zero. In the
  range between where the probability is \code{tiny} and \code{small}
  according to \code{nigCalcRange}, an exponential approximation to
  the hyperbolic distribution is used. In the inner part of the
  distribution, the range is divided in 4 regions, 2 above the mode, and
  2 below. On each side of the mode, the break point which forms the 2
  regions is where the derivative of the density function is
  \code{deriv} times the maximum value of the derivative on that side of
  the mode. In each of the 4 inner regions the numerical integration
  routine \code{\link[DistributionUtils]{safeIntegrate}} (which is a
  wrapper for \code{\link{integrate}}) is used to integrate the density
  \code{dnig}.

  \code{qnig} uses the breakup of the real line into the same 8
  regions as \code{pnig}. For quantiles which fall in the 2 extreme
  regions, the quantile is returned as \code{-Inf} or \code{Inf} as
  appropriate. In the range between where the probability is \code{tiny}
  and \code{small} according to \code{nigCalcRange}, an exponential
  approximation to the hyperbolic distribution is used from which the
  quantile may be found in closed form. In the 4 inner regions
  \code{splinefun} is used to fit values of the distribution function
  generated by \code{pnig}.  The quantiles are then found
  using the \code{uniroot} function.

  \code{pnig} and \code{qnig} may generally be expected to be
  accurate to 5 decimal places.

  Recall that the normal inverse Gaussian distribution is a special case of the 
  generalized hyperbolic distribution and the generalized hyperbolic 
  distribution can be represented as a particular mixture of the normal 
  distribution where the mixing
  distribution is the generalized inverse Gaussian.  \code{rnig} uses
  this representation to generate observations from the hyperbolic
  distribution. Generalized inverse Gaussian observations are obtained
  via the algorithm of Dagpunar (1989).}

\value{
  \code{dnig} gives the density, \code{pnig} gives the distribution
  function, \code{qnig} gives the quantile function and \code{rnig}
  generates random variates. An estimate of the accuracy of the
  approximation to the distribution function may be found by setting
  \code{accuracy = TRUE} in the call to \code{pnig} which then returns
  a list with components \code{value} and \code{error}.

  \code{ddnig} gives the derivative of \code{dnig}.

  \code{nigBreaks} returns a list with components:
   \item{xTiny}{Value such that probability to the left is less than
     \code{tiny}.}
   \item{xSmall}{Value such that probability to the left is less than
     \code{small}.}
   \item{lowBreak}{Point to the left of the mode such that the
     derivative of the density is \code{deriv} times its maximum value
     on that side of the mode}
   \item{highBreak}{Point to the right of the mode such that the
     derivative of the density is \code{deriv} times its maximum value
     on that side of the mode}
   \item{xLarge}{Value such that probability to the right is less than
     \code{small}.}
   \item{xHuge}{Value such that probability to the right is less than
     \code{tiny}.}
   \item{modeDist}{The mode of the given normal inverse Gaussian distribution.}
 }

\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.

  Paolella, Marc S. (2007)
  Intermediate Probability: A Computational Approach,
  Chichester: Wiley

  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Christine Yang Dong}

\seealso{
  \code{\link[DistributionUtils]{safeIntegrate}},
  \code{\link{integrate}} for its shortfalls, \code{\link{splinefun}},
  \code{\link{uniroot}} and \code{\link{hyperbChangePars}} for changing
  parameters to the \eqn{(\alpha,\beta)}{(alpha, beta)}
  parameterisation, \code{\link{dghyp}} for the generalized hyperbolic
  distribution.
}

\examples{
param <- c(0, 2, 1, 0)
nigRange <- nigCalcRange(param = param, tol = 10^(-3))
par(mfrow = c(1, 2))
curve(dnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Density of the\n Normal Inverse Gaussian Distribution")
curve(pnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Distribution Function of the\n Normal Inverse Gaussian Distribution")
dataVector <- rnig(500, param = param)
curve(dnig(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram\n of the Normal Inverse Gaussian Distribution")
logHist(dataVector, main =
        "Log-Density and Log-Histogram\n of the Normal Inverse Gaussian Distribution")
curve(log(dnig(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
par(mfrow = c(2, 1))
curve(dnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Density of the\n Normal Inverse Gaussian Distribution")
curve(ddnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
title("Derivative of the Density\n of the Normal Inverse Gaussian Distribution")
par(mfrow = c(1, 1))
nigRange <- nigCalcRange(param = param, tol = 10^(-6))
curve(dnig(x, param = param), from = nigRange[1], to = nigRange[2],
      n = 1000)
bks <- nigBreaks(param = param)
abline(v = bks)
}

\keyword{distribution}
