\name{Hyperbolic}
\alias{dhyperb}
\alias{phyperb}
\alias{qhyperb}
\alias{rhyperb}
\alias{ddhyperb}
\alias{hyperbBreaks}

\title{Hyperbolic Distribution}

\description{
  Density function, distribution function, quantiles and
  random number generation for the hyperbolic distribution
  with parameter vector \code{param}. Utility routines are included for
  the derivative of the density function and to find suitable break
  points for use in determining the distribution function.
}

\usage{
dhyperb(x, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta))
phyperb(q, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta),
        small = 10^(-6), tiny = 10^(-10),
        deriv = 0.3, subdivisions = 100, accuracy = FALSE, ...)
qhyperb(p, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta),
        small = 10^(-6), tiny = 10^(-10),
        deriv = 0.3, nInterpol = 100, subdivisions = 100, ...)
rhyperb(n, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta))
ddhyperb(x, mu = 0, delta = 1, alpha = 1, beta = 0,
         param = c(mu, delta, alpha, beta))
hyperbBreaks(mu = 0, delta = 1, alpha = 1, beta = 0,
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
  \item{alpha}{\eqn{\alpha}{alpha} is the tail parameter, with a default value of 1.}
  \item{beta}{\eqn{\beta}{beta} is the skewness parameter, by default this is 0.}
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
  \item{nInterpol}{The number of points used in qhyperb for cubic spline
    interpolation (see \code{splinefun}) of the distribution function.}
  \item{\dots}{Passes arguments to \code{uniroot}. See \bold{Details}.}
}

\details{
  The hyperbolic distribution has density

  \deqn{f(x)=\frac{1}{2\sqrt{1+\pi^2}K_1(\zeta)} %
    e^{-\zeta[\sqrt{1+\pi^2}\sqrt{1+(\frac{x-\mu}{\delta})^2}-%
    \pi\frac{x-\mu}{\delta}]}}{ %
  f(x)=1/(2 sqrt(1+pi^2) K_1(zeta)) %
  exp(-zeta(sqrt(1+pi^2) sqrt(1+((x-mu)/delta)^2)-pi (x-mu)/delta))}

  where \eqn{K_1()}{K_1()} is the modified Bessel function of the
  third kind with order 1.

  A succinct description of the hyperbolic distribution is given in
  Barndorff-Nielsen and \enc{Blæsild}{Blaesild} (1983). Three different
  possible parameterisations are described in that paper. A fourth
  parameterization is given in Prause (1999). All use location and scale
  parameters \eqn{\mu}{mu} and \eqn{\delta}{delta}. There are two other
  parameters in each case.

  Use \code{hyperbChangePars} to convert from the
  \eqn{(\pi, \zeta)}{(pi, zeta)} \eqn{(\phi, \gamma)}{(phi, gamma)} or
  \eqn{(\xi, \chi)}{xi, chi)} parameterisations to the
  \eqn{(\alpha, \beta)}{(alpha, beta)} parameterisation used above.

  Each of the functions are wrapper functions for their equivalent
  generalized hyperbolic. For example, \code{dhyperb} calls \code{dghyp}.

  \code{phyperb} breaks the real line into eight regions in order to
  determine the integral of \code{dhyperb}. The break points determining
  the regions are found by \code{hyperbBreaks}, based on the values of
  \code{small}, \code{tiny}, and \code{deriv}. In the extreme tails of
  the distribution where the probability is \code{tiny} according to
  \code{hyperbCalcRange}, the probability is taken to be zero. In the
  range between where the probability is \code{tiny} and \code{small}
  according to \code{hyperbCalcRange}, an exponential approximation to
  the hyperbolic distribution is used. In the inner part of the
  distribution, the range is divided in 4 regions, 2 above the mode, and
  2 below. On each side of the mode, the break point which forms the 2
  regions is where the derivative of the density function is
  \code{deriv} times the maximum value of the derivative on that side of
  the mode. In each of the 4 inner regions the numerical integration
  routine \code{\link[DistributionUtils]{safeIntegrate}} (which is a
  wrapper for \code{\link{integrate}}) is used to integrate the density
  \code{dhyperb}.


  \code{qhyperb} uses the breakup of the real line into the same 8
  regions as \code{phyperb}. For quantiles which fall in the 2 extreme
  regions, the quantile is returned as \code{-Inf} or \code{Inf} as
  appropriate. In the range between where the probability is \code{tiny}
  and \code{small} according to \code{hyperbCalcRange}, an exponential
  approximation to the hyperbolic distribution is used from which the
  quantile may be found in closed form. In the 4 inner regions
  \code{splinefun} is used to fit values of the distribution function
  generated by \code{phyperb}.  The quantiles are then found
  using the \code{uniroot} function.

  \code{phyperb} and \code{qhyperb} may generally be expected to be
  accurate to 5 decimal places.

  The hyperbolic distribution is a special case of the generalized
  hyperbolic distribution (Barndorff-Nielsen and \enc{Blæsild}{Blaesild}
  (1983)). The generalized hyperbolic distribution can be represented as
  a particular mixture of the normal distribution where the mixing
  distribution is the generalized inverse Gaussian.  \code{rhyperb} uses
  this representation to generate observations from the hyperbolic
  distribution. Generalized inverse Gaussian observations are obtained
  via the algorithm of Dagpunar (1989).}

\value{
  \code{dhyperb} gives the density, \code{phyperb} gives the distribution
  function, \code{qhyperb} gives the quantile function and \code{rhyperb}
  generates random variates. An estimate of the accuracy of the
  approximation to the distribution function may be found by setting
  \code{accuracy = TRUE} in the call to \code{phyperb} which then returns
  a list with components \code{value} and \code{error}.

  \code{ddhyperb} gives the derivative of \code{dhyperb}.

  \code{hyperbBreaks} returns a list with components:
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
   \item{modeDist}{The mode of the given hyperbolic distribution.}
 }

\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson, N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707. New York: Wiley.

  Dagpunar, J.S. (1989).
  An easily implemented generalized inverse Gaussian generator
  \emph{Commun. Statist. -Simula.},
  \bold{18}, 703--710.

  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg.
}

\author{David Scott \email{d.scott@auckland.ac.nz},
Ai-Wei Lee, Jennifer Tso, Richard Trendall}

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
hyperbRange <- hyperbCalcRange(param = param, tol = 10^(-3))
par(mfrow = c(1, 2))
curve(dhyperb(x, param = param), from = hyperbRange[1], to = hyperbRange[2],
      n = 1000)
title("Density of the\n Hyperbolic Distribution")
curve(phyperb(x, param = param), from = hyperbRange[1], to = hyperbRange[2],
      n = 1000)
title("Distribution Function of the\n Hyperbolic Distribution")
dataVector <- rhyperb(500, param = param)
curve(dhyperb(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add =TRUE)
title("Density and Histogram\n of the Hyperbolic Distribution")
logHist(dataVector, main =
        "Log-Density and Log-Histogram\n of the Hyperbolic Distribution")
curve(log(dhyperb(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
par(mfrow = c(2, 1))
curve(dhyperb(x, param = param), from = hyperbRange[1], to = hyperbRange[2],
      n = 1000)
title("Density of the\n Hyperbolic Distribution")
curve(ddhyperb(x, param = param), from = hyperbRange[1], to = hyperbRange[2],
      n = 1000)
title("Derivative of the Density\n of the Hyperbolic Distribution")
par(mfrow = c(1, 1))
hyperbRange <- hyperbCalcRange(param = param, tol = 10^(-6))
curve(dhyperb(x, param = param), from = hyperbRange[1], to = hyperbRange[2],
      n = 1000)
bks <- hyperbBreaks(param = param)
abline(v = bks)
}

\keyword{distribution}
