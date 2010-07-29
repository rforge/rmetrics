\name{Hyperbolic}
\alias{dhyperb}
\alias{phyperb}
\alias{qhyperb}
\alias{rhyperb}
\alias{ddhyperb}

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
        param = c(mu, delta, alpha, beta), log.p = FALSE,
        lower.tail = TRUE, subdivisions = 100,
        intTol = .Machine$double.eps^0.25,
        valueOnly = TRUE, ...)
qhyperb(p, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta), log.p = FALSE,
        lower.tail = TRUE, method = c("spline", "integrate"),
        nInterpol = 501, uniTol = .Machine$double.eps^0.25,
        subdivisions = 100, intTol = uniTol, ...)
rhyperb(n, mu = 0, delta = 1, alpha = 1, beta = 0,
        param = c(mu, delta, alpha, beta))
ddhyperb(x, mu = 0, delta = 1, alpha = 1, beta = 0,
         param = c(mu, delta, alpha, beta))
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
  \item{log.p}{Logical; if \code{TRUE}, probabilities are given as
    log(p).}
  \item{method}{Character. If \code{"spline"} quantiles are found from a
  spline approximation to the distribution function. If
  \code{"integrate"}, the distribution function used is always obtained
  by integration.}
  \item{lower.tail}{Logical. If \code{lower.tail = TRUE}, the cumulative
    density is taken from the lower tail.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density and determine the accuracy of the distribution
    function calculation.}
  \item{intTol}{Value of \code{rel.tol} and hence \code{abs.tol} in
    calls to \code{integrate}. See \code{\link{integrate}}.}
  \item{valueOnly}{Logical. If \code{valueOnly = TRUE} calls to
    \code{pghyp} only return the value obtained for the integral.
    If \code{valueOnly = FALSE} an estimate of the
    accuracy of the numerical integration is also returned.}
  \item{nInterpol}{Number of points used in \code{qghyp} for cubic
    spline interpolation of the distribution function.}
  \item{uniTol}{Value of \code{tol} in
    calls to \code{uniroot}. See \code{\link{uniroot}}.}
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
  generalized hyperbolic counterpart. For example, \code{dhyperb} calls
  \code{dghyp}. See \code{\link{dghyp}}.

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
}

\keyword{distribution}
