\name{GeneralizedHyperbolicDistribution}
\alias{dghyp}
\alias{pghyp}
\alias{qghyp}
\alias{rghyp}
\alias{ddghyp}


\title{
  Generalized Hyperbolic Distribution
}
\description{
  Density function, distribution function, quantiles and random number
  generation for the generalized hyperbolic distribution, with
  parameters \eqn{\alpha}{alpha} (tail), \eqn{\beta}{beta} (skewness),
  \eqn{\delta}{delta} (peakness), \eqn{\mu}{mu} (location) and
  \eqn{\lambda}{lambda} (shape).
}
\usage{
dghyp(x, mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
      param = c(mu, delta, alpha, beta, lambda), log = FALSE,
      tolerance = .Machine$double.eps^0.5)
pghyp(q, mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
      param = c(mu, delta, alpha, beta, lambda), log.p = FALSE,
      lower.tail = TRUE, subdivisions = 100,
      intTol = .Machine$double.eps^0.25, valueOnly = TRUE, ...)
qghyp(p, mu = 0, delta = 1, alpha = 1, beta = 1, lambda = 1,
      param = c(mu, delta, alpha, beta, lambda),
      lower.tail = TRUE, log.p = FALSE,
      method = c("spline","integrate"),
      nInterpol = 501, uniTol = .Machine$double.eps^0.25,
      subdivisions = 100, intTol = uniTol, ...)
rghyp(n, mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
      param = c(mu, delta, alpha, beta, lambda), log = FALSE)
ddghyp(x, mu = 0, delta = 1, alpha = 1, beta = 0, lambda = 1,
       param = c(mu, delta, alpha, beta, lambda))
}
\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of random variates to be generated.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Peakness parameter \eqn{\delta}{delta}, default is 1.}
  \item{alpha}{Tail parameter \eqn{\alpha}{alpha}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 0.}
  \item{Lambda}{Shape parameter \eqn{\lambda}{lambda}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,delta,alpha,beta,lambda)}.}
  \item{log,log.p}{Logical; if \code{TRUE}, probabilities are given as
    log(p).}
  \item{method}{Character. If \code{"spline"} quantiles are found from a
    spline approximation to the distribution function. If
    \code{"integrate"}, the distribution function used is always obtained
    by integration.}
  \item{lower.tail}{Logical. If \code{lower.tail = TRUE}, the cumulative
    density is taken from the lower tail.}
  \item{tolerance}{Specified level of tolerance when checking if
    parameter \code{beta} is equal to 0.}
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
  \item{\dots}{Passes additional arguments to \code{\link{integrate}} in
    \code{pghyp} and \code{qghyp}, and to \code{\link{uniroot}} in
    \code{qghyp}.}

}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones. In addition the
  parameter values are examined by calling the function
  \code{ghypCheckPars} to see if they are valid.

  The density function is

  \deqn{f(x)=c(\lambda,\alpha,\beta,\delta)\times%
    \frac{K_{\lambda-1/2}(\alpha\sqrt{\delta^2+(x-\mu)^2})}%
    {(\frac{\sqrt{\delta^2+(x-\mu)^2}}{\alpha})^{1/2-\lambda}}%
    e^{\beta(x-\mu)}}{%
    f(x)=c(lambda,alpha,beta,delta)%
    (K_(lambda-1/2)(alpha sqrt(delta^2+(x-mu)^2)))/%
    ((sqrt(delta^2+(x-mu)^2)/alpha)^(1/2-lambda))%
    exp(beta(x-mu))}

  where \eqn{K_\nu()}{K_nu()} is the modified Bessel function of the
  third kind with order \eqn{\nu}{nu}, and
  \deqn{c(\lambda,\alpha,\beta,\delta)=%
    \frac{(\frac{\sqrt{\alpha^2-\beta^2}}{\delta})^\lambda}%
    {\sqrt{2\pi}K_\lambda(\delta\sqrt{\alpha^2-\beta^2})}}{%
    c(lambda,alpha,beta,delta)=%
    (sqrt(alpha^2-beta^2)/delta)^lambda/%
    (sqrt(2\pi)K_lambda(delta sqrt(alpha^2-beta^2)))}

  Use \code{ghypChangePars} to convert from the
  \eqn{(\zeta, \rho)}{(zeta, rho)},
  \eqn{(\xi, \chi)}{(xi, chi)}, or
  \eqn{(\bar\alpha, \bar\beta)}{(alpha bar, beta bar)} parameterisations
  to the \eqn{(\alpha, \beta)}{(alpha, beta)} parameterisation used
  above.

  \code{pghyp} uses the function \code{\link{integrate}} to
  numerically integrate the density function. The integration is from
  \code{-Inf} to \code{x} if \code{x} is to the left of the mode, and
  from \code{x} to \code{Inf} if \code{x} is to the right of the
  mode. The probability calculated this way is subtracted from 1 if
  required. Integration in this manner appears to make calculation of
  the quantile function more stable in extreme cases.

  Calculation of quantiles using \code{qghyp} permits the use of two
  different methods. Both methods use \code{uniroot} to find the value
  of \eqn{x} for which a given \eqn{q} is equal \eqn{F(x)} where \eqn{F}
  denotes the cumulative distribution function. The difference is in how
  the numerical approximation to \eqn{F} is obtained. The obvious
  and more accurate method is to calculate the value of \eqn{F(x)}
  whenever it is required using a call to \code{pghyp}. This is what
  is done if the method is specified as \code{"integrate"}. It is clear
  that the time required for this approach is roughly linear in the
  number of quantiles being calculated. A Q-Q plot of a large data set
  will clearly take some time. The alternative (and default) method is
  that for the major part of the distribution a spline approximation to
  \eqn{F(x)} is calculated and quantiles found using \code{uniroot} with
  this approximation. For extreme values (for which the tail probability
  is less than \eqn{10^{-7}}{10^(-7)}), the integration method is still
  used even when the method specifed is \code{"spline"}.

  If accurate probabilities or quantiles are required, tolerances
  (\code{intTol} and \code{uniTol}) should be set to small values, say
  \eqn{10^{-10}}{10^(-10)} or \eqn{10^{-12}}{10^(-12)} with \code{method
  = "integrate"}. Generally then accuracy might be expected to be at
  least \eqn{10^{-9}}{10^(-9)}. If the default values of the functions
  are used, accuracy can only be expected to be around
  \eqn{10^{-4}}{10^(-4)}. Note that on 32-bit systems
  \code{.Machine$double.eps^0.25 = 0.0001220703} is a typical value.
}
\value{
  \code{dghyp} gives the density function, \code{pghyp} gives the
  distribution function, \code{qghyp} gives the quantile function and
  \code{rghyp} generates random variates.

  An estimate of the accuracy of the approximation to the distribution
  function can be found by setting \code{valueOnly = FALSE} in the call to
  \code{pghyp} which returns a list with components \code{value} and
  \code{error}.

  \code{ddghyp} gives the derivative of \code{\link{dghyp}}.
}
\references{
  Barndorff-Nielsen, O. and \enc{Blæsild}{Blaesild}, P (1983).
  Hyperbolic distributions.
  In \emph{Encyclopedia of Statistical Sciences},
  eds., Johnson,N. L., Kotz, S. and Read, C. B., Vol. 3,
  pp. 700--707.-New York: Wiley.

  Bibby, B. M and \enc{Sörenson}{Sorenson},M. (2003).
  Hyperbolic processes in finance.
  In \emph{Handbook of Heavy Tailed Distributions in Finance},ed.,
  Rachev, S. T. pp. 212--248. Elsevier Science B.~V.

  Dagpunar, J.S. (1989).
  An easily implemented generalised inverse Gaussian generator
  \emph{Commun. Statist.-Simula.}, \bold{18}, 703--710.

  Prause, K. (1999) \emph{The generalized hyperbolic models: Estimation,
  financial derivatives and risk measurement.} PhD Thesis, Mathematics
  Faculty, University of Freiburg.
}

\author{
  David Scott \email{d.scott@auckland.ac.nz}
}

\seealso{
  \code{\link{dhyperb}} for the hyperbolic distribution,
  \code{\link{dgig}} for the generalized inverse Gaussian distribution,
  \code{\link[DistributionUtils]{safeIntegrate}},
  \code{\link{integrate}} for its shortfalls, also
  \code{\link{splinefun}}, \code{\link{uniroot}} and
  \code{\link{ghypChangePars}} for changing parameters to the
  \eqn{(\alpha,\beta)}{(alpha,beta)} parameterisation.
}
\examples{
param <- c(0, 1, 3, 1, 1/2)
ghypRange <- ghypCalcRange(param = param, tol = 10^(-3))
par(mfrow = c(1, 2))

### curves of density and distribution
curve(dghyp(x, param = param), ghypRange[1], ghypRange[2], n = 1000)
title("Density of the \n Generalized  Hyperbolic Distribution")
curve(pghyp(x, param = param), ghypRange[1], ghypRange[2], n = 500)
title("Distribution Function of the \n Generalized Hyperbolic Distribution")

### curves of density and log density
par(mfrow = c(1, 2))
data <- rghyp(1000, param = param)
curve(dghyp(x, param = param), range(data)[1], range(data)[2],
      n = 1000, col = 2)
hist(data, freq = FALSE, add = TRUE)
title("Density and Histogram of the\n Generalized Hyperbolic Distribution")
logHist(data, main = "Log-Density and Log-Histogram of\n the Generalized
      Hyperbolic Distribution")
curve(log(dghyp(x, param = param)),
      range(data)[1], range(data)[2],
      n = 500, add = TRUE, col = 2)

### plots of density and derivative
par(mfrow = c(2, 1))
curve(dghyp(x, param = param), ghypRange[1], ghypRange[2], n = 1000)
title("Density of the\n Generalized  Hyperbolic Distribution")
curve(ddghyp(x, param = param), ghypRange[1], ghypRange[2], n = 1000)
title("Derivative of the Density of the\n Generalized Hyperbolic Distribution")
}
\keyword{distribution}
