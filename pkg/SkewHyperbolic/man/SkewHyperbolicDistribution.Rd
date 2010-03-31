\name{SkewHyperbolicDistribution}
\alias{dskewhyp}
\alias{pskewhyp}
\alias{qskewhyp}
\alias{rskewhyp}
\alias{ddskewhyp}

\title{
  Skewed Hyperbolic Student t-Distribution
}
\description{
  Density function, distribution function, quantiles and random number
  generation for the skew hyperbolic Student \emph{t}-distribution, with
  parameters \eqn{\beta}{beta} (skewness), \eqn{\delta}{delta}
  (scale), \eqn{\mu}{mu} (location) and \eqn{\nu}{nu} (shape). Also a
  function for the derivative of the density function.
}
\usage{
dskewhyp(x, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), log = FALSE,
         tolerance = .Machine$double.eps^0.5)
pskewhyp(q, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu, delta, beta, nu), log.p = FALSE,
         lower.tail = TRUE, subdivisions = 100,
         intTol = .Machine$double.eps^0.25, valueOnly = TRUE, ...)
qskewhyp(p, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta, beta, nu),
         lower.tail = TRUE, log.p = FALSE,
         method = c("spline","integrate"),
         nInterpol = 501, uniTol = .Machine$double.eps^0.25,
         subdivisions = 100, intTol = uniTol, ...)
rskewhyp(n, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), log = FALSE)
ddskewhyp(x, mu = 0, delta = 1, beta = 1, nu = 1,
          param = c(mu,delta,beta,nu),log = FALSE,
          tolerance = .Machine$double.eps^0.5)
}
\arguments{
  \item{x,q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of random variates to be generated.}
  \item{mu}{Location parameter \eqn{\mu}{mu}, default is 0.}
  \item{delta}{Scale parameter \eqn{\delta}{delta}, default is 1.}
  \item{beta}{Skewness parameter \eqn{\beta}{beta}, default is 1.}
  \item{nu}{Shape parameter \eqn{\nu}{nu}, default is 1.}
  \item{param}{Specifying the parameters as a vector of the form\cr
    \code{c(mu,delta,beta,nu)}.}
  \item{log,log.p}{Logical; if \code{log = TRUE}, probabilities are given as
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
    \code{pskewhyp} only return the value obtained for the
    integral. If \code{valueOnly = FALSE} an estimate of the
    accuracy of the numerical integration is also returned.}
  \item{nInterpol}{Number of points used in \code{qskewhyp} for cubic
    spline interpolation of the distribution function.}
  \item{uniTol}{Value of \code{tol} in
    calls to \code{uniroot}. See \code{\link{uniroot}}.}
  \item{\dots}{Passes additional arguments to \code{\link{integrate}} in
    \code{pskewhyp} and \code{qskewhyp}, and to \code{\link{uniroot}} in
    \code{qskewhyp}.}

}
\details{
  Users may either specify the values of the parameters individually or
  as a vector. If both forms are specified, then the values specified by
  the vector \code{param} will overwrite the other ones. In addition the
  parameter values are examined by calling the function
  \code{skewhypCheckPars} to see if they are valid.

  The density function is

  \deqn{f(x)=\frac{2^{(1-\nu)/2}\delta^\nu%latex
      |\beta|^{(\nu+1)/2}%
      K_{(\nu+1)/2}\sqrt{(\beta^2(\delta^2+(x-\mu)^2))}%
      \exp(\beta(x-\mu))}{\Gamma(\nu/2)\sqrt{(\pi)}%
      \sqrt{(\delta^2+(x-\mu)^2)^{(\nu+1)/2}}}}{%
    f(x) = (2^((1-nu)/2) delta^nu abs(beta)^((nu+1)/2)%
    K_((nu+1)/2)(sqrt(beta^2 (delta^2+(x-mu)^2)) )%
    exp(beta (x-mu)))/ (gamma(nu/2) sqrt(pi)%
    (sqrt(delta^2+(x-mu)^2))^((nu+1)/2))}

  when \eqn{\beta \ne 0}{beta != 0}, and

  \deqn{ f(x)=\frac{\Gamma((\nu+1)/2)}{\sqrt{\pi}\delta%
      \Gamma(\nu/2)}\left(1+\frac{(x-\mu)^2}{\delta^2}\right)^%
    {-(\nu+1)/2} }{%
    f(x)=(gamma((nu+1)/2)/(sqrt(pi) %
    delta gamma(nu/2)))(1+((x-mu)^2)/(delta^2))^(-(nu+1)/2)}

  when \eqn{\beta = 0}{beta = 0}, where \eqn{K_{\nu}(.)}{K_nu(.)} is the
  modified Bessel function of the third kind with order \eqn{\nu}{nu},
  and \eqn{\Gamma(.)}{gamma(.)} is the gamma function.

  \code{pskewhyp} uses the function \code{\link{integrate}} to
  numerically integrate the density function. The integration is from
  \code{-Inf} to \code{x} if \code{x} is to the left of the mode, and
  from \code{x} to \code{Inf} if \code{x} is to the right of the
  mode. The probability calculated this way is subtracted from 1 if
  required. Integration in this manner appears to make calculation of
  the quantile function more stable in extreme cases.

  Calculation of quantiles using \code{qhyperb} permits the use of two
  different methods. Both methods use \code{uniroot} to find the value
  of \eqn{x} for which a given \eqn{q} is equal \eqn{F(x)} where \eqn{F}
  denotes the cumulative distribution function. The difference is in how
  the numerical approximation to \eqn{F} is obtained. The obvious
  and more accurate method is to calculate the value of \eqn{F(x)}
  whenever it is required using a call to \code{phyperb}. This is what
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

  Note that when small values of \eqn{\nu}{nu} are used, and the density
  is skewed, there are often some extreme values generated by
  \code{rskewhyp}. These look like outliers, but are caused by the
  heaviness of the skewed tail, see \bold{Examples}.

  The extreme skewness of the distribution when \eqn{\beta}{beta} is
  large in absolute value and \eqn{\nu}{nu} is small make this
  distribution very challenging numerically.



}
\value{
  \code{dskewhyp} gives the density function, \code{pskewhyp} gives the
  distribution function, \code{qskewhyp} gives the quantile function and
  \code{rskewhyp} generates random variates.

  An estimate of the accuracy of the approximation to the distribution
  function can be found by setting \code{valueOnly = FALSE} in the call to
  \code{pskewyhp} which returns a list with components \code{value} and
  \code{error}.

  \code{ddskewhyp} gives the derivative of \code{\link{dskewhyp}}.
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
  \code{\link[DistributionUtils]{safeIntegrate}},
  \code{\link{integrate}} for its shortfalls,
  \code{\link{skewhypCheckPars}},
  \code{\link[DistributionUtils]{logHist}}. Also
  \code{\link{skewhypMean}} for information on moments and mode, and
  \code{\link{skewhypFit}} for fitting to data.

} \examples{
param <- c(0,1,40,10)
par(mfrow = c(1,2))
range <- skewhypCalcRange(param = param, tol = 10^(-2))

### curves of density and distribution
curve(dskewhyp(x, param = param), range[1], range[2], n = 1000)
title("Density of the \n Skew Hyperbolic Distribution")
curve(pskewhyp(x, param = param),
      range[1], range[2], n = 500)
title("Distribution Function of the \n Skew Hyperbolic Distribution")

### curves of density and log density
par(mfrow = c(1,2))
data <- rskewhyp(1000, param = param)
curve(dskewhyp(x, param = param), range(data)[1], range(data)[2],
      n = 1000, col = 2)
hist(data, freq = FALSE, add = TRUE)
title("Density and Histogram of the\n Skew Hyperbolic Distribution")
logHist(data, main = "Log-Density and Log-Histogram of\n the Skew
      Hyperbolic Distribution")
curve(dskewhyp(x, param = param, log = TRUE),
      range(data)[1], range(data)[2],
      n = 500, add = TRUE, col = 2)

##plots of density and derivative
par(mfrow = c(2,1))
curve(dskewhyp(x, param = param), range[1], range[2], n = 1000)
title("Density of the Skew\n Hyperbolic Distribution")
curve(ddskewhyp(x, param = param), range[1], range[2], n = 1000)
title("Derivative of the Density\n of the Skew Hyperbolic Distribution")

##example of density and random numbers for beta large and nu small
par(mfrow = c(1,2))
param1 <- c(0,1,10,1)
data1 <- rskewhyp(1000, param = param1)
curve(dskewhyp(x, param = param1), range(data1)[1], range(data1)[2],
      n = 1000, col = 2)
hist(data1, freq = FALSE, add = TRUE)
title("Density and Histogram\n when nu is small")
logHist(data1, main = "Log-Density and Log-Histogram\n when nu is small")
curve(dskewhyp(x, param = param1, log = TRUE),
      range(data1)[1], range(data1)[2],
      n = 500, add = TRUE, col = 2)
}
\keyword{distribution}
