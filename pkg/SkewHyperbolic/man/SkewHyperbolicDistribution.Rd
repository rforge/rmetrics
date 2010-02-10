\name{SkewHyperbolicDistribution}
\alias{dskewhyp}
\alias{pskewhyp}
\alias{qskewhyp}
\alias{rskewhyp}
\alias{ddskewhyp}
\alias{skewhypBreaks}

\title{
  Skewed Hyperbolic Student t-Distribution
}
\description{
  Density function, distribution function, quantiles and random number
  generation for the skew hyperbolic Student \emph{t}-distribution, with
  parameters \eqn{\beta}{beta} (skewness), \eqn{\delta}{delta}
  (scale), \eqn{\mu}{mu} (location) and \eqn{\nu}{nu} (shape). Also
  functions for the derivative of the distribution, and to calculate
  suitable break points.
}
\usage{
dskewhyp(x, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), log = FALSE,
         tolerance = .Machine$double.eps^0.5)
pskewhyp(q, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), log = FALSE, lower.tail = TRUE,
         small = 10^(-6), tiny = 10^(-10), subdivisions = 100,
         accuracy = FALSE, ...)
qskewhyp(p, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), small = 10^(-6), tiny = 10^(-10),
         deriv = 0.3, nInterpol = 100, subdivisions = 100, ...)
rskewhyp(n, mu = 0, delta = 1, beta = 1, nu = 1,
         param = c(mu,delta,beta,nu), log = FALSE)
ddskewhyp(x, mu = 0, delta = 1, beta = 1, nu = 1,
          param = c(mu,delta,beta,nu),log = FALSE,
          tolerance = .Machine$double.eps ^ 0.5)
skewhypBreaks(mu = 0, delta = 1, beta = 1, nu = 1,
              param = c(mu,delta,beta,nu), small = 10^(-6),
              tiny = 10^(-10), deriv = 0.3, ...)
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
  \item{log}{Logical; if \code{log = TRUE}, probabilities are given as
    log(p).}
  \item{lower.tail}{Logical; if \code{lower.tail = TRUE}, the cumulative
    density is taken from the lower tail.}
  \item{tolerance}{Specified level of tolerance when checking if
    parameter beta is equal to 0.}
  \item{small}{Size of a 'small' difference between the distribution
    function and 0 or 1.}
  \item{tiny}{Size of a 'tiny' difference between the distribution
    function and 0 or 1.}
  \item{subdivisions}{The maximum number of subdivisions used to
    integrate the density and determine the accuracy of the distribution
    function calculation.}
  \item{accuracy}{Logical; if \code{accuracy = TRUE}, uses accuracy
    calculated by \code{\link{integrate}} to try and determine the
    accuracy of the distribution function calculation.}
  \item{deriv}{Value between 0 and 1 which determines the point at which
    the value of the derivative becomes substantial compared to its
    maximal value, see \bold{Details}.}
  \item{nInterpol}{Number of points used in \code{qskewhyp} for cubic
    spline interpolation of the distribution function.}
  \item{\dots}{Passes additional arguments to \code{\link{integrate}} in
    \code{pskewhyp} and \code{qskewhyp}, and to \code{\link{uniroot}} in
    \code{skewhypBreaks}.}

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

  \code{pskewhyp} breaks the real line in to 8 regions in order to
  determine the integral of \code{dhyperb}. The breakpoints determining
  the regions are calculated by \code{\link{skewhypBreaks}}, based on the
  values of \code{small}, \code{tiny}, and \code{deriv}.

  The outer breakpoints, \code{xTiny}, \code{xHuge}, \code{xSmall} and
  \code{xLarge} and are calculated by calls to
  \code{\link{skewhypCalcRange}}, using the value \code{tiny} for the
  first two most extreme values, and \code{small} for the next two.
  The inner area is divided into two regions above and two below the
  mode. The breakpoint that divides these is calculated to be the point
  where the derivative of the density function is \code{deriv} times the
  value of the maximum derivative on that side of the mode. In the
  extreme tails of the distribution where the density is less than \code{tiny}
  the density is taken to be zero. In the remaining regions the
  integral of the density is calculated using the numerical routine
  \code{\link[DistributionUtils]{safeIntegrate}} (a wrapper for
  \code{\link{integrate}}).

  \code{qhyperb} uses the same breakup of the real line as
  \code{pskewhyp}. For quantiles that fall in the two extreme regions
  the quantile is returned as \code{Inf} or \code{-Inf} as
  appropriate. In the remaining regions \code{splinefun} is used to fit
  values of the distribution function calculated by \code{pskewhyp}. The
  quantiles are then found by the \code{uniroot} function.

  Note that when small values of \eqn{\nu}{nu} are used, and the density
  is skewed, there are often some extreme values generated by
  \code{rskewhyp}. These look like outliers, but are caused by the
  heaviness of the skewed tail, see \bold{Examples}.

}
\value{
  \code{dskewhyp} gives the density function, \code{pskewhyp} gives the
  distribution function, \code{qskewhyp} gives the quantile function and
  \code{rskewhyp} generates random variates.

  An estimate of the accuracy of the approximation to the distribution
  function can be found by setting \code{accuracy = TRUE} in the call to
  \code{pskewyhp} which returns a list with components \code{value} and
  \code{error}.

  \code{ddskewhyp} gives the derivative of \code{\link{dskewhyp}}.

  \code{skewhypBreaks} returns a list with components:
  \item{xTiny}{Value such that the probabilities to the left are less
    than \code{tiny}.}
  \item{xSmall}{Value such that probabilities to the left are less
    than \code{small}.}
  \item{lowBreak}{Point to the left of the mode such that the
    derivative of the density at that point is \code{deriv} times its
    maximum value on that side of the mode.}
  \item{highBreak}{Point to the right of the mode such that the derivative
    of the density at that point is \code{deriv} times the value of the
    maximum value on that side of the mode.}
  \item{xLarge}{Value such that the probabities to the right are less than
    \code{xSmall}.}
  \item{xHuge}{Value such that probability to the right is less than
    \code{tiny}.}
  \item{modeDist}{The mode of the given skewed hyperbolic distribution,
    calculated by \code{\link{skewhypMode}}.}
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
  \code{\link{skewhypBreaks}}, \code{\link{skewhypCheckPars}},
  \code{\link[DistributionUtils]{logHist}}. Also
  \code{\link{skewhypMean}} for information on moments and mode, and
  \code{\link{skewhypFit}} for fitting to data.
}
\examples{
param <- c(0,1,40,10)
par(mfrow = c(1,2))
range <- skewhypCalcRange(param = param, tol = 10^(-2))

##curves of density and distribution
curve(dskewhyp(x, param = param), range[1], range[2], n = 1000)
title("Density of the \n Skew Hyperbolic Distribution")
curve(pskewhyp(x, param = param, small = 10^(-2), tiny = 10^(-4)),
      range[1], range[2], n = 500)
title("Distribution Function of the \n Skew Hyperbolic Distribution")

##curves of density and log density
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

##plot of the density marking the break points
par(mfrow = c(1,1))
range <- skewhypCalcRange(param = param, tol = 10^(-6))
curve(dskewhyp(x, param = param), range[1], range[2], n = 1000)
title("Density of the Skew Hyperbolic Distribution\n with Breakpoints")
breaks <- skewhypBreaks(param = param, small = 10^(-2), tiny = 10^(-4))
abline(v = breaks, col = 2)

##example of density and random numbers when nu is small
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
