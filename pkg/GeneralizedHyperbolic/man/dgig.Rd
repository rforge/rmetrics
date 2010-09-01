\name{Generalized Inverse Gaussian}
\alias{dgig}
\alias{pgig}
\alias{qgig}
\alias{rgig}
\alias{rgig1}
\alias{ddgig}


\title{Generalized Inverse Gaussian Distribution}

\description{
  Density function, cumulative distribution function, quantile function
  and random number generation for the generalized inverse Gaussian
  distribution with parameter vector \code{param}. Utility routines are
  included for the derivative of the density function and to find
  suitable break points for use in determining the distribution function.
}

\usage{
dgig(x, chi = 1, psi = 1, lambda = 1,
     param = c(chi, psi, lambda), KOmega = NULL)
pgig(q, chi = 1, psi = 1, lambda = 1,
     param = c(chi,psi,lambda), log.p = FALSE, lower.tail = TRUE,
     ibfTol = .Machine$double.eps^(0.85), nmax = 100)
qgig(p, chi = 1, psi = 1, lambda = 1,
     param = c(chi, psi, lambda),  log.p = FALSE,
     lower.tail = TRUE, method = c("spline", "integrate"),
     nInterpol = 501, uniTol = 10^(-7),
     ibfTol = .Machine$double.eps^(0.85), nmax = 90, ...)
rgig(n, chi = 1, psi = 1, lambda = 1,
     param = c(chi, psi, lambda))
rgig1(n, chi = 1, psi = 1, param = c(chi, psi))
ddgig(x, chi = 1, psi = 1, lambda = 1,
      param = c(chi, psi, lambda), KOmega = NULL)
}

\arguments{
  \item{x, q}{Vector of quantiles.}
  \item{p}{Vector of probabilities.}
  \item{n}{Number of observations to be generated.}
  \item{chi}{A shape parameter that by default holds a value of 1.}
  \item{psi}{Another shape parameter that is set to 1 by default.}
  \item{lambda}{Shape parameter of the GIG distribution. Common to all
    forms of parameterization. By default this is set to 1.}
  \item{param}{Parameter vector taking the form \code{c(chi, psi, lambda)}
    for \code{rgig}, or \code{c(chi, psi)} for \code{rgig1}.}
  \item{log.p}{Logical. If \code{TRUE}, probabilities \eqn{p} are given
    as \eqn{\log(p)}{log(p)}.}
  \item{method}{Character. If \code{"spline"} quantiles are found from a
    spline approximation to the distribution function. If
    \code{"integrate"}, the distribution function used is always obtained
    by integration.}
  \item{lower.tail}{Logical. If \code{TRUE}, probabilities are
    \eqn{P(X\leq x)}{P(X<=x)}, otherwise as \eqn{P(X>x)}{P(X>x)}.}
  \item{KOmega}{Sets the value of the Bessel function in the density or
    derivative of the density. See \bold{Details}.}
  \item{ibfTol}{Value of tolerance to be passed to
    \code{incompleteBesselK} by \code{pgig}.}
    \item{nmax}{Value of maximum order of the approximating series to be
    passed to \code{incompleteBesselK} by \code{pgig}.}
  \item{nInterpol}{The number of points used in qgig for cubic spline
    interpolation (see \code{splinefun}) of the distribution function.}
  \item{uniTol}{Value of \code{tol} in
    calls to \code{uniroot}. See \code{\link{uniroot}}.}
  \item{\dots}{Passes arguments to \code{uniroot}.  See \bold{Details}.}
}

\details{
  The generalized inverse Gaussian distribution has density

  \deqn{f(x)=\frac{(\psi/\chi)^{\frac{\lambda}{2}}}%
    {2K_\lambda(\sqrt{\psi\chi})}x^{\lambda-1}%
    e^{-\frac{1}{2}\left(\chi x^{-1}+\psi x\right)}}{%
    f(x)=(psi/chi)^{lambda/2}/%
    (2K_lambda(sqrt(psi chi)))x^(lambda-1)%
    exp(-(1/2)(chi x^(-1)+psi x))}

   for \eqn{x>0}{x>0}, where \eqn{K_\lambda()}{K_lambda()} is the
   modified Bessel function of the third kind with order
   \eqn{\lambda}{lambda}.

  The generalized inverse Gaussian distribution is investigated in
  detail in \enc{Jörgensen}{Jorgensen} (1982).

  Use \code{gigChangePars} to convert from the
  \eqn{(\delta,\gamma)}{(delta, gamma)},
  \eqn{(\alpha,\beta)}{(alpha, beta)}, or
  \eqn{(\omega,\eta)}{(omega, eta)} parameterisations to the
  \eqn{(\chi,\psi)}{(chi, psi)}, parameterisation used above.

  \code{pgig} calls the function
  \code{\link[DistributionUtils]{incompleteBesselK}} from the package
  \pkg{DistributionUtils} to integrate the density function
  \code{dgig}. This can be expected to be accurate to about 13 decimal
  places on a 32-bit computer, often more accurate. The algorithm used
  is due to Slavinsky and Safouhi (2010).

  Calculation of quantiles using \code{qgig} permits the use of two
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

  Generalized inverse Gaussian observations are obtained via the
  algorithm of Dagpunar (1989).
}

\value{
  \code{dgig} gives the density, \code{pgig} gives the distribution
  function, \code{qgig} gives the quantile function, and \code{rgig}
  generates random variates. \code{rgig1} generates random variates in
  the special case where \eqn{\lambda = 1}{lambda = 1}.

  \code{ddgig} gives the derivative of \code{dgig}.
}

\references{
  Dagpunar, J.S. (1989). An easily implemented generalised inverse
  Gaussian generator. \emph{Commun. Statist. -Simula.}, \bold{18},
  703--710.

  \enc{Jörgensen}{Jorgensen}, B. (1982). \emph{Statistical Properties of
  the Generalized Inverse Gaussian Distribution}. Lecture Notes in
  Statistics, Vol. 9, Springer-Verlag, New York.

  Slevinsky, Richard M., and Safouhi, Hassan (2010) A recursive
  algorithm for the G transformation and accurate computation of
  incomplete Bessel functions. \emph{Appl. Numer. Math.}, In press.
}

\author{David Scott \email{d.scott@auckland.ac.nz}, Richard Trendall,
  and Melanie Luen.}

\seealso{
  \code{\link[DistributionUtils]{safeIntegrate}},
  \code{\link{integrate}} for its shortfalls, \code{\link{splinefun}},
  \code{\link{uniroot}} and \code{\link{gigChangePars}} for changing
  parameters to the \eqn{(\chi,\psi)}{(chi,psi)} parameterisation,
  \code{\link{dghyp}} for the generalized hyperbolic distribution.
}

\examples{
param <- c(2, 3, 1)
gigRange <- gigCalcRange(param = param, tol = 10^(-3))
par(mfrow = c(1, 2))
curve(dgig(x, param = param), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Density of the\n Generalized Inverse Gaussian")
curve(pgig(x, param = param), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Distribution Function of the\n Generalized Inverse Gaussian")
dataVector <- rgig(500, param = param)
curve(dgig(x, param = param), range(dataVector)[1], range(dataVector)[2],
      n = 500)
hist(dataVector, freq = FALSE, add = TRUE)
title("Density and Histogram\n of the Generalized Inverse Gaussian")
logHist(dataVector, main =
        "Log-Density and Log-Histogram\n of the Generalized Inverse Gaussian")
curve(log(dgig(x, param = param)), add = TRUE,
      range(dataVector)[1], range(dataVector)[2], n = 500)
par(mfrow = c(2, 1))
curve(dgig(x, param = param), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Density of the\n Generalized Inverse Gaussian")
curve(ddgig(x, param = param), from = gigRange[1], to = gigRange[2],
      n = 1000)
title("Derivative of the Density\n of the Generalized Inverse Gaussian")
}

\keyword{distribution}
