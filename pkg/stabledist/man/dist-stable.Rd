\name{StableDistribution}

\title{Stable Distribution Function}

\alias{StableDistribution}
\alias{dstable}
\alias{pstable}
\alias{qstable}
\alias{rstable}

\concept{Stable Distribution}

\description{
    A collection and description of functions to compute
    density, distribution and quantile function and
    to generate random variates of the stable distribution.

    The functions are:
    \tabular{ll}{
      \code{[dpqr]stable} \tab the (skewed) stable distribution.
    }
}

\usage{
dstable(x, alpha, beta, gamma = 1, delta = 0, pm = 0,
        log = FALSE,
        tol = 16*.Machine$double.eps, subdivisions = 1000)
pstable(q, alpha, beta, gamma = 1, delta = 0, pm = 0,
	tol = 16*.Machine$double.eps, subdivisions = 1000)
qstable(p, alpha, beta, gamma = 1, delta = 0, pm = 0,
        tol = .Machine$double.eps^0.25, maxiter = 1000,
        integ.tol = 1e-7, subdivisions = 200)
rstable(n, alpha, beta, gamma = 1, delta = 0, pm = 0)
}

\arguments{
  \item{alpha, beta, gamma, delta}{
    value of the index parameter \code{alpha} with \code{alpha = (0,2]};
    skewness parameter \code{beta}, in the range [-1, 1];
    scale parameter \code{gamma}; and
    shift parameter \code{delta}.
  }
  \item{n}{sample size (integer).}
  \item{p}{numeric vector of probabilities.}
  \item{pm}{
    parameterization, an integer in \code{0, 1, 2}; by default \code{pm=0},
    the \sQuote{S0} parameterization.}
  \item{x, q}{numeric vector of quantiles.}
  \item{log}{logical; if TRUE, the probabilities \code{p} are given as
    \code{log(p)}.}
  \item{integ.tol}{positive number, the tolerance used for numerical
    integration, see \code{\link{integrate}}.}
  \item{tol}{numerical tolerance,
    \describe{
      \item{dstable(), pstable():}{used for numerical integration, see
	\code{integ.tol} above.}
      \item{qstable():}{used for rootfinding, see \code{\link{uniroot}}.}
      }
  }
  \item{subdivisions}{maximal number of intervals for integration, see
    \code{\link{integrate}}.}
  \item{maxiter}{maximal number of iterations in \code{\link{uniroot}}.}
}

\value{
    All values for the \code{*stable} functions
    are numeric vectors:
    \code{d*} returns the density,
    \code{p*} returns the distribution function,
    \code{q*} returns the quantile function, and
    \code{r*} generates random deviates.
}

\details{
    \bold{Skew Stable Distribution:}
    \cr\cr
    The function uses the approach of J.P. Nolan for general
    stable distributions. Nolan derived expressions in form of
    integrals based on the characteristic function for standardized
    stable random variables. These integrals are numerically
    evaluated using \R's \code{\link{integrate}()} function,
    following Nolan (1997).
    \cr
    \dQuote{S0} parameterization [pm=0]: based on the (M) representation
    of Zolotarev for an alpha stable distribution with skewness
    beta. Unlike the Zolotarev (M) parameterization, gamma and
    delta are straightforward scale and shift parameters. This
    representation is continuous in all 4 parameters, and gives
    an intuitive meaning to gamma and delta that is lacking in
    other parameterizations.
    \cr
    Switching the sign of \code{beta} \emph{mirrors} the distribution at
    the vertical axis \eqn{x = \delta}{x = delta}, i.e.,
    \deqn{f(x, \alpha, -\beta, \gamma, \delta, 0) =
      f(2\delta-x, \alpha, +\beta, \gamma, \delta, 0),}
    see the graphical example below.

    \dQuote{S} or \dQuote{S1} parameterization [pm=1]: the parameterization used
    by Samorodnitsky and Taqqu in the book Stable Non-Gaussian
    Random Processes. It is a slight modification of Zolotarev's
    (A) parameterization.
    \cr
    \dQuote{S*} or \dQuote{S2} parameterization [pm=2]: a modification of the S0
    parameterization which is defined so that (i) the scale gamma
    agrees with the Gaussian scale (standard dev.) when alpha=2
    and the Cauchy scale when alpha=1, (ii) the mode is exactly at
    delta.
    \cr
    \dQuote{S3} parameterization [pm=3]: an internal parameterization,
    currently not available for these functions.  The scale is the same
    as the \dQuote{S2} parameterization, the shift is
    \eqn{-beta*g(alpha)}, where \eqn{g(alpha)} is defined in Nolan(1999).
}

\seealso{
  the \code{\link[fBasics]{stableSlider}()} function for displaying
  densities and probabilities of these distributions, for educational
  purposes.
}
\author{
  Diethelm Wuertz for the original Rmetrics \R-port.
  Numerical improvements by Martin Maechler.
}


\references{
Chambers J.M., Mallows, C.L. and Stuck, B.W. (1976);
    \emph{A Method for Simulating Stable Random Variables},
    J. Amer. Statist. Assoc. 71, 340--344.

Nolan J.P. (1999);
    \emph{Stable Distributions},
    Preprint, University Washington DC, 30 pages.

Nolan J.P. (1997)
    Numerical calculation of stable densities and distribution
    functions.
    \emph{Stochastic Models} \bold{13}(4), 759--774.
    \cr
    Also available as \file{density.ps} from Nolan's web page.

Samoridnitsky G., Taqqu M.S. (1994);
    \emph{Stable Non-Gaussian Random Processes, Stochastic Models
    with Infinite Variance},
    Chapman and Hall, New York, 632 pages.

Weron, A., Weron R. (1999);
    \emph{Computer Simulation of Levy alpha-Stable Variables and
    Processes},
    Preprint Technical University of Wroclaw, 13 pages.
}

\examples{
## stable -

## Plot stable random number series
   set.seed(1953)
   r <- rstable(n = 1000, alpha = 1.9, beta = 0.3)
   plot(r, type = "l", main = "stable: alpha=1.9 beta=0.3",
        col = "steelblue")
   grid()

## Plot empirical density and compare with true density:
   hist(r, n = 25, probability = TRUE, border = "white",
        col = "steelblue")
   x <- seq(-5, 5, 0.25)
   lines(x, dstable(x, alpha = 1.9, beta = 0.3, tol= 1e-3), lwd = 2)

## Plot df and compare with true df:
   plot(ecdf(r), do.points=TRUE, col = "steelblue",
        main = "Probabilities:  ecdf(rstable(1000,..)) and true  cdf F()")
   rug(r)
   lines(x, pstable(q = x, alpha = 1.9, beta = 0.3),
         col="#0000FF88", lwd= 2.5)

## Switching  sign(beta)  <==> Mirror the distribution around  x == delta:
curve(dstable(x, alpha=1.2, beta =  .8, gamma = 3, delta = 2), -10, 10)
curve(dstable(x, alpha=1.2, beta = -.8, gamma = 3, delta = 2),
      add=TRUE, col=2)
## or the same
curve(dstable(2*2-x, alpha=1.2, beta = +.8, gamma = 3, delta = 2),
      add=TRUE, col=adjustcolor("gray",0.2), lwd=5)
abline(v = 2, col = "gray", lty=2, lwd=2)
axis(1, at = 2, label = expression(delta == 2))

## Compute quantiles:
   x. <- -4:4
   px <- pstable(x., alpha = 1.9, beta = 0.3)
  (qs <- qstable(px, alpha = 1.9, beta = 0.3))
   stopifnot(all.equal(as.vector(qs), x., tol = 1e-5))
}

\keyword{distribution}

