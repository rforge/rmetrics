\name{distribution-futures}
\docType{methods}
\title{Schwartz two-factor Model: Distribution of Futures Prices}

\alias{pfutures}
\alias{pfutures,ANY,ANY,ANY,missing-method}
\alias{pfutures,ANY,ANY,ANY,schwartz2factor-method}
\alias{pfutures,ANY,ANY,ANY,fit.schwartz2factor-method}

\alias{dfutures}
\alias{dfutures,ANY,ANY,ANY,missing-method}
\alias{dfutures,ANY,ANY,ANY,schwartz2factor-method}
\alias{dfutures,ANY,ANY,ANY,fit.schwartz2factor-method}

\alias{qfutures}
\alias{qfutures,ANY,ANY,ANY,missing-method}
\alias{qfutures,ANY,ANY,ANY,schwartz2factor-method}
\alias{qfutures,ANY,ANY,ANY,fit.schwartz2factor-method}

\alias{rfutures}
\alias{rfutures,ANY,ANY,ANY,missing-method}
\alias{rfutures,ANY,ANY,ANY,schwartz2factor-method}
\alias{rfutures,ANY,ANY,ANY,fit.schwartz2factor-method}


% <---------------------------------------------------------------------->
\description{

  Density, distribution function, quantile function and random number
  generation of futures prices. Notice that futures prices are
  log-normally distributed.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{dfutures}{ANY,ANY,ANY,missing}(x, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{dfutures}{ANY,ANY,ANY,schwartz2factor}(x, time = 0.1, ttm = 1, object, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{dfutures}{ANY,ANY,ANY,fit.schwartz2factor}(x, time = 0.1, ttm = 1, object, \dots)


\S4method{pfutures}{ANY,ANY,ANY,missing}(q, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{pfutures}{ANY,ANY,ANY,schwartz2factor}(q, time = 0.1, ttm = 1, object, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{pfutures}{ANY,ANY,ANY,fit.schwartz2factor}(q, time = 0.1, ttm = 1, object, \dots)

\S4method{qfutures}{ANY,ANY,ANY,missing}(p, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{qfutures}{ANY,ANY,ANY,schwartz2factor}(p, time = 0.1, ttm = 1, object, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{qfutures}{ANY,ANY,ANY,fit.schwartz2factor}(p, time = 0.1, ttm = 1, object, \dots)

\S4method{rfutures}{ANY,ANY,ANY,missing}(n, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{rfutures}{ANY,ANY,ANY,schwartz2factor}(n, time = 0.1, ttm = 1, object, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{rfutures}{ANY,ANY,ANY,fit.schwartz2factor}(n, time = 0.1, ttm = 1, object)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{q, x}{vector of quantiles.}
  \item{p}{vector of probabilities.}
  \item{n}{number of observations. If \code{length(n) > 1}, the length
    is taken to be the number required.}
  \item{time}{Time where the futures process is evaluated
    (relative to time zero).}
  \item{ttm}{Time to maturity.}
  \item{object}{An object inheriting from class
    \code{\link[=schwartz2factor-class]{schwartz2factor}}.}
  \item{s0}{A \code{numeric} representing the initial value of the
    commodity spot price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{Drift term of commodity spot price.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{lambda}{Market price of convenience yield risk (see
  \bold{Details}).}
  \item{alphaT}{Mean-level of the convenience yield process with
    respect to the equivalent martingale measure (see \bold{Details}).}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{\dots}{Arguments to be passed to the functions
    \code{\link[stats:dnorm]{d/p/q-norm}}.}

}

% <---------------------------------------------------------------------->
\details{

  The market price of convenience yield risk can either be specified
  explicitly by \code{lambda} or implicitly by \code{alphaT}. The
  relation is \code{alphaT = alpha - lambda / kappa}. See the package
  vignette or
  \code{\link[=schwartz2factor-class]{schwartz2factor-classes}}.

}

% <---------------------------------------------------------------------->
\value{

  Probabilities, densities, quantiles or samples of the log-normally
  distributed futures prices as \code{vector}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{pricefutures}},
  \code{\link{pstate}},
  \code{\link{dstate}},
  \code{\link{qstate}},
  \code{\link{rstate}}

}

% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\examples{
## Create a "schwartz2factor"-object
model <- schwartz2factor()

## Probability
pfutures(q = 10 * 3:9, time = 0.5, ttm = 2, model, lambda = 0.01)

## Density
dfutures(x = c(20, 40, 100), time = 0.5, ttm = 2, model, lambda = 0.01)

## Quantile
qfutures(p = 0.1 * 2:5, time = 0.5, ttm = 10, model, lambda = 0.01)

## Sample
sim <- rfutures(n = 1000, time = 0.5, ttm = 5, model, lambda = 0.01)
hist(sim)

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}
\keyword{datagen}
