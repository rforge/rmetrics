\name{distribution-futures}
\docType{methods}
\title{Schwartz two-factor Model: Distribution of Futures Prices}

\alias{pfutures}
\alias{pfutures,ANY,ANY,ANY,numeric-method}
\alias{pfutures,ANY,ANY,ANY,schwartz2f-method}
\alias{pfutures,ANY,ANY,ANY,schwartz2f.fit-method}

\alias{dfutures}
\alias{dfutures,ANY,ANY,ANY,numeric-method}
\alias{dfutures,ANY,ANY,ANY,schwartz2f-method}
\alias{dfutures,ANY,ANY,ANY,schwartz2f.fit-method}

\alias{qfutures}
\alias{qfutures,ANY,ANY,ANY,numeric-method}
\alias{qfutures,ANY,ANY,ANY,schwartz2f-method}
\alias{qfutures,ANY,ANY,ANY,schwartz2f.fit-method}

\alias{rfutures}
\alias{rfutures,ANY,ANY,ANY,numeric-method}
\alias{rfutures,ANY,ANY,ANY,schwartz2f-method}
\alias{rfutures,ANY,ANY,ANY,schwartz2f.fit-method}


% <---------------------------------------------------------------------->
\description{

  Density, distribution function, quantile function and random number
  generation of futures prices. Notice that futures prices are
  log-normally distributed.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{dfutures}{ANY,ANY,ANY,numeric}(x, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{dfutures}{ANY,ANY,ANY,schwartz2f}(x, time = 0.1, ttm = 1, s0, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{dfutures}{ANY,ANY,ANY,schwartz2f.fit}(x, time = 0.1, ttm = 1, s0, \dots)


\S4method{pfutures}{ANY,ANY,ANY,numeric}(q, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{pfutures}{ANY,ANY,ANY,schwartz2f}(q, time = 0.1, ttm = 1, s0, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{pfutures}{ANY,ANY,ANY,schwartz2f.fit}(q, time = 0.1, ttm = 1, s0, \dots)

\S4method{qfutures}{ANY,ANY,ANY,numeric}(p, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, \dots)

\S4method{qfutures}{ANY,ANY,ANY,schwartz2f}(p, time = 0.1, ttm = 1, s0, r = 0.05, lambda = 0, alphaT = NULL, \dots)

\S4method{qfutures}{ANY,ANY,ANY,schwartz2f.fit}(p, time = 0.1, ttm = 1, s0, \dots)

\S4method{rfutures}{ANY,ANY,ANY,numeric}(n, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{rfutures}{ANY,ANY,ANY,schwartz2f}(n, time = 0.1, ttm = 1, s0, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{rfutures}{ANY,ANY,ANY,schwartz2f.fit}(n, time = 0.1, ttm = 1, s0)

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
  \item{s0}{Either a \code{numeric} representing the initial value of the
    commodity spot price or an object inheriting from class
  \code{\link[=schwartz2f-class]{schwartz2f}}.}
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
  \code{\link[=schwartz2f-class]{schwartz2f-classes}}.

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
## Create a "schwartz2f"-object
model <- schwartz2f()

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
