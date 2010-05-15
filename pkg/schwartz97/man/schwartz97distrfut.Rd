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
         alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{dfutures}{ANY,ANY,ANY,schwartz2f}(x, time = 0.1, ttm = 1, s0, r = 0.05,
lambda = 0, alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{dfutures}{ANY,ANY,ANY,schwartz2f.fit}(x, time = 0.1, ttm = 1, s0, measure = c("P", "Q"), \dots)


\S4method{pfutures}{ANY,ANY,ANY,numeric}(q, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{pfutures}{ANY,ANY,ANY,schwartz2f}(q, time = 0.1, ttm = 1, s0, r = 0.05,
lambda = 0, alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{pfutures}{ANY,ANY,ANY,schwartz2f.fit}(q, time = 0.1, ttm = 1, s0, measure = c("P", "Q"), \dots)

\S4method{qfutures}{ANY,ANY,ANY,numeric}(p, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
         sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0,
         alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{qfutures}{ANY,ANY,ANY,schwartz2f}(p, time = 0.1, ttm = 1, s0, r = 0.05,
lambda = 0, alphaT = NULL, measure = c("P", "Q"), \dots)

\S4method{qfutures}{ANY,ANY,ANY,schwartz2f.fit}(p, time = 0.1, ttm = 1, s0, measure = c("P", "Q"), \dots)

\S4method{rfutures}{ANY,ANY,ANY,numeric}(n, time = 0.1, ttm = 1, s0 = 50, delta0 = 0,
         mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.5,
         rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL, measure = c("P", "Q"))

\S4method{rfutures}{ANY,ANY,ANY,schwartz2f}(n, time = 0.1, ttm = 1, s0, r = 0.05,
lambda = 0, alphaT = NULL, measure = c("P", "Q"))

\S4method{rfutures}{ANY,ANY,ANY,schwartz2f.fit}(n, time = 0.1, ttm = 1, s0, measure = c("P", "Q"))

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
  \item{measure}{under which the functions are computed. \dQuote{P}
    denotes the objective measure, \dQuote{Q} the risk-neutral measure
    (see \bold{Details}).} 
  \item{\dots}{Arguments to be passed to the functions
    \code{\link[stats:dnorm]{d/p/q-norm}}.}

}

% <---------------------------------------------------------------------->
\note{

  Note that futures and forward prices coincide as the interest rate is
  assumed to be constant in the Schwartz two-factor model.

}
% <---------------------------------------------------------------------->
\details{

  Futures prices depend on the spot-price and the convenience yield.\cr

  To get the real (i.e. the objective) distribution of futures prices at
  some date in the future the dynamics is considered under the objective
  measure \emph{P}. The \emph{P}-dynamics is

  \deqn{dS_t = (\mu - \delta_t) S_t dt + \sigma_S~S_t~dW_t^1,}{dS = (mu - delta) * S * dt + sigmaS * S * dW1,}

  \deqn{d \delta_t = \kappa (\alpha - \delta_t) dt +\sigma_E dW_t^2}{d delta = kappa * (alpha - delta) * dt + sigmaE * dW2}
  \deqn{dW_t^1~dW_t^2 = \rho dt,}{dW1 * dW2 = rho dt,} where
  \eqn{W^1, \, W^2}{W1, W2} are Brownian motions under the objective
  measure, the measure \emph{P}.\cr

  Options on futures are evaluated based on the risk-neutral dynamics of
  the spot-price and the convenience yield, i.e. under the measure
  \emph{Q}. The \emph{Q}-dynamics is

  \deqn{dS_t = (r - \delta_t) S_t dt + \sigma_S~S_t~d\tilde{W}_t^1}{dS = (r - delta) * S * dt + sigmaS * S *  dW1*} 

  \deqn{d \delta_t = \kappa (\tilde{\alpha} - \delta_t) dt + \sigma_E d\tilde{W}_t^2,}{d delta = kappa * (alphaT - delta) * dt + sigmaE * dW2*,}

  where \eqn{\tilde{W}^1, \, \tilde{W}^2}{W1*, W2*} are
  Brownian motions with respect to \emph{Q}.\cr

  \eqn{\tilde{\alpha} = \alpha - \lambda / \kappa}{alphaT = alpha -
  lambda / kappa} where \eqn{\lambda}{lambda} is the market price of
  convenience-yield risk.\cr

  The market price of convenience yield risk can either be specified
  explicitly by \code{lambda} or implicitly by \code{alphaT}. The
  relation is \code{alphaT = alpha - lambda / kappa}. See the package
  vignette.

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
  \code{\link{rstate}},
  \code{\link{simstate}}  

}

% <---------------------------------------------------------------------->
\references{

  \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
  52, 1997, 923-973\cr

  \emph{Valuation of Commodity Futures and Options Under Stochastic
    Convenience Yields, Interest Rates, and Jump Diffusions in the Spot}
  by Jimmy E. Hilliard and Jorge Reis \cr Journal of Financial and
  Quantitative Analysis 33, 1998, 61-86

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

## At time 0 the futures price is a deterministic function of s0 and
## delta0. Therefore 3 times the same value is obtained:
rfutures(3, time = 0, ttm = 1, model, lambda = 0)

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}
\keyword{datagen}
