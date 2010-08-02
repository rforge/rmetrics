\name{filter-futures}
\docType{methods}
\title{Schwartz two-factor Model: Filter futures data}

\alias{filter.schwartz2f}
\alias{filter.schwartz2f,ANY,ANY,numeric-method}
\alias{filter.schwartz2f,ANY,ANY,schwartz2f-method}
\alias{filter.schwartz2f,ANY,ANY,schwartz2f.fit-method}

% <---------------------------------------------------------------------->
\description{

  Filter a series of futures prices to obtain expected state variables.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{filter.schwartz2f}{ANY,ANY,numeric}(data, ttm, s0 = 50, delta0 = 0,
              mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.5,
              rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL,
              deltat = 1/260, meas.sd = rep(1e-3, ncol(data)),
              P0 = 0.5 * diag(c(log(s0), abs(delta0))))

\S4method{filter.schwartz2f}{ANY,ANY,schwartz2f}(data, ttm, s0,
              r = 0.05, lambda = 0, alphaT = NULL, deltat = 1/260,
              meas.sd = rep(1e-3, ncol(data)),
              P0 = 0.1 * diag(2))

\S4method{filter.schwartz2f}{ANY,ANY,schwartz2f.fit}(data, ttm, s0)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
  \item{s0}{Either a \code{numeric} representing the initial value of the
    commodity spot price or an object inheriting from class
  \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{sigmaS}{Diffusion parameter of the spot price process.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{lambda}{Market price of convenience yield risk.}
  \item{alphaT}{Mean-level of the convenience yield process with
    respect to the equivalent martingale measure.}
  \item{deltat}{Time increment.}
  \item{meas.sd}{The standard deviation of the measurement equation (see
    \bold{Details} section of \code{\link{fit.schwartz2f}}).}
  \item{P0}{Variance of the state variables \code{s0} and \code{P0}.}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the futures price whose time
  to maturity was \code{ttm[i,j]} when it was observed. \cr

  \code{deltat} is the time between observation \code{data[i,j]} and
    \code{data[i + 1,j]}. It is assumed to be constant, i.e., that
    \code{data} is a regular time-series.

}

% <---------------------------------------------------------------------->
\value{

  A list with components:
  \tabular{rl}{
  \code{state} \tab A matrix giving the the expected spot price and the
  convenience yield. The \code{colnames} are \dQuote{S} and \dQuote{delta}.\cr
  \code{fkf.obj} \tab The filter output from the package
  \code{\link[FKF:fkf]{fkf}}. Note that the log of the commodity spot
  price is filtered.\cr
  }

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit.schwartz2f}}, \code{\link{pricefutures}}.

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\examples{

data(futures)

### Estimate parameters for coffee data (stop after 20 iterations)
fit.obj <- fit.schwartz2f(futures$coffee$price, futures$coffee$ttm / 260,
                          control = list(maxit = 20))

### Filter the futures data to get the spot price and the convenience yield.
filtered <- filter.schwartz2f(futures$coffee$price, futures$coffee$ttm / 260, fit.obj)


### ...and plot it.
par(mfrow = c(2, 1))
plot(filtered$state[,1], ylab = "Spot price", type = "l")
lines(futures$coffee$price[,1], col = "red") # Closest to maturity futures
plot(filtered$state[,2], ylab = "Convenience yield", type = "l")
abline(h = 0)

}

% <---------------------------------------------------------------------->
\keyword{iteration}
