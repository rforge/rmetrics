\name{filter-futures}
\docType{methods}
\title{Schwartz two-factor Model: Filter futures data}

\alias{filter2factor}
\alias{filter2factor,ANY,ANY,missing-method}
\alias{filter2factor,ANY,ANY,schwartz2factor-method}
\alias{filter2factor,ANY,ANY,fit.schwartz2factor-method}

% <---------------------------------------------------------------------->
\description{

  Filter a series of futures prices to obtain expected state variables.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{filter2factor}{ANY,ANY,missing}(data, ttm, s0 = 50, delta0 = 0,
              mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.5,
              rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL,
              deltat = 1/260, meas.sd = rep(1e-3, ncol(data)),
              P0 = 0.5 * diag(c(log(s0), abs(delta0))))

\S4method{filter2factor}{ANY,ANY,schwartz2factor}(data, ttm, object,
              r = 0.05, lambda = 0, alphaT = NULL, deltat = 1/260,
              meas.sd = rep(1e-3, ncol(data)),
              P0 = 0.1 * diag(2))

\S4method{filter2factor}{ANY,ANY,fit.schwartz2factor}(data, ttm, object)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
  \item{object}{An object inheriting from class
    \code{\link[=schwartz2factor-class]{schwartz2factor}}.}
  \item{s0}{A \code{numeric} representing the initial value of the
    commodity spot price.}
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
    respect to the equivalent martingale measure (see \bold{Details}).}
  \item{deltat}{Time increment (see \bold{Details}).}
  \item{meas.sd}{The standard deviation of the measurement equation (see
    \bold{Details}).}
  \item{P0}{Variance of the state variables \code{s0} and \code{P0}.}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the price of the futures
  contract with time to maturity \code{ttm[i,j]}. \cr

  The market price of convenience yield risk can either be specified
  explicitly by \code{lambda} or implicigtly by \code{alphaT}. The
  relation is \code{alphaT = alpha - lambda / kappa}. See the package
  vignette or
  \code{\link[=schwartz2factor-class]{schwartz2factor-classes}}. \cr


  The time increment observation \code{data[i,j]} and \code{data[i +
    1,j]} is denoted with \code{deltat}.

}

% <---------------------------------------------------------------------->
\value{

  A list with components:
  \tabular{rl}{
  \code{state} \tab A matrix giving the the expected spot price and the
  convenience yield. The \code{colnames} are \dQuote{S} and \dQuote{delta}\cr
  \code{fkf.obj} \tab The filter output from the package
  \code{\link[FKF:fkf]{fkf}}. Note that the log of the commodity spot
  price is filtered.\cr
  }

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{fit2factor}}, \code{\link{pricefutures}}.

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
% \examples{

% }

% <---------------------------------------------------------------------->
\keyword{iteration}
