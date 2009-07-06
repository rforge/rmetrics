\name{value-at-risk-futures}
\docType{methods}
\title{Schwartz two-factor Model: Value at Risk for Futures Prices}

\alias{VaRfutures}
\alias{VaRfutures,ANY,ANY,missing-method}
\alias{VaRfutures,ANY,ANY,schwartz2factor-method}
\alias{VaRfutures,ANY,ANY,fit.schwartz2factor-method}


% <---------------------------------------------------------------------->
\description{

  A function to compute the Value at Risk (VaR) for futures contracts
  for arbitrary confidence levels.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{VaRfutures}{ANY,ANY,missing}(level = 0.95, deltat = 0.1, ttm = 1, g0 = 50, delta0 = 0,
           mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.5,
           rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{VaRfutures}{ANY,ANY,schwartz2factor}(level = 0.95, deltat = 0.1, ttm = 1, object, r = 0.05,
           lambda = 0, alphaT = NULL)

\S4method{VaRfutures}{ANY,ANY,fit.schwartz2factor}(level = 0.95, deltat = 0.1, ttm = 1, object)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{level}{Vector of confidence levels.}
  \item{deltat}{Time horizon for VaR-analysis (see package vignette).}
  \item{ttm}{Time to maturity of the futures contract.}
  \item{object}{An object of class
    \code{\link[=schwartz2factor-class]{schwartz2factor}} or
    \code{\link[=fit.schwartz2factor-class]{fit.schwartz2factor}}.}
  \item{g0}{A \code{numeric} representing the initial value of the
    \bold{futures price}.}
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
\note{

  If either a \code{\link[=schwartz2factor-class]{schwartz2factor}},
  \code{\link[=fit.schwartz2factor-class]{fit.schwartz2factor}} object
  is submitted the futures price is calculated first and then the
  default function is called with the calculated futures price
  \code{g0}.

}

% <---------------------------------------------------------------------->
\value{

  Value at Risk (VaR) (\code{numeric}) for arbitrary confidence
  levels. \code{VaRfutures} computes the potential loss of a futures
  contract for a time horizon \code{deltat} at the confidence level
  \code{level}. See package vignette for details.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{VaRfutures}},
  \code{\link{pricefutures}},
  \code{\link{qfutures}},
  \code{\link{dfutures}}

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\examples{
VaRfutures(level = 0.95, deltat = 0.1, ttm = 0.5, g0 = 45, alphaT = 0)

## Create a "schwartz2factor"-obect
model <- schwartz2factor()
VaRfutures(level = c(0.9, 0.95, 0.99), deltat = 0.1, ttm = 0.5,
           object = model, alphaT = 0)

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}
