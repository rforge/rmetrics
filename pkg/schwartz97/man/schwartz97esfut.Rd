\name{expected-shortfall-futures}
\docType{methods}
\title{Schwartz two-factor Model: Expected Shortfall for Futures Prices}

\alias{ESfutures}
\alias{ESfutures,ANY,ANY,ANY,numeric-method}
\alias{ESfutures,ANY,ANY,ANY,schwartz2f-method}
\alias{ESfutures,ANY,ANY,ANY,schwartz2f.fit-method}


% <---------------------------------------------------------------------->
\description{

  A function to approximate the Expected Shortfall (ES) for futures
  contracts for arbitrary confidence levels.

}

% <---------------------------------------------------------------------->
\usage{

\S4method{ESfutures}{ANY,ANY,ANY,numeric}(level = 0.95, time = 0.1,
          ttm = 1, g0 = 50, type = c("return", "nominal"),
          delta0 = 0, mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0,
          sigmaE = 0.5, rho = 0.75, r = 0.05, lambda = 0, alphaT = NULL)

\S4method{ESfutures}{ANY,ANY,ANY,schwartz2f}(level = 0.95,
          time = 0.1, ttm = 1, g0, type = c("return", "nominal"), r = 0.05,
          lambda = 0, alphaT = NULL)

\S4method{ESfutures}{ANY,ANY,ANY,schwartz2f.fit}(level = 0.95,
          time = 0.1, ttm = 1, g0, type = c("return", "nominal"))

}

% <---------------------------------------------------------------------->
\arguments{
  \item{level}{Vector containing confidence levels.}
  \item{time}{Time horizon for ES-analysis (see package vignette).}
  \item{ttm}{Time to maturity of the futures contract.}
  \item{g0}{Either a \code{numeric} representing the initial value of the
    \emph{futures price} or an object inheriting from class
  \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{type}{Either \code{"return"} (VaR of negative futures price
    log-return) or \code{"nominal"} (VaR of futures price). For details
    see package vignette.}
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
  \code{\link[=schwartz2f-class]{schwartz2f-classes}}.

}
% <---------------------------------------------------------------------->
\note{

  If either a \code{\link[=schwartz2f-class]{schwartz2f}},
  \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}} object
  is submitted the futures price is calculated first and then the
  default function is called with the calculated futures price
  \code{g0}.

}

% <---------------------------------------------------------------------->
\value{

  Approximative Expected shortfall (ES) (\code{numeric}) for arbitrary
  confidence levels. \code{ESfutures} computes the potential conditional
  loss of a futures contract for a time horizon \code{time} at the
  confidence level \code{level}. See package vignette for details.

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
ESfutures(level = 0.95, time = 0.1, ttm = 0.5, g0 = 45, type = "return",
          alphaT = 0)

## Create a "schwartz2f"-object
model <- schwartz2f()
ESfutures(level = c(0.9, 0.95, 0.99), time = 0.1, ttm = 0.5,
          g0 = model, type = "nominal", alphaT = 0)

}

% <---------------------------------------------------------------------->
\keyword{distribution}
\keyword{models}
