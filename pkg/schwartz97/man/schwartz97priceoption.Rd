\name{pricing-options}
\docType{methods}
\alias{priceoption}
\alias{priceoption,ANY,ANY,ANY,ANY,numeric-method}
\alias{priceoption,ANY,ANY,ANY,ANY,schwartz2f-method}
\alias{priceoption,ANY,ANY,ANY,ANY,schwartz2f.fit-method}

\title{Schwartz two-factor Model: European Option Prices}
% <---------------------------------------------------------------------->
\description{

  Compute arbitrage-free prices of European call and put options on
  commodity futures contracts.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{priceoption}{ANY,ANY,ANY,ANY,numeric}(type = c("call", "put"), time = 0.5, Time = 1, K = 40,
            g0 = 50, sigmaS = 0.3, kappa = 1, sigmaE = 0.5,
            rho = 0.75, r = 0.03)

\S4method{priceoption}{ANY,ANY,ANY,ANY,schwartz2f}(type = c("call", "put"),
            time = 0.5, Time = 1, K = 40,
            g0, r = 0.03, lambda = 0, alphaT = NULL)

\S4method{priceoption}{ANY,ANY,ANY,ANY,schwartz2f.fit}(type = c("call", "put"),
            time = 0.5, Time = 1, K = 40, g0)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{type}{Either a European \code{"call"} or a \code{"put"}
    option on a futures contract.}
  \item{time}{Exercise time of the option.}
  \item{Time}{Maturity date of the underlying futures (see \bold{Details}).}
  \item{K}{Strike price.}
  \item{g0}{The current \emph{futures price} or an object inheriting from class
    \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{kappa}{Speed of mean-reversion of the convenience-yield process.}
  \item{sigmaE}{Diffusion parameter of the convenience-yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot-price and the convenience-yield process.}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{lambda}{Market price of convenience yield risk (see
  \bold{Details}).}
  \item{alphaT}{Mean-level of the convenience yield process with
    respect to the equivalent martingale measure (see \bold{Details}).}

}

% <---------------------------------------------------------------------->
\details{

  The price of an option on the spot commodity is obtained by setting
  \code{time == Time}. This is because of the convergence of the futures
  price towards the spot price at maturity.  In general the option
  expires before the futures contract (\code{time < Time}).\cr

  If \code{g0} is either of class
  \code{\link[=schwartz2f-class]{schwartz2f}} or class
  \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}} the futures price
  \code{g0} is computed first and then plugged into the pricing function
  with signature \code{ANY,ANY,ANY,ANY,numeric}.\cr

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.

}
% <---------------------------------------------------------------------->
\note{

  Since the two-factor model assumes a constant interest rate, futures
  and forwards always have the same value and therefore also any
  derivative of futures or forwards.

}

% <---------------------------------------------------------------------->
\value{

  A \code{numeric} containing the option prices.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{pricefutures}} to price futures,
  \code{\link[=pfutures]{d/p/q/rfutures}} to work with futures,
  \code{\link[=schwartz2f-class]{schwartz2f}}-constructor,
  \code{\link{fit.schwartz2f}} for parameter estimation.

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973 \cr

    \emph{Pricing of Options on Commodity Futures with Stochastic Term
    Structures of Convenience Yields and Interest Rates} by Kristian
    R. Miltersen and Eduardo S. Schwartz \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 33-59 \cr

    \emph{Valuation of Commodity Futures and Options under Stochastic
      Convenience Yields, Interest Rates, and Jump Diffusions in the Spot}
    by Jimmy E. Hilliard and Jorge Reis \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 61-86

}
% <---------------------------------------------------------------------->
\examples{

## The option expires in 0.5 years and the futures contract in 1 year.
priceoption(type = "call", time = 0.5, Time = 1, K = 40, g0 = 50)

## The price of a European put option on the spot which expires in 2.5
## years.
priceoption(type = "put", time = 2.5, Time = 2.5, K = 900, lambda = 0.02,
            g0 = schwartz2f(s0 = 1000))

}
% <---------------------------------------------------------------------->
\keyword{models}
\keyword{derivative}
