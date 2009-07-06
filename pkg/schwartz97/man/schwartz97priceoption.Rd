\name{pricing-options}
\docType{methods}
\title{Schwartz two-factor Model: European Option Prices}

\alias{priceoption}
\alias{priceoption,ANY,ANY,ANY,ANY,missing-method}
\alias{priceoption,ANY,ANY,ANY,ANY,schwartz2factor-method}
\alias{priceoption,ANY,ANY,ANY,ANY,fit.schwartz2factor-method}


% <---------------------------------------------------------------------->
\description{

  Computes arbitrage-free prices of European call and put options on
  commodity futures contracts.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{priceoption}{ANY,ANY,ANY,ANY,missing}(type = c("call", "put"), time = 0.5, Time = 1, K = 40,
            g0 = 50, sigmaS = 0.3, kappa = 1, sigmaE = 0.5,
            rho = 0.75, r = 0.05)

\S4method{priceoption}{ANY,ANY,ANY,ANY,schwartz2factor}(type = c("call", "put"), time = 0.5, Time = 1, K = 40, object,
            r = 0.05, lambda = 0, alphaT = NULL)

\S4method{priceoption}{ANY,ANY,ANY,ANY,fit.schwartz2factor}(type = c("call", "put"), time = 0.5, Time = 1, K = 40, object)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{type}{Valuing either a European \code{"call"} or a \code{"put"}
    option on a futures/ forward.}
  \item{time}{Exercise time of the option.}
  \item{Time}{Maturity date of the underlying instrument (see \bold{Details}).}
  \item{K}{Strike price at exercise time \code{time}.}
  \item{object}{An object of class
    \code{\link[=schwartz2factor-class]{schwartz2factor}} or
    \code{\link[=fit.schwartz2factor-class]{fit.schwartz2factor}}.}
  \item{g0}{A \code{numeric} representing the current futures price.}
  \item{kappa}{Speed of mean-reversion of the convenience-yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
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

  The price of an option on the spot commodity is obtained by simply
  setting \code{time == Time}. This is because of the convergence of the
  futures price towards the spot price at maturity.  In general the
  option expires before the futures contract (\code{time < Time}).\cr

  The market price of convenience yield risk can either be specified
  explicitly by \code{lambda} or implicigtly by \code{alphaT}. The
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

  A \code{numeric} giving the option prices.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{pricefutures}}, \code{\link{fit2factor}}
  \code{\link{dfutures}}.

}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973 \cr

    \emph{Pricing of Options on Commodity Futures with Stochastic Term
    Structures of Convenience Yields and Interest Rates} by Kristian
    R. Miltersen and Eduardo S. Schwartz \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 33-59

}
% <---------------------------------------------------------------------->
\examples{

priceoption(type = "call", time = 0.5, Time = 1, K = 40, g0 = 50)
priceoption(type = "put", time = 1, Time = 1, K = 40, g0 = 50)

}

% <---------------------------------------------------------------------->
\keyword{models}
