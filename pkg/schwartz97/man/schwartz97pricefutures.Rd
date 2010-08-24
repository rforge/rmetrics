\name{pricing-futures}
\docType{methods}
\alias{pricefutures}
\alias{pricefutures,ANY,numeric-method}
\alias{pricefutures,ANY,schwartz2f-method}
\alias{pricefutures,ANY,schwartz2f.fit-method}

\title{Schwartz two-factor Model: Futures Prices}
% <---------------------------------------------------------------------->
\description{

  Compute arbitrage-free futures prices.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{pricefutures}{ANY,numeric}(ttm = 1, s0 = 50, delta0 = 0, sigmaS = 0.3,
             kappa = 1, alpha = 0, sigmaE = 0.5, rho = 0.75,
             r = 0.03, lambda = 0, alphaT = NULL)

\S4method{pricefutures}{ANY,schwartz2f}(ttm = 1, s0, r = 0.03,
             lambda = 0, alphaT = NULL)

\S4method{pricefutures}{ANY,schwartz2f.fit}(ttm = 1, s0)

}

% <---------------------------------------------------------------------->
\arguments{

  \item{ttm}{Time to maturity.}
  \item{s0}{Either a \code{numeric} representing the initial value of the
    commodity spot price or an object inheriting from class
  \code{\link[=schwartz2f-class]{schwartz2f}}.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{kappa}{Speed of mean-reversion of the convenience-yield process.}
  \item{alpha}{Mean-level of the convenience-yield process.}
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

  The model and its parameters are described in the \bold{Details}
  section of the \code{\link[=schwartz2f-class]{schwartz2f}}-class
  documentation and in the package vignette \emph{Technical Document}.

}

% <---------------------------------------------------------------------->
\value{

  A \code{numeric} containing futures prices.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{priceoption}} to price options,
  \code{\link[=pfutures]{d/p/q/rfutures}} to work with futures,
  \code{\link[=schwartz2f-class]{schwartz2f}}-constructor,
  \code{\link{fit.schwartz2f}} for parameter estimation.

}

% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973 \cr

    \emph{Valuation of Commodity Futures and Options under Stochastic
      Convenience Yields, Interest Rates, and Jump Diffusions in the Spot}
    by Jimmy E. Hilliard and Jorge Reis \cr Journal of Financial and
    Quantitative Analysis 33, 1998, 61-86

}
% <---------------------------------------------------------------------->
\examples{

## function call by atomic arguments
forward.curve <- pricefutures(ttm = 0.2 * 1:10, s0 = 10, delta0 = 0,
                              alpha = 0, lambda = 0.02, r = 0)
plot(forward.curve, type = "b")

## function call via schwartz2f-object. 
obj <- schwartz2f(delta0 = 0, sigmaE = 1e-5) # Make convenience yield inactive
forward.curve <- pricefutures(ttm = 0.2 * 1:10, s0 = obj, r = 0, alphaT = 0)
plot(forward.curve, type = "b")


}
% <---------------------------------------------------------------------->
\keyword{models}
\keyword{derivative}

