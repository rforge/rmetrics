\name{pricing-futures}
\docType{methods}
\title{Schwartz two-factor Model: Futures Prices}

\alias{pricefutures}
\alias{pricefutures,ANY,numeric-method}
\alias{pricefutures,ANY,schwartz2f-method}
\alias{pricefutures,ANY,schwartz2f.fit-method}

% <---------------------------------------------------------------------->
\description{

  A function to compute futures prices.

}

% <---------------------------------------------------------------------->
\usage{
\S4method{pricefutures}{ANY,numeric}(ttm = 1, s0 = 50, delta0 = 0, sigmaS = 0.3,
             kappa = 1, alpha = 0, sigmaE = 0.5, rho = 0.75,
             r = 0.05, lambda = 0, alphaT = NULL)

\S4method{pricefutures}{ANY,schwartz2f}(ttm = 1, s0, r = 0.05,
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
  \item{kappa}{Speed of mean-reversion of the convenience-yield process.}
  \item{alpha}{Mean-level of the convenience-yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{sigmaE}{Diffusion parameter of the convenience-yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot-price and the convenience-yield process.}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{lambda}{Market price of convenience yield risk (see
  \bold{Details}).}
  \item{alphaT}{Mean-level of the convenience yield process with
    respect to the equivalent martingale measure (see \bold{Details}).}
  \item{\dots}{Arguments to be passed to methods of package
    \code{\link[stats]{stats}}.}

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

  A \code{numeric} giving futures prices.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link{priceoption}},
  \code{\link{pfutures}},
  \code{\link{dfutures}},
  \code{\link{qfutures}},
  \code{\link{rfutures}}

}

% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\examples{

  pricefutures(ttm = 2, s0 = 10, delta0 = 0)

}

% <---------------------------------------------------------------------->
\keyword{models}
