\name{schwartz2factor-constructor}
\alias{schwartz2factor}
\title{Create a schwartz2factor object}

\description{
  Create an object of class \code{schwartz2factor}.
}

% <---------------------------------------------------------------------->
\usage{
schwartz2factor(s0 = 100, delta0 = 0,
                mu = 0.1, sigmaS = 0.3,
                kappa = 1, alpha = 0, sigmaE = 0.3, rho = 0.5)

}
% <---------------------------------------------------------------------->
\arguments{

  \item{s0}{Initial value of the commodity spot price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}

}
% <---------------------------------------------------------------------->
\details{

  The dynamics of the Schwartz two-factor model is given in
  \code{\link[=schwartz2factor-class]{schwartz2factor}} or in the
  package vignette in the doc-folder.

}
% <---------------------------------------------------------------------->
\value{
 An object of class \code{\link[=schwartz2factor-class]{schwartz2factor}}.
}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}
% <---------------------------------------------------------------------->
\references{

    \emph{The Stochastic Behavior of Commodity Prices: Implications for
    Valuation and Hedging} by Eduardo S. Schwartz \cr Journal of Finance
    52, 1997, 923-973

}
% <---------------------------------------------------------------------->
\seealso{
  \code{\link{fit2factor}}.
}
% <---------------------------------------------------------------------->
\examples{

  ## Initialize a 'schwartz2factor' object
  ## with little convenience yield volatility:
  obj <- schwartz2factor(sigmaE = 1e-6)

  trajectory <- simstate(n = 1000, time = 1, obj)

  plot(trajectory[, "S"])

}
% <---------------------------------------------------------------------->
\keyword{models}
