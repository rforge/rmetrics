\name{schwartz2f-constructor}
\alias{schwartz2f}
\title{Create schwartz2f objects}
% <---------------------------------------------------------------------->
\description{
  Create objects of class \code{\link[=schwartz2f-class]{schwartz2f}}.
}

% <---------------------------------------------------------------------->
\usage{
schwartz2f(s0 = 100, delta0 = 0, mu = 0.1, sigmaS = 0.3,
           kappa = 1, alpha = 0, sigmaE = 0.3, rho = 0.5)

}
% <---------------------------------------------------------------------->
\arguments{

  \item{s0}{Initial value of the commodity spot price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{sigmaS}{Diffusion parameter of the spot price-process.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}

}
% <---------------------------------------------------------------------->
\details{

  The dynamics of the Schwartz two-factor model is explained in the
  \code{\link[=schwartz2f-class]{schwartz2f}} class documentation or in
  the package vignette in the doc-folder.

}
% <---------------------------------------------------------------------->
\value{

  An object of class \code{\link[=schwartz2f-class]{schwartz2f}}.

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

  \code{\link{fit.schwartz2f}} for parameter estimation.
  \code{\link[=dstate]{d/p/q/r/simstate}} for the density, distribution,
  and quantile function of the state variables and random number
  generation. \code{\link[=plot.schwartz2f]{plot}}-method for
  \code{\link[=schwartz2f-class]{schwartz2f}}-objects.
  
}
% <---------------------------------------------------------------------->
\examples{

## Initialize a 'schwartz2f' object with high convenience yield volatility:
obj <- schwartz2f(sigmaE = 0.7)

plot(obj) # plot it

rstate(10, time = 1, s0 = obj) # generate 10 random variates.

## Get the probability of the event 'the spot price is >= 100 and the
## convenience yield is >= 0':
pstate(c(0, -Inf), c(100, 0), time = 10, s0 = obj) 

}
% <---------------------------------------------------------------------->
\keyword{models}
