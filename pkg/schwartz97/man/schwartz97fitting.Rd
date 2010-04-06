\name{parameter-estimation}
\alias{fit.schwartz2f}
\title{Schwartz 1997 commodity model}

% <---------------------------------------------------------------------->
\description{

  Fit the Schwartz 1997 commodity model to futures data

}

% <---------------------------------------------------------------------->
\usage{

fit.schwartz2f(data, ttm, deltat = 1 / 260,
               s0 = data[1,1], delta0 = 0,
               mu = 0.1, sigmaS = 0.3, kappa = 1, alpha = 0, sigmaE = 0.5,
               rho = 0.75, lambda = 0,
               meas.sd = rep(0.02, ncol(data)),
               opt.pars = c(s0 = FALSE, delta0 = FALSE, mu = TRUE, sigmaS = TRUE,
                            kappa = TRUE, alpha = TRUE, sigmaE = TRUE,
                            rho = TRUE, lambda = TRUE),
               opt.meas.sd = c("scalar", "all", "none"),
               % P0 = 2 * diag(c(var(as.vector(data)), 1)),
               r = 0.05, silent = FALSE, \dots)

}

% <---------------------------------------------------------------------->
\arguments{
  \item{data}{A matrix with futures prices.}
  \item{ttm}{A matrix with the corresponding time to maturity (see \bold{Details}).}
  \item{deltat}{Time increment (see \bold{Details}).}
  \item{s0}{Initial value of the commodity spot price.}
  \item{delta0}{Initial value of the convenience yield.}
  \item{mu}{enters the drift of the commodity spot price.}
  \item{kappa}{Speed of mean-reversion of the convenience yield process.}
  \item{alpha}{Mean-level of the convenience yield process.}
  \item{sigmaS}{Diffusion parameter of the spot price process.}
  \item{sigmaE}{Diffusion parameter of the convenience yield process.}
  \item{rho}{Correlation coefficient between the Brownian motion
    driving the spot price and the convenience yield process.}
  \item{meas.sd}{The standard deviation of the measurement equation (see \bold{Details}).}
  \item{lambda}{Market price of convenience yield risk.}
  \item{opt.pars}{A logical vector which gives the parameters which
    shall be fitted.}
  \item{opt.meas.sd}{States how the standard deviation of the
    measurement equation should be treated (see \bold{Details}).}
  % \item{P0}{Variance of the state variables \code{s0} and \code{P0}.}
  \item{r}{Instantaneous risk-free interest rate.}
  \item{silent}{If \code{FALSE} the log-likelihood and parameters will
    be printed in each iteration.}
  \item{\dots}{Arguments passed to \code{\link{optim}}.}
}

% <---------------------------------------------------------------------->
\details{

  The elements of \code{data} and \code{ttm} have the following
  interpretation: \code{data[i,j]} denotes the price of the futures
  contract with time to maturity \code{ttm[i,j]}. \cr

  The time increment observation \code{data[i,j]} and \code{data[i +
    1,j]} is denoted with \code{deltat}.\cr

  \code{opt.meas.sd} is used to determine how measurement uncertainty is
  treated in the fit: According to the model there should be a
  one-to-one correspondance between the spot and the futures price. In
  reality, futures prices with different maturities but the same
  observation date lead to different spot prices. This is reflected in
  the measurement uncertainty \code{meas.sd}. The parameters
  \code{meas.sd} can be fitted. However, it might be sufficient to fit
  only a scalar where the measurement uncertainty is parametrized by
  \code{scalar * meas.sd}. In this case set \code{opt.meas.sd} to
  \dQuote{scalar}. If all of \code{meas.sd} should be fitted choose
  \dQuote{all}. If the measurement uncertainty is known set
  \code{meas.sd} to \dQuote{none}. See also the package vignette.

}

% <---------------------------------------------------------------------->
\value{

  An object of class \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}}.

}

% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}

% <---------------------------------------------------------------------->
\seealso{

  \code{\link[=schwartz2f.fit-class]{schwartz2f.fit}} class,
  \code{\link{pricefutures}}.

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
\keyword{models}
\keyword{iteration}
\keyword{optimize}
\keyword{distribution}
