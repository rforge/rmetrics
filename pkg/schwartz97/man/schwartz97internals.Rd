\name{schwartz97-internal}
\alias{.clean.rda.data}
\alias{.mu.state.schwartz2factor}
\alias{.sigma.state.schwartz2factor}
\alias{.A.schwartz2factor}
\alias{.B.schwartz2factor}
\alias{.mu.fut.schwartz2factor}
\alias{.sigma.fut.schwartz2factor}
\title{Internal Schwartz97 Functions}

% <---------------------------------------------------------------------->
\description{

  Internal \dQuote{Schwartz97} functions. These functions are
  not to be called by the user.

}
\usage{
.clean.rda.data(tmp.list, idx = 1:6)

.mu.state.schwartz2factor(x0, delta0, mu, sigmaS, kappa,
                          alpha, sigmaE, rho, time, as.mat = FALSE)

.sigma.state.schwartz2factor(sigmaS, kappa, sigmaE, rho, time)

.A.schwartz2factor(kappa, sigmaS, sigmaE, rho, alphaT, r, ttm)

.B.schwartz2factor(kappa, ttm)

.mu.fut.schwartz2factor(x0, delta0, mu, sigmaS, kappa, sigmaE, rho,
                        alpha, alphaT, r, time, ttm)

.sigma.fut.schwartz2factor(sigmaS, kappa, sigmaE, rho, time, ttm)

.sigma.opt.schwartz2factor(time = 0.5, Time = 1, kappa = 1,
                           sigmaS = 0.3, sigmaE = 0.5, rho = 0.75)

}
% <---------------------------------------------------------------------->
\details{

  \code{.clean.rda.data} \cr Removes NAs from the internal futures data
  sets. This is needed in order to fit parameters to the data.

  \code{.mu.state.schwartz2factor} \cr Computes the mean vector of the
  jointly normally distributed state variables of the Schwartz
  two-factor model. The state variables are the spot log-price and the
  spot convenience yield.

  \code{.sigma.state.schwartz2factor} \cr Computes the covariance matrix
  of the jointly normally distributed state variables of the Schwartz
  two-factor model. The state variables are the spot log-price and the
  spot convenience yield.

  \code{.A.schwartz2factor} \cr Computes the deterministic component
  \code{A(t,T)} of the affine futures term-structure.

  \code{.B.schwartz2factor} \cr Computes the deterministic component
  \code{B(t,T)} of the affine futures term-structure.

  \code{.mu.fut.schwartz2factor} \cr Computes the parameter \code{mu}
  of the futures price log-normal distribution.

  \code{.sigma.fut.schwartz2factor} \cr Computes the parameter
  \code{sigma} of the futures price log-normal distribution.

  \code{.sigma.opt.schwartz2factor} \cr Computes the \code{sigma} for
  the options formula.

}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}
% <---------------------------------------------------------------------->
\keyword{internal}
