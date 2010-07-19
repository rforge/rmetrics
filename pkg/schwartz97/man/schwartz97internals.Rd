\name{schwartz97-internal}
\alias{.clean.rda.data}
\alias{.get.data}
\alias{.mu.state.schwartz2f}
\alias{.sigma.state.schwartz2f}
\alias{.A.schwartz2f}
\alias{.B.schwartz2f}
\alias{.mu.fut.schwartz2f}
\alias{.sigma.fut.schwartz2f}
\alias{.sim.futures}
\title{Internal Schwartz97 Functions}

% <---------------------------------------------------------------------->
\description{

  Internal \dQuote{Schwartz97} functions. These functions are
  not to be called by the user.

}
\usage{
.clean.rda.data(tmp.list, idx = 1:6)

.get.data(data, type = c("uv", "mv"))

.mu.state.schwartz2f(x0, delta0, mu, sigmaS, kappa,
                     alpha, sigmaE, rho, time, as.mat = FALSE)

.sigma.state.schwartz2f(sigmaS, kappa, sigmaE, rho, time)

.A.schwartz2f(kappa, sigmaS, sigmaE, rho, alphaT, r, ttm)

.B.schwartz2f(kappa, ttm)

.mu.fut.schwartz2f(x0, delta0, mu, sigmaS, kappa, sigmaE, rho,
                   alpha, alphaT, r, time, ttm, measure = "P")

.sigma.fut.schwartz2f(sigmaS, kappa, sigmaE, rho, time, ttm)

.sigma.opt.schwartz2f(time, Time, kappa, sigmaS, sigmaE, rho)

.sim.futures(time, dt, ttm = NA, obj = schwartz2f(), r = 0.03, lambda = 0, sd = 0.01)
}
% <---------------------------------------------------------------------->
\details{

  \code{.clean.rda.data} \cr Removes NAs from the internal futures data
  sets. This is needed in order to fit parameters to the data.

  \code{.get.data} \cr Check whether \code{data} is of a particular
  format and return a clean version of \code{data}.

  \code{.mu.state.schwartz2f} \cr Computes the mean vector of the
  jointly normally distributed state variables of the Schwartz
  two-factor model. The state variables are the spot log-price and the
  spot convenience yield.

  \code{.sigma.state.schwartz2f} \cr Computes the covariance matrix
  of the jointly normally distributed state variables of the Schwartz
  two-factor model. The state variables are the spot log-price and the
  spot convenience yield.

  \code{.A.schwartz2f} \cr Computes the deterministic component
  \code{A(t,T)} of the affine futures term-structure.

  \code{.B.schwartz2f} \cr Computes the deterministic component
  \code{B(t,T)} of the affine futures term-structure.

  \code{.mu.fut.schwartz2f} \cr Computes the parameter \code{mu}
  of the futures price log-normal distribution.

  \code{.sigma.fut.schwartz2f} \cr Computes the parameter
  \code{sigma} of the futures price log-normal distribution.

  \code{.sigma.opt.schwartz2f} \cr Computes the \code{sigma} for
  the options formula.

  \code{.sim.futures} \cr Simulate futures prices and overlay with
  noise. This function is used to test \code{\link{fit.schwartz2f}}.

}
% <---------------------------------------------------------------------->
\author{Philipp Erb, David Luethi, Juri Hinz}
% <---------------------------------------------------------------------->
\keyword{internal}
