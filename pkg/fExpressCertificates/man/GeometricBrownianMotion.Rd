\name{GeometricBrownianMotion}
\alias{GBM}
\alias{GeometricBrownianMotion}
\alias{BM}
\alias{BrownianMotion}
\alias{ArtihmeticBrownianMotion}
\alias{GeometricBrownianMotionMatrix}

\title{ Simulate paths from a Arithmetic or Geometric Brownian Motion }
\description{
Simulate one or more paths for an Arithmetic Brownian Motion \eqn{B(t)} or 
for a Geometric Brownian Motion \eqn{S(t)} for \eqn{0 \le t \le T} 
using grid points (i.e. Euler scheme).
}

\usage{
BM(S0, mu=0, sigma=1, T, N)
GBM(S0, mu, sigma, T, N)
GeometricBrownianMotionMatrix(S0, mu, sigma, T, mc.loops, N)
}
\arguments{
  \item{S0}{ start value of the Arithmetic/Geometric Brownian Motion, i.e. S(0)=S0 or B(0) = S0}
  \item{mu}{ the drift parameter of the Brownian Motion }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{T}{ time }
  \item{mc.loops}{ number of Monte Carlo price paths }
  \item{N}{ number of grid points in price path }
}

\value{
a vector of length \code{N+1} with simulated asset prices at \eqn{(i * T/N), i=0,\ldots,N}{(i * T/N), i=0,...,N}.
}
\references{ 
Iacus, Stefan M. (2008). Simulation and Inference for Stochastic Differential Equations: With R Examples
\emph{Springer}
}
\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\examples{
# Simulate three trajectories of the Geometric Brownian Motion S(t)
T        <- 1
mc.steps <- 100
dt       <- T/mc.steps
t        <- seq(0, T, by=dt)
S_t      <- GBM(S0=100, mu=0.05, sigma=0.3, T=T, N=mc.steps)
plot(t, S_t, type="l", main="Sample paths of the Geometric Brownian Motion")
for (i in 1:2) {
 S_t      <- GBM(S0=100, mu=0.05, sigma=0.3, T=T, N=mc.steps)
 lines(t, S_t, type="l")
}
}

\keyword{ math }
