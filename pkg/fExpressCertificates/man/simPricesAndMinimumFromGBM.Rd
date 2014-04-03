\name{simPricesAndMinimumFromGBM}
\Rdversion{1.1}
\encoding{latin1}
\alias{simPricesAndMinimumFromGBM}
\alias{simPricesAndMinimumFromGBM2}

\title{
Simulation of the joint finite-dimensional distribution of the Geometric Brownian Motion
and its minimum
}
\description{
Simulates from the joint distribution of finite-dimensional distribution
\eqn{(S(t_1),\ldots,S(t_n))} and the minimum \eqn{m(t_n)} of a Geometric Brownian motion 
by either using simple grid approach or 
using the multivariate normal distribution of the returns and the conditional distribution of
a minimum of a Brownian Bridge given the returns.
}
\usage{
simPricesAndMinimumFromGBM(N = 100, S, T, mu, sigma, log = FALSE, m=Inf)

simPricesAndMinimumFromGBM2(N = 10000, S, T, mu, sigma, mc.steps = 1000)
}

\arguments{
  \item{N}{number of samples to draw}
  \item{S}{start value of the Arithmetic/Geometric Brownian Motion, i.e. S(0)=S0 or B(0) = S0}
  \item{T}{ Numeric vector of valuation times (length n). \eqn{T = (t_1,...,t_n)'}}
  \item{mu}{the drift parameter of the Geometric Brownian Motion}
  \item{sigma}{volatility p.a., e.g. 0.2 for 20\%}
  \item{log}{logical, if true the returns instead of prices are returned}
  \item{m}{Possible prior minimum value.}
  \item{mc.steps}{Number of gridpoints}
}

\details{

grid-approach \cr
The \code{simPricesAndMinimumFromGBM2} method 
uses the Monte Carlo Euler Scheme, the stepsize is \eqn{\delta t = t_n/mc.steps}.
The method is quite slow.


multivariate-normal distribution approach \cr

The method \code{simPricesAndMinimumFromGBM} draws from the multivariate normal
distribution of returns. For the \eqn{n} valuation times given by \eqn{T = (t_1,\ldots,t_n)'} we simulate 
from the joint distribution \eqn{(S(t_1),\dots,S(t_n),m(t_1),\ldots,m(t_n))} 
of the finite-dimensional distribution
\eqn{(S(t_1),...,S(t_n))} and the running minimum \eqn{m(t_i) = \min_{0 \le t \le t_i}(S(t))} 
of a Geometric Brownian motion.
This is done by using the
multivariate normal distribution of the returns of a GBM and the conditional distribution of
a minimum of a Brownian Bridge (i.e. in-between valuation dates).

First we simulate \eqn{(S(t_1),\dots,S(t_n))} from a multivariate normal distribution of the returns
with mean vector  
\deqn{(\mu - \sigma^2/2) T} 
and covariance matrix 
\deqn{(\Sigma)_{ij} = \min{(t_i, t_j)} * \sigma^2}{%
       \code{Sigma = outer(T, T, pmin) * sigma^2}}
 
Next, we simulate the period minimum \eqn{m(t_{i-1},t_i) = \min_{t_{i-1} \le t \le t_i}S(t)} 
between two times \eqn{t_{i-1}} and \eqn{t_i} for all \eqn{i=1,\dots,n}.
This minimum \eqn{m(t_{i-1},t_i) | S(t_{i-1}), S(t_i)} is the minimum of a Brownian Bridge between \eqn{t_{i-1}} and \eqn{t_i}.

The global minimum is the minimum of all period minima given by \cr
\eqn{m(t_n) = \min(m_{(0,1)},m_{(1,2)},\dots,
m_{(n-1,n)}) = \min m(t_{i-1},t_i)} for all \eqn{i=1,\dots,n}.
}

\value{
A matrix \eqn{(N \times 2n)}{(N x 2*n)} with rows \eqn{(S(t_1),\ldots,S(t_n), m(t_1),\ldots,m(t_n))}
}

\note{
Since we are considering a specific path for the prices and are interested in the minimum
given the specific trajectory (i.e. \eqn{m(t_n) | S(t_1),\dots,S(t_n)}), 
it is not sufficient to sample from the bivariate density \eqn{(S(t_n), m(t_n))}, 
for which formulae is given by Karatzas/Shreve and others. 
Otherwise we could face the problem that some of the \eqn{S(t_1),...,S(t_{n-1})} 
are smaller than the simulated \eqn{m(t_n)}.
However, both approaches yield the same marginal density for \eqn{m(t_n)}.
}

\references{
Beskos, A.; Papaspiliopoulos, O. and Roberts, G. O. (2006).
 Retrospective Exact Simulation of Diffusion Sample Paths with Applications 
 \emph{Bernoulli}, \bold{12}, 1077--1098

Karatzas/Shreve (2008). Brownian Motion and Stochastic Calculus, \emph{Springer}
}

\author{Stefan Wilhelm \email{wilhelm@financial.com}}

\seealso{
The method \code{\link{simPricesAndMinimumFromGBM2}} 
returns the same, but using the Euler Scheme.

See also \code{\link{calcGBMProbability}} 
for the CDF of the minimum m_t (i.e. \code{Type="P(m_t <= B)"})
}
\examples{
# Comparison of sampling of GBM Minimum m_t via finite dimensional approach +
# Brownian Bridges vs. crude Monte Carlo

# naive grid-based approach
X0 <- simPricesAndMinimumFromGBM2(N=5000, S=100, T=c(1,2,3), mu = 0.05, sigma=0.3, 
  mc.steps=1000)

# Simulation of minimums m_t via prices at valuation dates 
# (S(t_1),S(t_2),...,S(t_n)) and Brownian Bridges in-between	
X1 <- simPricesAndMinimumFromGBM(N=5000, S=100, T=c(1,2,3), mu=0.05, sigma=0.3)
m1 <- X1[,4]
  
# Monte Carlo simulation of m_t via gridpoints (m2)
mc.loops <- 5000
mc.steps <- 2000
S <- matrix(NA, mc.loops, mc.steps + 1)
for (j in 1:mc.loops) {
 S[j,] <- GBM(S0=100, mu=0.05, sigma=0.3, T=3, N=mc.steps)
}	
m2 <- apply(S, 1, min) # minimum for each price path
  
# Compare probability density function and CDF for m_t against each other
# and against theoretical CDF.
par(mfrow=c(2,2))
# a) pdf of GBM minimum m_t at maturity for both approaches
plot(density(m1, to=100), col="black")
lines(density(m2, to=100), col="blue")
  
# b) compare empirical CDFs for m_t with theoretical probability P(m_t <= B)
B  <- seq(0, 100, by=1)
p3 <- calcGBMProbability(Type="P(m_t <= B)", 
  S0=100, B=B, t=3, mu=0.05, sigma=0.3)
  
plot(ecdf(m1), col="black", main="Sampling of GBM minimum m_t")
lines(ecdf(m2), col="blue")
lines(B, p3, col="red")
legend("topleft", legend=c("Finite-dimensions and Brownian Bridge", 
   "MC Euler scheme", "Theoretical value"), 
   col=c("black","blue","red"), lwd=2) 
}
\keyword{ multivariate }
