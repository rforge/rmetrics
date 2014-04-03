\name{Distribution of the Brownian Bridge Minimum}
\Rdversion{1.1}
\encoding{latin1}
\alias{rBrownianBridgeMinimum}
\alias{dBrownianBridgeMinimum}
\title{ Distribution of the Minimum of a Brownian Bridge }
\description{
  Density function and random generation of the minimum \eqn{m_T=\min_{t_0 \le t \le T}} 
  of a Brownian Bridge \eqn{B_t} between time \eqn{t_0} and \eqn{T}.
}

\usage{
 rBrownianBridgeMinimum(n = 100, t0 = 0, T = 1, a = 0, b = 0, sigma = 1)
 dBrownianBridgeMinimum(x, t0 = 0, T = 1, a = 0, b = 0, sigma = 1)
}

\arguments{
  \item{n}{ the number of samples to draw }
  \item{x}{ a vector of minimum values to calculate the density for }
  \item{t0}{ start time }
  \item{T}{ end time }
  \item{a}{ start value of the Brownian Bridge (B(t0)=a) }
  \item{b}{ end value of the Brownian Bridge (B(T)=b) }
  \item{sigma}{ volatility p.a., e.g. 0.2 for 20\% }
}

\details{
  \code{rBrownianBridgeMinimum()} simulates the minimum \eqn{m(T)} for a Brownian Bridge \eqn{B(t)}
  between \eqn{t_0 \le t \le T}{t0 <= t <= T}, i.e. a Brownian Motion \eqn{W(t)} constraint to \eqn{W(t_0)=a} and \eqn{W(T)=b}.\cr
  The simulation algorithm uses the conditional density \eqn{f(m(T) = x | B(t_0)=a, B(T)=b)}
  and is based on the exponential distribution given by Beskos et al. (2006), pp.1082--1083, 
  which we generalized to the \eqn{\sigma^2 \ne 1}{sigma^2<>1} case.
 
  The joint density function \eqn{m(T)} and \eqn{W(T)} is (see \cite{Beskos2006}, pp.1082--1083 
  and \cite{Karatzas2008}, p.95):
  \deqn{
    f_{m(T),W(T)}(b,a) = \frac{2 \cdot (a-2b)}{\sqrt{2\pi} \sigma^3 \sqrt{T^3}} \cdot \exp{\left\{ -\frac{(a-2b)^2}{2\sigma^2 T} \right\}}
  }

  With the density of \eqn{W(T)}
  \deqn{
    f_{W(T)}(a)      = \frac{1}{\sqrt{2\pi} \sigma \sqrt{T}} \cdot \exp{\left\{ -\frac{a^2}{2\sigma^2 T} \right\}}
  }
  it follows for the conditional density of the minimum \eqn{m(T) | W(T)=a}
  \deqn{
    f_{m(T)|W(T)=a}(b) = \frac{2 \cdot (a-2b)}{\sigma^2 T} \cdot \exp{\left\{ -\frac{(a-2b)^2}{2\sigma^2 T} + \frac{a^2}{2\sigma^2 T} \right\}} 
  }
}

\value{
  \code{simBrownianBridgeMinimum()} returns a vector of simulated minimum values of length \code{n}.
  
  \code{densityBrownianBridgeMinimum} returns a vector of length \code{length(x)} with density values 
}
\references{
  Beskos, A.; Papaspiliopoulos, O. and Roberts, G. O. (2006).
    Retrospective Exact Simulation of Diffusion Sample Paths with Applications 
    \emph{Bernoulli}, \bold{12}, 1077--1098
   
  Karatzas/Shreve (2008). Brownian Motion and Stochastic Calculus, \emph{Springer}, p.95
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\examples{
  # simulate 1000 samples from minimum distribution
  m <- rBrownianBridgeMinimum(n = 1000, t0 = 0, T = 1, a = 0.2, b = 0, sigma = 2)
    
  # and compare against the density
  x  <- seq(-6, 0, by=0.01)
  dm <- dBrownianBridgeMinimum(x, t0 = 0, T = 1, a = 0.2, b = 0, sigma = 2)
  
  plot(density(m))
  lines(x, dm, lty=2, col="red")
}
\keyword{ math }