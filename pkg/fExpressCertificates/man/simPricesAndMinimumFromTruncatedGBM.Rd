\name{simPricesAndMinimumFromTruncatedGBM}
\alias{simPricesAndMinimumFromTruncatedGBM}
\Rdversion{1.1}
\encoding{latin1}

\title{
Simulation of the joint finite-dimensional distribution of a restricted Geometric Brownian Motion
and its minimum
}
\description{
Simulates from the joint distribution of finite-dimensional distributions
\eqn{(S(t_1),...,S(t_n))} and the minimum \eqn{m(t_n)} 
of a restricted Geometric Brownian motion by using the
truncated multivariate normal distribution of the returns and the conditional distribution of
a minimum of a Brownian Bridge given the returns.
}
\usage{
simPricesAndMinimumFromTruncatedGBM(N = 100, S, T, mu, sigma, 
  lowerX = rep(0, length(T)), 
  upperX = rep(+Inf, length(T)), 
  log = FALSE, m=Inf)
}

\arguments{
  \item{N}{number of samples to draw}
  \item{S}{start value of the Arithmetic/Geometric Brownian Motion, i.e. \eqn{S(0)=S_0} or \eqn{B(0) = S_0}}
  \item{T}{Numeric vector of n valuation times \eqn{T = (t_1,...,t_n)'}}
  \item{mu}{the drift parameter of the Geometric Brownian Motion}
  \item{sigma}{volatility p.a., e.g. 0.2 for 20\%}
  \item{lowerX}{Numeric vector of n lower bounds for the Geometric Brownian Motion, zeros are permitted, default is \code{rep(0,length(T))}}
  \item{upperX}{Numeric vector of n upper bounds for the Geometric Brownian Motion, \code{+Inf} are permitted, default is \code{rep(+Inf,length(T))}}
  \item{log}{logical, if true the returns instead of prices are returned}
  \item{m}{Possible prior minimum value.}
}
\details{
For the \eqn{n} valuation times given by \eqn{T = (t_1,\dots,t_n)'} we simulate 
from the joint distribution \eqn{(S(t_1),\ldots,S(t_n),m(t_1),\ldots,m(t_n))} 
of the finite-dimensional distribution
\eqn{(S(t_1),\ldots,S(t_n))} and the running minimum \eqn{m(t_i) = \min_{0 \le t \le t_i}(S_t)} 
of a restricted/truncated Geometric Brownian motion.

The Geometric Brownian Motion is conditioned at the \eqn{n} valuation dates \eqn{(t_1,...,t_n)} 
on \eqn{lowerX_i \le S(t_i) \le upperX_i} for all \eqn{i=1,\dots,n}. 

First we simulate \eqn{(S(t_1),\ldots,S(t_n))} 
from a truncated multivariate normal distribution of the returns
with mean vector  
\deqn{(\mu - \sigma^2/2) * T} 
and covariance matrix 
\deqn{\Sigma = (\min{(t_i,t_j)}\sigma^2) \\
             = \left[ \begin{array}{cccc}
         \min{(t_1,t_1)} \sigma^2 & \min{(t_1,t_2)} \sigma^2 & \cdots & \min{(t_1,t_n)} \sigma^2 \\
         \min{(t_2,t_1)} \sigma^2 & \min{(t_2,t_2)} \sigma^2 & \cdots & \min{(t_2,t_n)} \sigma^2 \\
         \vdots & & & \\
         \min{(t_n,t_1)} \sigma^2 & \cdots &  & \min{(t_n,t_1)} \sigma^2
      \end{array} \right]
}{\code{Sigma = outer(T, T, pmin) * sigma^2}}
and lower and upper truncation points \code{lower=log(lowerX/S)} and \code{upper=log(upperX/S)} respectively.

Given the realized prices \eqn{(S(t_1),\ldots,S(t_n))}
we simulate the global minimum as the minimum of several Brownian Bridges as described in Beskos (2006):

We simulate the period minimum \eqn{m_{(i-1,i)}} between two times \eqn{t_{i-1}} and \eqn{t_i} 
for all \eqn{i=1,\dots,n}.
This minimum \eqn{m_{(i-1,i)} | S(t_{i-1}),S(t_i)} is the minimum of a Brownian Bridge between \eqn{t_{i-1}} and \eqn{t_i}.

The global minimum is the minimum of all period minima given by \cr 
 \eqn{m_n = \min(m_{(0,1)},m_{(1,2)},\dots,m_{(n-1,n)}) = \min(m_{(i-1,i)})} 
 for all \eqn{i=1,\dots,n}.
}

\value{
A \eqn{(N \times 2*n)} matrix with N rows and columns \eqn{(S(t_1),\ldots,S(t_n),m(t_1),\ldots,m(t_n))}
}

\author{Stefan Wilhelm \email{wilhelm@financial.com}}

\note{
This function can be used to determine the barrier risk of express certificates
at maturity, i.e. the probability that barrier \eqn{B} has been breached given that
we reach maturity: \eqn{P(m(t_n) \le B | \forall_{i < n} S(t_i) < X(t_i))}
}

\seealso{
See the similar method \code{\link{simPricesAndMinimumFromGBM}} 
for the unrestricted Geometric Brownian Motion (i.e. \code{lowerX=rep(0,n)} 
and \code{upperX=rep(Inf,n)}).
}

\examples{
# 1. Simulation of restricted GBM prices and minimums m_t
# finite-dimensional distribution and Brownian Bridge
X1 <- simPricesAndMinimumFromTruncatedGBM(N=5000, S=100, T=c(1,2,3), 
  upperX=c(100,100,Inf), mu=0.05, sigma=0.3)
m1 <- X1[,4]
  
# 2. Compare to distribution of unrestricted GBM minimums
X2 <- simPricesAndMinimumFromGBM(N=5000, S=100, T=c(1,2,3), 
  mu=0.05, sigma=0.3)
m2 <- X2[,4]
	
plot(density(m1, to=100), col="black", main="Minimum m_t for Express Certificate 
  price paths at maturity")
lines(density(m2, to=100), col="blue")
legend("topleft", legend=c("Restricted GBM minimum","Unrestricted GBM minimum"),
  col=c("black","blue"), lty=1, bty="n")
}

\keyword{ math }
\keyword{ multivariate }
