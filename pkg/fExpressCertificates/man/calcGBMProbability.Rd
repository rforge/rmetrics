\name{calcGBMProbability}
\Rdversion{1.1}
\encoding{latin1}
\alias{calculateProbabilityGeometricBrownianMotion}
\alias{calcGBMProbability}
\title{ Calculates probabilities for the Geometric Brownian Motion }
\description{
  This method is a compilation of formulas for some (joint) probabilities for the 
  Geometric Brownian Motion \eqn{S_t=S(t)} with drift parameter \eqn{\mu} and volatility \eqn{\sigma}
  and its minimum \eqn{m_t=m(t)=\min_{0 \le \tau \le t}{S(\tau)}} 
  and its maximum \eqn{M_t=M(t)=\max_{0 \le \tau \le t}{S(\tau)}}.
}
\usage{
calculateProbabilityGeometricBrownianMotion(
  Type = 
  c("P(S_t <= X)",
    "P(S_t >= X)",
    "P(S_t >= X, m_t >= B)",
    "P(M_t <= B)",
    "P(M_t >= B)",
    "P(m_t <= B)", 
    "P(m_t >= B)"), S0 = 100, X, B, t = 1, mu = 0, sigma = 1)
}
\arguments{
  \item{Type}{ Type of probability to be calculated, see details. }
  \item{S0}{ Start price }
  \item{X}{ strike level }
  \item{B}{ barrier level }
  \item{t}{ time }
  \item{mu}{ drift term }
  \item{sigma}{ volatility in \% p.a. }
}
\details{
  Let \eqn{M_t = \max(S_t)} and \eqn{m_t = \min(S_t)} 
  for \eqn{t > 0} be the running maximum/minimum of the Geometric Brownian Motion \eqn{S}{S} up to time \eqn{t}{t} 
  respectively.
  
  \itemize{
  \item{\eqn{P(S_t \le X)}{\code{P(S_t <= X)}}}{ is the probability of the process being below \eqn{X}{\code{X}} at time \eqn{t}{\code{t}}.\cr 
    Possible Application: shortfall risk of a plain-vanilla call option at maturity}
  \item{\eqn{P(M_t \ge B)}{\code{P(M_t >= B)}}}{ is the probability of the maximum exceeding a barrier level \eqn{B}{\code{B}}.}
  \item{\eqn{P(M_t \le B)}{\code{P(M_t <= B)}}}{ is the probability of the maximum staying below a barrier level \eqn{B}{\code{B}} up to time \eqn{t}{\code{t}}.}
  \item{\eqn{P(m_t \le B)}{\code{P(m_t <= B)}}}{ is the probability of the minimum to fall below a barrier level \eqn{B}{\code{B}}.}
  \item{\eqn{P(m_t \ge B)}{\code{P(m_t >= B)}}}{ is the probability of the minimum to stay above barrier level \eqn{B}{\code{B}}.}
  }
}
\value{
  a vector of probabilities
}
\references{ 
  Poulsen, R. (2004), Exotic Options: Proofs Without Formulas, Working Paper p.7 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\seealso{ \code{\link{calcBMProbability}} for probabilities of the standard Brownian Motion }

\examples{
# Simulate paths for Geometric Brownian Motion and compute barrier probabilities
N=400
S <- matrix(NA,1000,N+1)
for (i in 1:1000) {
  S[i,] <- GBM(S0=100, mu=0.05, sigma=1, T=1, N=N)
}

# a) Maximum M_t
M_t <- apply(S, 1, max, na.rm=TRUE)

S0 <- 100
B  <- seq(100, 1000, by=1)

p1 <- calcGBMProbability(Type="P(M_t <= B)", S0=S0, B=B, t=1, mu=0.05, sigma=1)

# or via arithmetic Brownian Motion and drift mu - sigma^2/2
p2 <- calcBMProbability(Type="P(M_t <= a)", a=log(B/S0), t=1, mu=0.05-1/2, sigma=1)

plot(ecdf(M_t))
lines(B, p1, col="red", lwd=2)
lines(B, p2, col="green")

# b) Minimum m_t
m_t <- apply(S, 1, min, na.rm=TRUE)

B  <- seq(0, 100, by=1)
p3 <- calcGBMProbability(Type="P(m_t <= B)", S0=S0, B=B, t=1, mu=0.05, sigma=1)
p4 <- calcBMProbability(Type="P(m_t <= a)", a=log(B/S0), t=1, mu=0.05-1/2, sigma=1)

plot(ecdf(m_t))
lines(B, p3, col="red", lwd=2)
lines(B, p4, col="green", lty=2)
}

\keyword{ math }
