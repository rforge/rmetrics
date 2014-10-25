\name{calcBMProbability}
\Rdversion{1.1}
\encoding{latin1}
\alias{calculateProbabilityBrownianMotion}
\alias{calcBrownianMotionProbability}
\alias{calcBMProbability}

\title{Calculates probabilities for the Arithmetic Brownian Motion }
\description{
  This method is a compilation of formulas for some (joint) probabilities 
  for the Arithmetic Brownian Motion \eqn{B_t=B(t)} with drift parameter \eqn{\mu} and volatility \eqn{\sigma}
  and its minimum \eqn{m_t=m(t)} or maximum \eqn{M_t=M(t)}.
}

\usage{
 calcBMProbability(
  Type = c(
    "P(M_t >= a)",
    "P(M_t <= a)",
    "P(m_t <= a)",
    "P(m_t >= a)",
    "P(M_t >= a, B_t <= z)",
    "P(m_t <= a, B_t >= z)",
    "P(a <= m_t, M_t <= b)",
    "P(M_s >= a, B_t <= z | s < t)",
    "P(m_s <= a, B_t >= z | s < t)",
    "P(M_s >= a, B_t <= z | s > t)",
    "P(m_s <= a, B_t >= z | s > t)"), 
  a, z=0, t = 1, mu = 0, sigma = 1, s = 0)
}

\arguments{
  \item{Type}{ Type of probability to be calculated, see details. }
  \item{a}{ level }
  \item{z}{ level }
  \item{t}{ point in time, \eqn{t > 0} }
  \item{mu}{ Brownian Motion drift term \eqn{\mu}}
  \item{sigma}{ Brownian Motion volatility \eqn{\sigma}}
  \item{s}{ Second point in time, used by some probabilities like \code{P(M_s >= a, B_t <= z | s < t)} } 
}

\details{
  Let \eqn{      M_t = \max(B_t)}{
           \code{M_t = max(B_t)}} and 
      \eqn{      m_t = \min(B_t)}{
           \code{m_t = min(B_t)}} 
  for \eqn{t > 0} be the running maximum/minimum of the Brownian Motion up to time \eqn{t}{\code{t}} respectively.
  
  \itemize{
  \item{\eqn{P(M_t \ge a)}{P(M_t >= a)} (\eqn{P(M_t \le a)}{P(M_t <= a)}) }
    {is the probability of the maximum \eqn{M_t} exceeding (staying below) a level  \eqn{a}{\code{a}} up to time \eqn{t}{\code{t}}. 
    See Chuang (1996), equation (2.3).}
    
  \item{ \eqn{P(m_t \le a)}{P(m_t <= a)} (\eqn{P(m_t \ge a)}{P(m_t >= a)}) }
    {is the probability of the minimum \eqn{m_t} to fall below (rise above) a level \eqn{a}{\code{a}} up to time \eqn{t}{\code{t}}.}
    
  \item{\eqn{P(M_t \ge a, B_t \le z)}{P(M_t >= a, B_t <= z)} }{
    is the joint probability of the maximum, exceeding level \eqn{a}{\code{a}}, 
    while the Brownian Motion is below level \eqn{z}{\code{z}} at time \eqn{t}{\code{t}}. 
    See Chuang (1996), equation (2.1), p.82.}
    
  \item{\eqn{P(m_t \le a, B_t \ge z)}{P(m_t <= a, B_t >= z)} }{
    is the joint probability of the minimum to be below level \eqn{a}{\code{a}}, while the Brownian Motion is above level \code{z} at time \code{t}.}
    
  \item{\eqn{P(M_s \ge a, B_t \le z | s < t)}{P(M_s >= a, B_t <= z | s < t)} }{
    See Chuang (1996), equation (2.7), p.84 for the joint probability \eqn{(M_s,B_t)} of the maximum \eqn{M_s} and the Brownian Motion \eqn{B_t} at different times \eqn{s < t}}
  
  \item{\eqn{P(m_s \le a, B_t \ge z | s < t)}{P(m_s <= a, B_t >= z | s < t)} }{
    See Chuang (1996), equation (2.7), p.84 for the joint probability of \eqn{(M_s,B_t)} \eqn{s < t}. Changed formula to work for the minimum.}
    
  \item{\eqn{P(M_s \ge a, B_t \le z | s > t)}{P(M_s >= a, B_t <= z | s > t)} }{
    See Chuang (1996), equation (2.9), p.85 for the joint probability \eqn{(M_s,B_t)} of the maximum \eqn{M_s} and the Brownian Motion \eqn{B_t} at different times \eqn{s > t}}
    
  \item{\eqn{P(m_s \le a, B_t \ge z | s > t)}{P(m_s <= a, B_t >= z | s > t)} }{
    See Chuang (1996), equation (2.9), p.85 for the joint probability \eqn{(M_s,B_t)} of the maximum \eqn{M_s} and the Brownian Motion \eqn{B_t} at different times \eqn{s > t}.
    Adapted this formula for the minimum \eqn{(m_s,B_t)} by 
     \eqn{P(M_s \ge a, B_t \le z) = P(m_s \le -a, B^{*}_t \ge -z)}{P(M_s >= a, B_t <= z) = P(m_s <= -a, B^{*}_t >= -z)}.}
  }
  
  \bold{Some identities:}\cr
    For \eqn{s < t}:\cr 
    \deqn{P(M_s \le a , M_t \ge a , B_t \le z ) = P(M_t \ge a , B_t \le z) - P(M_s \ge a , B_t \le z)}{%
          P(M_s <= a ,  M_t >= a , B_t <= z )   = P(M_t >= a , B_t <= z) - P(M_s >= a , B_t <= z)}
         
    \deqn{P(M_s \ge a,  B_t \le z) = P(M_s \ge a)- P(M_s \ge a, B_t \ge z)}{%
          P(M_s >= a,  B_t <= z) = P(M_s >= a)- P(M_s >= a, B_t >= z)}
    
    \deqn{P(X \le -x,Y \le -y) = P(-X \ge x, -Y \ge y)  
                               = 1 - P(-X \le x) - P(-Y \le y) + P(-X \le x, - Y \le y)}{%
          P(X <= -x,Y <= -y) = P(-X >= x, -Y >= y) = 1 - P(-X <= x) - P(-Y <= y) + P(-X <= x, - Y <= y)}
    
  \bold{Changing from maximum \eqn{M_t}{M_t} of \eqn{B_t}{B_t} to minimum \eqn{m^{*}_t}{m^*_t} of \eqn{B^{*}_t = -B_t}{B^*_t = -B_t}:}\cr
  \eqn{P(M_t \ge z)}{P(M_t >= z)} becomes \eqn{P(m^{*}_t \le -z)}{P(m^*_t <= -z)}. 
}

\value{
  The method returns a vector of probabilities, if used with vector inputs.
}

\references{
 Chuang (1996). Joint distribution of Brownian motion and its maximum, 
 with a generalization to correlated BM and applications to barrier options
 \emph{Statistics & Probability Letters} \bold{28}, 81--90
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\examples{
################################################################################
#
# Example 1: Maximum M_t of Brownian motion
#
################################################################################

# simulate 1000 discretized paths from Brownian Motion B_t
B <- matrix(NA,1000,101)
for (i in 1:1000) {
  B[i,] <- BrownianMotion(S0=100, mu=0.05, sigma=1, T=1, N=100)
}

# get empirical Maximum M_t
M_t <- apply(B, 1, max, na.rm=TRUE)
plot(density(M_t, from=100))

# empirical CDF of M_t
plot(ecdf(M_t))
a <- seq(100, 103, by=0.1)
# P(M_t <= a)
# 1-cdf.M_t(a-100, t=1, mu=0.05, sigma=1)
p <- calcBMProbability(Type = "P(M_t <= a)", a-100, t = 1, 
    mu = 0.05, sigma = 1)
lines(a, p, col="red")

################################################################################
#
# Example 2: Minimum m_t of Brownian motion
#
################################################################################

# Minimum m_t : Drift ändern von 0.05 auf -0.05
m_t <- apply(B, 1, min, na.rm=TRUE)

a <- seq(97, 100, by=0.1)
# cdf.m_t(a-100, t=1, mu=0.05, sigma=1)
p <- calcBMProbability(Type = "P(m_t <= a)", a-100, t = 1, mu = 0.05, sigma = 1)

plot(ecdf(m_t))
lines(a, p, col="blue")
}

\keyword{ math }

