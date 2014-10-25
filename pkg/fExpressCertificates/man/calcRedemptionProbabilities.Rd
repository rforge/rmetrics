\name{Express Certificates Redemption Probabilities}
\Rdversion{1.1}
\alias{calcRedemptionProbabilities}
\alias{calcStopProbabilities}
\alias{simRedemptionProbabilities}
\alias{MonteCarloStopProbabilities}
\title{ Redemption Probabilities for Express Certificates }
\description{
  Calculates the stop probabilities/early redemption probabilities 
  for express certificates
  using the multivariate normal distribution or determines stop probabilities
  with Monte Carlo simulation.
}
\usage{
calcRedemptionProbabilities(S, X, T, r, r_d, sigma)
simRedemptionProbabilities(S, X, T, r, r_d, sigma, mc.steps=1000, mc.loops=20)
}

\arguments{
  \item{S}{ the asset price, a numeric value }
  \item{X}{ a vector of early exercise prices ("Bewertungsgrenzen"), vector of length \eqn{(n-1)} }
  \item{T}{ a numeric vector of evaluation times measured in years ("Bewertungstage"): \eqn{T = (t_1,...,t_n)'}, vector of length n }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{mc.steps}{ Monte Carlo steps in one path }
  \item{mc.loops}{ Monte Carlo loops (iterations) }                
}
\details{
  Calculates the stop probabilities/early redemption probabilities for Express Certificates
  at valuation dates \eqn{(t_1,...,t_n)'} using the multivariate normal distribution 
  of log returns of a Geometric Brownian Motion. 
  The redemption probability \eqn{p(t_i)} at \eqn{t_i < t_n} is
  \deqn{p(t_i) = P(S(t_i) \ge X(t_i), \forall_{j < i}{S(t_j) < X(t_j)})}{P(S[i] >= X[i], S[j] < X[j] for all j < i)} i.e.
  \deqn{p(t_i) = P(S(t_i) \ge X(t_i), S(t_1) \le X(t_1),\ldots,S(t_{i-1}) \le X(t_{i-1}))}{%
        p(t_i) = P(S[i] >= X[i], S[1] <= X[1],...,S[i-1] <= X[i-1])} 
  for \eqn{i=1,\ldots,(n-1)}{i=1,...,(n-1)} and 
  \deqn{p(t_n) = P(S(t_1) \le X(t_1),\ldots,S(t_{n-1}) \le X(t_{n-1}))}{%
        p(t_n) = P(S[1]<=X[1],...,S[n-1]<=X[n-1])} for \eqn{i=n}. 
}
\value{
  a vector of length \code{n} with the redemption probabilities at valuation
  dates \eqn{(t_1,...,t_n)'}.
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\references{
 Wilhelm, S. (2009). The Pricing of Derivatives when Underlying Paths Are Truncated: The Case of Express Certificates in Germany.
 Available at SSRN: \url{http://ssrn.com/abstract=1409322}
}

\examples{
# Monte Carlo simulation of redemption probabilities 
# p(t_i) = P(S(t_i)>=X(t_i),\forall_{j<i} S(t_j)<X(t_j))
mc.loops <- 5000
probs <- simRedemptionProbabilities(S=100, X=c(100,100,100), T=c(1,2,3,4), 
  r=0.045, r_d=0, sigma=0.3, mc.steps=3000, mc.loops=5000)
table(probs$stops)/mc.loops

# Analytic calculation of redemption probabilities
probs2 <- calcRedemptionProbabilities(S=100, X=c(100,100,100), T=c(1,2,3,4), 
  r=0.045, r_d=0, sigma=0.3)
probs2
}
\keyword{ math }