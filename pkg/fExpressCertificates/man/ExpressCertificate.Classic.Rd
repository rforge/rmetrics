\name{ExpressCertificate.Classic}
\Rdversion{1.1}
\encoding{latin1}
\alias{ExpressCertificate.Classic}
\title{
 Analytical and numerical pricing of Classic Express Certificates
}
\description{
Pricing of Classic Express Certificates using 
the truncated multivariate normal distribution (early stop probabilities)
and numerical integration of the one-dimensional marginal return distribution 
at maturity
}
\usage{
ExpressCertificate.Classic(S, X, T, K, g = function(S_T) {S_T}, 
  r, r_d, sigma, ratio = 1)
}
\arguments{
  \item{S}{the asset price, a numeric value}
  \item{X}{a vector of early exercise prices ("Bewertungsgrenzen"), , vector of length (n-1)}
  \item{T}{a vector of evaluation times measured in years ("Bewertungstage"), vector of length n}
  \item{K}{vector of fixed early cash rebates in case of early exercise, length (n-1)}
  \item{g}{a payoff function at maturity, by default \code{g(S_T)=S_T}}
  \item{r}{the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa.}
  \item{r_d}{the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa.}
  \item{sigma}{the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa.}
  \item{ratio}{ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset}
}

\details{
The principal feature inherent to all express certificates is the callable feature with pretermined
valuation dates (\eqn{t_1< \ldots < t_n}) prior to final maturity \eqn{t_n}.
 Express certificates are typically called, if the underlying price on the valuation date 
 is above a strike price (call level): \eqn{S(t_i) > X(t_i)}.

The payoff of an express classic certificate at maturity is 
the underlying performance itself. So the payoff function at maturity takes 
the simple form of \eqn{g(S(t_n)) = S(t_n)}.

We compute early redemption probabilities via the truncated multivariate normal
distribution and integrate the one-dimensional marginal distribution for
the expected payoff \eqn{E[g(S(t_n))] = E[S(t_n)]}.
}

\value{
a vector of length n with certificate prices
}

\references{
 Wilhelm, S. (2009). The Pricing of Derivatives when Underlying Paths Are Truncated: The Case of Express Certificates in Germany.
 Available at SSRN: \url{http://ssrn.com/abstract=1409322}
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\seealso{
\code{\link{MonteCarlo.ExpressCertificate.Classic}} and 
\code{\link{MonteCarlo.ExpressCertificate}} for Monte Carlo evaluation with similar
payoff functions
}

\examples{
ExpressCertificate.Classic(S=100, X=c(100), 
  T=c(1, 2), g = function(S) { S }, 
  K=142.5, r=0.01, r_d=0, sigma=0.3, ratio = 1)

ExpressCertificate.Classic(S=100, X=c(100), 
  T=c(1, 2), g = function(S) { max(S, 151) }, 
  K=142.5, r=0.01, r_d=0, sigma=0.3, ratio = 1)    
}

\keyword{ math }
