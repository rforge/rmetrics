\name{SimulateExpressCertificate}
\Rdversion{1.1}
\encoding{latin1}
\alias{SimulateGenericExpressCertificate}
\alias{SimulateExpressClassicCertificate}
\alias{SimulateExpressBonusCertificate}
\alias{simExpressPriceMVN}
\alias{simExpressPriceTMVN}

\title{
Monte Carlo Valuation of Express Certificates
}
\description{
Generic Monte Carlo Valuation of Express Certificates using the Euler scheme, 
multivariate normal distribution and truncated multivariate normal.
}
\usage{
SimulateGenericExpressCertificate(S, X, K, T, r, r_d, sigma, mc.loops = 10000, 
  mc.steps = 1000, payoffFunction = payoffExpressClassic, ...)
SimulateExpressClassicCertificate(S, X, K, T, r, r_d, sigma, mc.loops = 10000, 
  mc.steps = 1000)
SimulateExpressBonusCertificate(S, X, B, K, T, r, r_d, sigma, mc.loops = 10000, 
  mc.steps = 1000, barrierHit = FALSE)
  
simExpressPriceMVN(S, m = Inf, X, K, B, T, r, r_d, sigma, 
  mc.loops = 100000, payoffFunction, ...)
simExpressPriceTMVN(S, m = Inf, X, K, B, T, r, r_d, sigma, 
  mc.loops = 100000, payoffFunction, ...)  
}

\arguments{
  \item{S}{the asset price, a numeric value}
  \item{X}{a vector of early exercise prices/call levels ("Bewertungsgrenzen"), vector of length \code{(n-1)}}
  \item{B}{barrier level}
  \item{K}{vector of fixed early cash rebates in case of early exercise, length \code{(n-1)} or \code{n} in case of a fixed rebate at maturity}
  \item{T}{a vector of evaluation times measured in years ("Bewertungstage"), vector of length \code{n}}
  \item{r}{the annualized rate of interest, a numeric value; 
            e.g. 0.05 means 5\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{mc.loops}{Monte Carlo Loops (iterations)}
  \item{mc.steps}{Monte Carlo steps in one path}
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime }
  \item{payoffFunction}{definition of a payoff function, see details below}
  \item{m}{The minimum price up to today for pricing during the lifetime.}
  \item{\dots}{Additional parameters passed to the payoff function}
}

\details{
TO BE DONE: Definition of payoff functions
}

\value{
The methods return an object of class "\code{express.certificate}". 

An object of class "\code{express.certificate}" is a list containing at least the following components:
\item{price}{Monte Carlo estimate}
\item{prices}{A vector of simulated discounted prices (length \code{mc.loops})}
\item{n}{The number of valuation dates}
\item{redemptionTimes}{A vector of redemption times i = 1..n (length \code{mc.loops})}
\item{S}{the asset price, a numeric value}
\item{X}{early exercise prices/call levels}
\item{K}{vector of fixed early cash rebates in case of early exercise}
\item{T}{a vector of evaluation times measured in years ("Bewertungstage")}

There is also a method \code{\link{print.express.certificate}} for pretty printing of \code{express.certificate} objects.
}

\author{Stefan Wilhelm \email{wilhelm@financial.com}}

\seealso{
Definition of several payoff functions in 
\code{\link{payoffExpressClassic}}, 
\code{\link{payoffExpressCappedBonusType1}}
or \code{\link{payoffExpressBonusType1}}

\code{\link{print.express.certificate}} for pretty printing of \code{express.certificate} objects
}

\examples{
\dontrun{
# Example CB7AXR on Deutsche Telekom on 10.12.2009
p <- SimulateExpressBonusCertificate(S=10.4/12.10*100, X=c(100,100,100), B=7/12.1*100, 
    K=c(134, 142.5, 151), 
	T=.RLZ(c("16.12.2009","17.06.2010","17.12.2010"), start="10.12.2009"), r=0.01, r_d=0, 
	sigma=0.23, mc.loops=10000, mc.steps=1000)
p
}
}
