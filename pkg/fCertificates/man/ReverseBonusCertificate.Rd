\name{ReverseBonusCertificate}
\alias{ReverseBonusCertificate}

\title{ Reverse Bonus Certificate valuation using pricing by duplication }

\description{
  values a Reverse Bonus Certificate using pricing by duplication
}

\usage{
ReverseBonusCertificate(S, S0, X, B, Time, r, r_d, sigma, ratio=1, 
  barrierHit=FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{S0}{ the underlying start price at issue date }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime }              
}

\details{
A Reverse Bonus Certificate is a combination of
\enumerate{ 
\item a short position in the stock with reference price 2*S0 (aka long put with Strike 2*S0)
\item a up-and-out-call with strike X and barrier B (Cash rebate K = 0 for standard barrier options)
}
}

\value{
the price (scalar or vector) of the ReverseBonusCertificate
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\examples{
##
ReverseBonusCertificate(S=110, S0=120, X=100, B=140, Time=1, r=0.045, 
  r_d=0, sigma=0.4, ratio=1)

## payoff diagram
S <- seq(0, 150)
p <- ReverseBonusCertificate(S, S0=120, X=100, B=140, Time=0.1, r=0.045, 
  r_d=0, sigma=0.4, ratio=1)
p2 <- ReverseBonusCertificate(S, S0=120, X=100, B=140, Time=0, r=0.045, 
  r_d=0, sigma=0.4, ratio=1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Reverse Bonus")
lines(S, p2, col="blue")
abline(v=c(100, 140), lty=2, col="gray80")
}

\keyword{math}