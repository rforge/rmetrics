\name{CappedBonusCertificate}
\alias{CappedBonusCertificate}

\title{Capped Bonus Certificate valuation using pricing by duplication }

\description{
  This function values a Capped Bonus Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
CappedBonusCertificate(S, X, B, Cap, Time, r, r_d, sigma, ratio = 1, 
  barrierHit=FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Cap}{ the cap, a numeric value. }
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
A Capped Bonus Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a long down-and-out-put with strike price X and barrier B (StandardBarrierOption)
\item a short call with strike price equal to cap
}
The payoff of similar to the one of BonusCertificate, albeit capped.

Classification according to the SVSP Swiss Derivative Map 2008: Capped Bonus Certificates (380)\cr 
Classification according to the SVSP Swiss Derivative Map 2010: Capped Bonus Certificates (1250)
}

\value{
the price (scalar or vector) of the CappedBonusCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ \code{\link{BonusCertificate}}, \code{\link{CappedReverseBonusCertificate}} for similar structures}

\examples{
##
CappedBonusCertificate(S=50, X=60 , B=35, Cap=75, Time=2, sigma=0.14, 
  r=0.02, r_d=0, ratio=1)
   
## payoff diagram
S <- seq(0,120)
p <- CappedBonusCertificate(S, X=60 , B=35, Cap=75, Time=2, sigma=0.14, 
  r=0.02, r_d=0, ratio=1)
p2 <- CappedBonusCertificate(S, X=60 , B=35, Cap=75, Time=0, sigma=0.14, 
  r=0.02, r_d=0, ratio=1)
plot(S, p,  type="l", col="red", ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Capped Bonus")
lines(S, p2, col="blue")
abline(v=c(35, 60, 75), lty=2, col="gray80") 
}

\keyword{math}