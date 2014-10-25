\name{LeveragedBonusCertificate}
\alias{LeveragedBonusCertificate}

\title{ Leveraged Bonus Certificate valuation using pricing by duplication }
\description{
  values a Leveraged Bonus Certificate using pricing by duplication
}
\usage{
  LeveragedBonusCertificate(S, X, B, B2, Time, r, r_d, 
    sigma, ratio = 1, barrierHit = FALSE)
}
\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{B2}{ knock-out level for the long position (B2 < B) }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime. 
                     Default is FALSE }
}

\details{
A Leveraged Bonus Certificate is a combination of
\enumerate{ 
\item a long leveraged position in the stock (aka Turbo Call) 
\item a long down-and-out-put with strike price X and barrier B (\code{\link[fExoticOptions]{StandardBarrierOption}})
}
  
In contrast to normal Bonus Certificates, a Leveraged Bonus Certificates have a second barrier B2 which marks the knock-out level
for the long position (turbo call). They are cheaper than conventional Bonus Certificates because of the inherent barrier risk, but
allow for higher performances.
   
Classification according to the SVSP Swiss Derivative Map 2008: Outperformance Bonus Certificates (235)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Outperformance Bonus Certificates (1330)
}

\value{
the price (scalar or vector) of the Leveraged Bonus Certificate
}

\references{ 
SVSP Swiss Derivative Map 2008 \url{http://www.svsp-verband.ch/} 
}
\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\seealso{
\code{\link[fExoticOptions]{StandardBarrierOption}} in fExoticOptions package.
}

\examples{
##
LeveragedBonusCertificate(S=100, X=120, B=80, B2=70, Time=1, r=0.01, r_d=0, 
  sigma=0.3, ratio=1, barrierHit=FALSE)

## payoff diagram
S <- seq(0, 140)
p <- LeveragedBonusCertificate(S, X=120, B=80, B2=70, Time=1, r=0.01, r_d=0, 
  sigma=0.3, ratio=1, barrierHit=FALSE)
p2 <- LeveragedBonusCertificate(S, X=120, B=80, B2=70, Time=0, r=0.01, r_d=0, 
  sigma=0.3, ratio=1, barrierHit=FALSE)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Leveraged Bonus")
lines(S, p2, col="blue")
abline(v=c(70, 80, 120), lty=2, col="gray80")          
}

\keyword{math}
