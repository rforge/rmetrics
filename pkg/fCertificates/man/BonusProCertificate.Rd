\name{BonusProCertificate}
\alias{BonusProCertificate}
\title{ Bonus Pro Certificate valuation using pricing by duplication}
\description{
  values a Bonus Pro Certificate using pricing by duplication
}
\usage{
  BonusProCertificate(TypeFlag=c("poB1","pdoB2"), S, X, B, Time, time1 = 0, 
   r, r_d, sigma, ratio = 1, barrierHit = FALSE)
}

\arguments{
  \item{TypeFlag}{see details below}
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{time1}{ The start time of barrier monitoring, measured in years.
                Default value = 0 }
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
A Bonus Pro Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a long partial time down-and-out-put with strike price X and barrier B (\code{\link[fExoticOptions]{PTSingleAssetBarrierOption}})
}
  
It just differs from Bonus Certificates in that it has a partial-time-end barrier. 
Partial-time-end barrier options have the monitoring period start at an arbitrary date before expiration and end at expiration. 
For example the barrier is just monitored during the last 3 months prior to maturity. Ceteris paribus, this means a reduced risk of knock-out.

There are two types of "B" options: 
"B1" is defined such that only a barrier hit or crossed causes the option to be knocked out, and a 
"B2" is defined such that a down-and-out-put is knocked out as soon as the underlying price is below the barrier.

TypeFlag = "poB1":\cr 
The barrier of the down-and-out-put is only monitored in [time1, Time] with 0 <= time1 <= Time (partial-time monitoring) 
instead of [0, Time]. Ceteris paribus, this means a reduced risk of knock-out.
For time1 = 0 (full-time monitoring), the value of a type "poB1" Bonus Pro equals the value of a standard Bonus certificate.
For time1 = Time (no barrier to be monitored), the value of the type "poB1" Bonus Pro duplicates a Protective Put strategy (except for the dividend payments).

TypeFlag = "pdoB2":\cr
The down-and-out-put is knocked out as soon as the underlying price is below the barrier.
  
Classification according to the SVSP Swiss Derivative Map 2008: Bonus Certificates (220)\cr 
Classification according to the SVSP Swiss Derivative Map 2010: Bonus Certificates (1320)
}

\value{
the price (scalar or vector) of the BonusPro Certificate
}

\references{ 
SVSP Swiss Derivative Map 2008 \url{http://www.svsp-verband.ch/}
  
Heynen and Kat (1994). Partial barrier options.
\emph{The Journal of Financial Engineering}, \bold{3}, 253--274.
  
Haug (2007). The complete Guide to Option Pricing Formulas, \emph{Wiley & Sons}, 2nd edition, pp.160  
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ \code{\link{BonusCertificate}}, \code{\link[fExoticOptions]{PTSingleAssetBarrierOption}} }

\examples{
## payoff diagram
S <- seq(50, 130)
p1 <- numeric(length(S))
p2 <- numeric(length(S))
for (i in seq(along=S))
{
  p1[i] <- BonusProCertificate(TypeFlag="pdoB2", S=S[i], X=100, B=70, 
     Time=0.5, time1 = 0.25, 
     r=0.01, r_d=0, sigma=0.3, ratio = 1)
  p2[i] <- BonusProCertificate(TypeFlag="pdoB2", S=S[i], X=100, B=70, 
     Time=0, time1 = 0, 
     r=0.01, r_d=0, sigma=0.3, ratio = 1)
}   
plot(S, p1, ylim=range(p1, p2, na.rm=TRUE), type="l", lwd=2, col="red",
  xlab="underlying price", ylab="payoff", main="Bonus Pro Certificate")
lines(S, p2, lwd=2, col="blue")
abline(v=c(70, 100), lty=2, col="gray80")
    
## example: BonusPro vs. Bonus Certificate
S <- seq(50, 130)
p1 <- numeric(length(S))
p2 <- numeric(length(S))
for (i in seq(along=S))
{
  p1[i] <- BonusProCertificate(TypeFlag="pdoB2", S=S[i], X=100, B=70, 
    Time=1, time1 = 0.75, 
    r=0.01, r_d=0, sigma=0.3, ratio = 1)
  p2[i] <- BonusProCertificate(TypeFlag="pdoB2", S=S[i], X=100, B=70, 
    Time=1, time1 = 0.25, 
    r=0.01, r_d=0, sigma=0.3, ratio = 1)
}
p3 <- BonusCertificate(S=S, X=100, B=70, Time=1, r=0.01, r_d=0, sigma=0.3, ratio = 1)
  
plot(S, S, ylim=range(S, p1, p2, p3), type="l", lwd=2, 
  xlab="underlying price", ylab="payoff", main="Bonus Pro Certificate vs. Bonus Certificate")
lines(S, p1, lwd=2, col="red")
lines(S, p2, lwd=2, col="blue")
lines(S, p3, lwd=2, col="darkgreen")
abline(v=c(70,100), lty=2, col="gray")
}

\keyword{math}
