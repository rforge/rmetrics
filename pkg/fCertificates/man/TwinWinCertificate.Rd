\name{TwinWinCertificate}
\alias{TwinWinCertificate}

\title{ Twin Win Certificate valuation using pricing by duplication }
\description{
  This function values a Twin Win Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
TwinWinCertificate(S, X, B, Time, r, r_d, sigma, 
  participation = 1, ratio = 1)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{participation}{ participation rate/factor between bonus level and strike level. Defaults to 1. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
}

\details{
A Twin Win Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item 2 long down-and-out-puts with strike price X and barrier B (StandardBarrierOption)
}
The structure is similar to a Bonus Certificate, the only difference is a double participation at maturity in the range between
B and X, implying a "Twin Win" situation for slightly falling underlying prices.
  
Classification according to the SVSP Swiss Derivative Map 2008: Twin-Win Certificates (250)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Twin-Win Certificates (1340)
}

\value{
the price (scalar or vector) of the Twin Win Certificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ \code{\link{BonusCertificate}} for a similar structure }

\examples{
##
TwinWinCertificate(S=100, X=100, B=80, Time=2, r = 0.03, r_d = 0, 
  sigma=0.15, participation=1.2, ratio = 1)
 
## payoff diagram
S <- seq(50, 150, by=0.1)
p <- TwinWinCertificate(S, X=100, B=80, Time=0.2, r = 0.03, r_d = 0, 
  sigma=0.15, participation=1.2, ratio = 1)
p2 <- TwinWinCertificate(S, X=100, B=80, Time=0, r = 0.03, r_d = 0, 
  sigma=0.15, participation=1.2, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Twin-Win")
lines(S, p2, col="blue")    
abline(v=c(80, 100), lty=2, col="gray80")  
}

\keyword{math}
