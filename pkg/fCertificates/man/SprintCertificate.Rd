\name{SprintCertificate}
\alias{SprintCertificate}

\title{ Sprint Certificate valuation using pricing by duplication }

\description{
  This function values a Sprint Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
SprintCertificate(S, X, Cap, Time, r, r_d, sigma, participation, ratio = 1)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price, a numeric value. }
  \item{Cap}{ the cap, a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{participation}{ participation rate/factor above strike level. Defaults to 1. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
}

\details{
A Sprint Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item 2 long calls with strike price X
\item a short call with strike price Cap
}
The long calls permit an outperformance (double participation) in the range between strike level X and Cap at maturity.
It is somehow a capped outperformance certificate.
  
Alternative names of this structure are:
\enumerate{ 
\item Kick-start-Certificates
\item Double-Chance-Certificate
\item Capped Outperformance Certificate
}
  
Classification according to the SVSP Swiss Derivative Map 2008: Capped Outperformance Certificates (350)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Capped Outperformance Certificates (1240)
}

\value{
the price (scalar or vector) of the SprintCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ similar structures: \code{\link{OutperformanceCertificate}}}

\examples{
##
SprintCertificate(S=32, X=30, Cap=40, Time=1, r=0.045, 
  r_d=0, sigma=0.4, participation=2, ratio = 1)

## payoff diagramm
S <- seq(0, 100)
p <- SprintCertificate(S, X=30, Cap=40, Time=1, r=0.045, 
  r_d=0, sigma=0.4, participation=2, ratio = 1)  
p2 <- SprintCertificate(S, X=30, Cap=40, Time=0, r=0.045, 
  r_d=0, sigma=0.4, participation=2, ratio = 1)  
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Sprint")
lines(S, p2, col="blue")
abline(v=c(30, 40), lty=2, col="gray80")
}

\keyword{math}