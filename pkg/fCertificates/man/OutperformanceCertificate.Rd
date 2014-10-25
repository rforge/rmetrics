\name{OutperformanceCertificate}
\alias{OutperformanceCertificate}

\title{ Outperformance Certificate valuation using pricing by duplication }

\description{
  values a Outperformance Certificate using pricing by duplication
}

\usage{
OutperformanceCertificate(S, X, Time, r, r_d, sigma, participation, ratio = 1)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price (cap), a numeric value. }
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
A Outperformance Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a long call with strike price X
}
The long call permits a outperformance above strike level X.
 
Classification according to the SVSP Swiss Derivative Map 2008: Outperformance Certificates (230)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Outperformance Certificates (1310)
}

\value{
the price (scalar or vector) of the OutperformanceCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}
\author{ Stefan Wilhelm \email{wilhelm@financial.com} }

\examples{
##
OutperformanceCertificate(S=50, X=60, Time=1, 
  r=0.03, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
    
## payoff diagram
S <- seq(0,100)
p <- OutperformanceCertificate(S, X=60, Time=1, 
  r=0.03, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
p2 <- OutperformanceCertificate(S, X=60, Time=0, 
  r=0.03, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Outperformance")
lines(S, p2, col="blue")
abline(v=60, lty=2, col="gray80")    
}

\keyword{math}
