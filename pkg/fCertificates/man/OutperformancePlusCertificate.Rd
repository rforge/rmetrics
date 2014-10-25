\name{OutperformancePlusCertificate}
\alias{OutperformancePlusCertificate}

\title{ Outperformance Plus Certificate valuation using pricing by duplication }

\description{
  values a Outperformance Plus Certificate using pricing by duplication
}

\usage{
  OutperformancePlusCertificate(S, X, B, Time, r, 
  r_d, sigma, participation, ratio = 1, barrierHit=FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price, a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
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
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime. 
                     Defaults to FALSE. }              
}

\details{
A Outperformance Plus Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a number of long calls ("participation") with strike price X
\item a (partial time) down-and-out-put with strike price X and barrier level B
}
The long call permits a outperformance above strike level X. 
The down-and-out-put offers partial protection.
  
Also known as:
\itemize{
\item Outperformance Bonus Certificate
}
  
Classification according to the SVSP Swiss Derivative Map 2008: Outperformance Bonus Certificates (235)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Outperformance Bonus Certificates (1330)
}

\value{
the price (scalar or vector) of the OutperformancePlusCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\seealso{ similar structures: \code{\link{OutperformanceCertificate}} }

\examples{
##
OutperformancePlusCertificate(S=10, X=12, B=7, Time=1, 
  r=0.045, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
 
## payoff diagram
S <- seq(30, 100, by=0.1)
p <- OutperformancePlusCertificate(S, X=60, B=40, Time=1, 
  r=0.045, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
p2 <- OutperformancePlusCertificate(S, X=60, B=40, Time=0, 
  r=0.045, r_d=0, sigma=0.4, participation=1.2, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Outperformance Bonus")
lines(S, p2, col="blue")
abline(v=c(40, 60), lty=2, col="gray80")    
}

\keyword{math}