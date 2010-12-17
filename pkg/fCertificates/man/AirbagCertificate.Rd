\name{AirbagCertificate}
\alias{AirbagCertificate}

\title{ Airbag Certificate valuation using pricing by duplication }

\description{
  This function values a Airbag Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
AirbagCertificate(S, X, B, Time, r, r_d, sigma, participation, ratio = 1)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price ("Partizipationslevel"), a numeric value. }
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
}
\details{
An Airbag Certificate is a combination of
  \enumerate{ 
  \item a fixed component X 
  \item a long call with strike price X
  \item X/B short puts with strike price equal to B.
  }
  
Classification according to the SVSP Swiss Derivative Map 2008: Airbag Certificates (240)
}

\value{
  the price (scalar or vector) of the AirbagCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\examples{
##
AirbagCertificate(S=100, X=100, B=75, Time=0, r=0.045, r_d=0, sigma=0.2, 
  participation=1, ratio=1)

## payoff diagram
S <- seq(0,120)
p <- AirbagCertificate(S, X=100, B=75, Time=1, r=0.045, r_d=0, sigma=0.2, 
  participation=1, ratio=1)
p2 <- AirbagCertificate(S, X=100, B=75, Time=0, r=0.045, r_d=0, sigma=0.2, 
  participation=1, ratio=1)
plot(S, p,  type="l", col="red", xlab="underlying price", 
  ylab="payoff", main="Airbag")
lines(S, p2, col="blue")
abline(v=c(75, 100), lty=2, col="gray80")
}

\keyword{math}
