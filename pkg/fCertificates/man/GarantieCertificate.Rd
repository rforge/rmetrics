\name{GarantieCertificate}
\alias{GarantieCertificate}
\alias{GuaranteeCertificate}

\title{ Guarantee Certificate valuation using pricing by duplication }

\description{
  values a Guarantee Certificate using pricing by duplication
}

\usage{
GarantieCertificate(S, X, Time, r, r_d, sigma, 
  participation, ratio = 1, nominal)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price ("Bonuslevel"), a numeric value. }
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
  \item{nominal}{ nominal value, e.g. 1000 EUR that is guaranteed; can be the same as S the price of one share }
}
\details{
A Guarantee Certificate is a combination of
\enumerate{ 
\item a zero bond with nominal value "nominal"
\item a number of long calls ("participation", default 1) with strike price X
}
The long calls allow for upside participation while the zero bond ensures the nominal value at maturity (hence a guarantee).
  
Also known as:
\itemize{
\item Capital Protected Certificate
\item Structured Note
}

Classification according to the SVSP Swiss Derivative Map 2008: Uncapped Capital Protection (410)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Uncapped Capital Protection (1100)
}

\value{
the price (scalar or vector) of the GarantieCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\examples{
##
GarantieCertificate(S=110, X=90, Time=1, r=0.045, r_d=0, 
  sigma=0.4, participation=0.8, ratio = 1, nominal=110)

## payoff diagram
S <- seq(0, 150)
p <- GarantieCertificate(S, X=90, Time=1, r=0.045, r_d=0, 
  sigma=0.4, participation=0.8, ratio = 1, nominal=110)
p2 <- GarantieCertificate(S, X=90, Time=0, r=0.045, r_d=0, 
  sigma=0.4, participation=0.8, ratio = 1, nominal=110)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", 
  main="Uncapped Capital Protection")
lines(S, p2, col="blue")
abline(v=90, lty=2, col="gray80")    
}

\keyword{math}
