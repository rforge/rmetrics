\name{DiscountCertificate}
\alias{DiscountCertificate}
\alias{CoveredCall}
\title{ Discount Certificate valuation using pricing by duplication using pricing by duplication }

\description{
  This function values a Discount Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
DiscountCertificate(S, X, Time, r, r_d, sigma, ratio = 1)
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
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
            e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
}

\details{
A Discount Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a short call with strike price X (= cap)
}
  
Also known as:
\itemize{
\item Covered Call
\item Discountzertifikat Classic
}
 
Classification according to the SVSP Swiss Derivative Map 2008: Discount Certificates (310)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Discount Certificates (1200)
} 

\value{
the price (scalar or vector) of the DiscountCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\seealso{ \code{\link{DiscountPlusCertificate}} for a similar structure and \code{\link[fOptions]{GBSOption}}}

\examples{
##
DiscountCertificate(S=40, X=42, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)

## payoff diagram
S <- seq(0, 100)
p <- DiscountCertificate(S, X=42, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)
p2 <- DiscountCertificate(S, X=42, Time=0, r=0.035, r_d=0, sigma=0.3, ratio=1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Discount")
lines(S, p2, col="blue")
abline(v=42, lty=2, col="gray80")
}

\keyword{math}
