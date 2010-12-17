\name{DiscountPlusCertificate}
\alias{DiscountPlusCertificate}

\title{ DiscountPlus Certificate valuation using pricing by duplication }

\description{
  This function values a DiscountPlus certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
DiscountPlusCertificate(S, X, B, Time, r, r_d, sigma, ratio = 1, 
  barrierActive = TRUE,
  barrierHit = FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price (cap), a numeric value. }
  \item{B}{ the barrier level, a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
            e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
  \item{barrierActive}{ flag telling whether barrier is currently active (TRUE) or inactive (FALSE). Default value is TRUE. }
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime }
}

\details{
A Discount Plus Certificate is a combination of
\enumerate{ 
\item a long position in the stock (aka Zero-Strike Call) 
\item a short call with strike price X (= cap)
\item a long (partial-time) down-and-out-put
}
also known as:
\itemize{
\item Barrier Discount Certificates
}

Classification according to the SVSP Swiss Derivative Map 2008: Barrier Discount Certificates (320)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Barrier Discount Certificates (1210)
}

\value{
the price (scalar or vector) of the DiscountPlusCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{
\code{\link{DiscountCertificate}} for a similar structure and \code{\link[fOptions]{GBSOption}}
}

\examples{
##
DiscountPlusCertificate(S=42, X=42, B=30, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)

## payoff diagram
S <- seq(0, 100)
p <- DiscountPlusCertificate(S, X=42, B=30, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=1)
p2 <- DiscountPlusCertificate(S, X=42, B=30, Time=0, r=0.035, r_d=0, sigma=0.3, ratio=1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Barrier Discount")
lines(S, p2, col="blue")
abline(v=c(30, 42), lty=2, col="gray80")
}

\keyword{math}