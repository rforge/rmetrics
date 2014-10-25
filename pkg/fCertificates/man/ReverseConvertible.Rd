\name{ReverseConvertible}
\alias{ReverseConvertible}

\title{ Reverse Convertible Certificate valuation using pricing by duplication }

\description{
values a Reverse Convertible Certificate using pricing by duplication
}

\usage{
ReverseConvertible(S, Cap, Time, r, r_d, sigma, nominal, coupon)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{Cap}{ the cap, a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{nominal}{ nominal value, e.g. 1000 EUR }
  \item{coupon}{ annualized coupon rate, e.g. 0.14 means 14\% coupon p.a. }
}

\details{
Duplication:
\itemize{
  \item coupon bond with nominal value \code{nominal} and coupon \code{coupon}
  \item \code{nominal}/\code{Cap} short puts with strike price equal to \code{Cap}
}
  
Also known as:
\itemize{
  \item Aktienanleihe
} 
Classification according to the SVSP Swiss Derivative Map 2008: Reverse Convertibles (330)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Reverse Convertibles (1220)
}

\value{
the price (scalar or vector) of the Reverse Convertible
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/}  
}
\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\seealso{ similar structures : \code{\link{ReverseConvertiblePlusPro}},\code{\link{DiscountCertificate}} }

\examples{
##
ReverseConvertible(S=40, Cap=50, Time=1, r=0.045, r_d=0, sigma=0.4, 
  nominal=1000, coupon=0.14)

## payoff diagram
S <- seq(0, 100)
p <- ReverseConvertible(S, Cap=50, Time=1, r=0.045, r_d=0, sigma=0.4, 
  nominal=1000, coupon=0.14)
p2 <- ReverseConvertible(S, Cap=50, Time=0, r=0.045, r_d=0, sigma=0.4, 
  nominal=1000, coupon=0.14)
plot(S, p,  type="l", col="red", xlab="underlying price", 
  ylab="payoff", main="Reverse Convertible")
lines(S, p2, col="blue")
abline(v=50, lty=2, col="gray80")
}

\keyword{math}
