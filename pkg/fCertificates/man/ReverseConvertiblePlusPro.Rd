\name{ReverseConvertiblePlusPro}
\alias{ReverseConvertiblePlusPro}

\title{ Reverse Convertible Plus Pro Certificate valuation using pricing by duplication}

\description{
values a Reverse Convertible Plus Pro Certificate using pricing by duplication
}

\usage{
ReverseConvertiblePlusPro(S, Cap, B, Time, r, r_d, sigma, nominal, 
  coupon, barrierHit=FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{Cap}{ the cap, a numeric value. }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{nominal}{ nominal value, e.g. 1000 EUR }
  \item{coupon}{ annualized coupon rate, e.g. 0.14 means 14\% coupon p.a. }
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime. Defaults to FALSE. }
}

\details{
Also known as:
\itemize{
 \item Barrier Reverse Convertibles
}
   
Classification according to the SVSP Swiss Derivative Map 2008: Barrier Reverse Convertibles (340)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Barrier Reverse Convertibles (1230)
}

\value{
the price (scalar or vector) of the ReverseConvertible
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com} }
\seealso{ similar structures: \code{\link{ReverseConvertible}} }

\examples{
##
ReverseConvertiblePlusPro(S=40, Cap=50, B=35, Time=1, r=0.045, r_d=0, 
  sigma=0.4, nominal=1000, coupon=0.14)
  
## payoff diagram
S <- seq(0, 100)
p <- ReverseConvertiblePlusPro(S, Cap=50, B=35, Time=1, r=0.045, r_d=0, 
  sigma=0.4, nominal=1000, coupon=0.14)
p2 <- ReverseConvertiblePlusPro(S, Cap=50, B=35, Time=0, r=0.045, r_d=0, 
  sigma=0.4, nominal=1000, coupon=0.14)
plot(S, p,  type="l", col="red", xlab="underlying price", 
  ylab="payoff", main="Barrier Reverse Convertible")
lines(S, p2, col="blue")
abline(v=c(35, 50), lty=2, col="gray80")      
}

\keyword{math}
