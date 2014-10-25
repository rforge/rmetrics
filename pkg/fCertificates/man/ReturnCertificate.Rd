\name{ReturnCertificate}
\alias{ReturnCertificate}

\title{ Return Certificate valuation using pricing by duplication }

\description{
This function values a Return certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
ReturnCertificate(S, Bonus, S0, B, Cap, Time, r, r_d, sigma, ratio = 1, 
  barrierHit=FALSE)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{Bonus}{ the bonus payment/cash rebate in EUR  }
  \item{S0}{ underlying start price }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
  \item{Cap}{ the cap, a numeric value. }
  \item{Time}{ time to maturity measured in years }
  \item{r}{ the annualized rate of interest, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{r_d}{ the annualized dividend yield, a numeric value; 
            e.g. 0.25 means 25\% pa. }
  \item{sigma}{ the annualized volatility of the underlying security, 
                a numeric value; e.g. 0.3 means 30\% volatility pa. }
  \item{ratio}{ ratio, number of underlyings one certificate refers to, a numeric value;
                e.g. 0.25 means 4 certificates refer to 1 share of the underlying asset }
  \item{barrierHit}{ flag whether the barrier has already been reached/hit during the lifetime }              
}

\details{
A Return Certificate is similar to a Bonus Certificate in the way it offers an extra payment ("bonus") 
under certain conditions. However, while with Bonus Certificates 
this bonus is a fixed amount is payed in the range B and X, Return certificates pays a bonus on top of the underlying price.
  
\enumerate{
\item a long position in the stock (aka Zero-Strike Call) 
\item a long binary down-and-out-cash-or-nothing-put with strike price X and barrier B and cash rebate Bonus (\code{\link[fExoticOptions]{BinaryBarrierOption}})
\item a short call with strike equal to Cap
}  
}

\value{
the price (scalar or vector) of the ReturnCertificate
}

\references{ see packages fOptions and \code{\link[fExoticOptions]{BinaryBarrierOption}} in package fExoticOptions }

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ See also \code{\link[fOptions]{GBSOption}} in package fOptions, \code{\link[fExoticOptions]{BinaryBarrierOption}} 
in package fExoticOptions, \code{\link{BonusCertificate}}}

\examples{
##
ReturnCertificate (S=100, S0=91.7, Bonus=11, B=45, Cap=91.7, 
  Time=0, r=0.02, r_d = 0, sigma=0.3, ratio = 1)

## payoff diagram
S <- seq(30,120, by=1)
p <- ReturnCertificate (S, S0=91.7, Bonus=11, B=45, Cap=91.7, 
  Time=0.5, r=0.02, r_d = 0, sigma=0.3, ratio = 1)
p2 <- ReturnCertificate (S, S0=91.7, Bonus=11, B=45, Cap=91.7, 
  Time=0, r=0.02, r_d = 0, sigma=0.3, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Return Certificate")
lines(S, p2, col="blue")
abline(v=c(45,91.7), lty=2, col="gray80")
}

\keyword{math}