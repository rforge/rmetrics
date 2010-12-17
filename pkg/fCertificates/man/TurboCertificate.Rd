\name{TurboCertificate}
\alias{TurboCertificate}

\title{ Turbo Certificate valuation }

\description{
This function values a TurboCertificate 
using barrier option formulas.
"Call Turbos/Turbo Bulls" are effectively long down-and-out calls and "Put Turbos/Turbo Bears" are effectively up-and-out-puts.
}

\usage{
TurboCertificate(type, S, X, B, Time, r, r_d, sigma, ratio = 1)
}

\arguments{
  \item{type}{ type flag, either "c" for long and "p" for short  }
  \item{S}{ the asset price, a numeric value }
  \item{X}{ the exercise price (strike), a numeric value. }
  \item{B}{ the barrier (knock-out-level), a numeric value. }
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
This is simply a convenience wrapper function for the \code{StandardBarrierOption} method
which can also scale with a \code{ratio}.
  
Also known as:
\itemize{
 \item Knock-out Warrant
 \item Turbo Bull/Turbo Bear
 \item Turbo Long/Turbo Short
 \item Up-and-Out-Call/Down-and-Out-Put
 \item Barrier Option
}
  
Classification according to the SVSP Swiss Derivative Map 2008: Knock-out Warrants (120)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Knock-out Warrants (2200)
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\value{
the price of the TurboCertificate
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ \code{\link[fExoticOptions]{StandardBarrierOption}}}

\examples{
##
TurboCertificate("c", S=40, X=42, B=38, Time=1, r=0.035, r_d=0, 
  sigma=0.3, ratio=1)

## payoff
S <- seq(0, 100)
p <- TurboCertificate("c", S, X=42, B=38, Time=1, r=0.035, r_d=0, 
  sigma=0.3, ratio=1)
p2 <- TurboCertificate("c", S, X=42, B=38, Time=0, r=0.035, r_d=0, 
  sigma=0.3, ratio=1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Knock-out Warrant")
lines(S, p2, col="blue")
abline(v=c(38, 42), lty=2, col="gray80")
}

\keyword{math}