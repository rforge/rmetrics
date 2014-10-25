\name{CappedWarrant}
\alias{DiscountCall}
\alias{DiscountPut}

\title{ Capped Warrant (Discount Call/Discount Put) valuation using pricing by duplication }

\description{
This function values a Capped Warrant (Discount Call/Discount Put) Certificate using pricing by duplication and the Generalized Black/Scholes formula.
}

\usage{
DiscountCall(S, X, Cap, Time, r, r_d, sigma, ratio = 1)
DiscountPut(S, X, Cap, Time, r, r_d, sigma, ratio = 1)
}

\arguments{
\item{S}{ the asset price, a numeric value. }
\item{X}{ the exercise price, a numeric value. }
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
}

\details{
A discount call is a combination of
\enumerate{ 
\item a long call with strike X 
\item a short call with strike Cap
}
Because of the short call component, the discount call is cheaper than a normal call which allows higher returns. On the other hand,
the payoff is capped.
  
A discount put is a combination of
\enumerate{ 
\item a long put with strike Cap 
\item a short put with strike X
}
Because of the short put, the discount put is cheaper than a normal put which allows higher returns. On the other hand,
the payoff is capped.
  
Also known as:
\itemize{
\item Capped Warrant
\item Spread Warrant
}
  
Classification according to the SVSP Swiss Derivative Map 2008: Spread Warrants (115)  
}

\value{
the price (scalar or vector) of the Discount Call(Discount Put)
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{\code{\link[fOptions]{GBSOption}}, \code{\link{DiscountPut}}}

\examples{
##
DiscountCall(S=10, X=10, Cap=12, Time=1, r=0.045, r_d=0, sigma=0.2, ratio = 1)

## payoff diagram
S <- seq(0,20, by=0.1)
p <- DiscountCall(S, X=10, Cap=12, Time=1, r=0.045, r_d=0, sigma=0.2, ratio = 1)
p2 <- DiscountCall(S, X=10, Cap=12, Time=0, r=0.045, r_d=0, sigma=0.2, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Discount Call")
lines(S, p2, col="blue")
abline(v=c(10, 12), lty=2, col="gray80") 
}

\keyword{math}
