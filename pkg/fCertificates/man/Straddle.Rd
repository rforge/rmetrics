\name{Straddle}
\alias{Straddle}

\title{ Straddle valuation}

\description{
  valuation of a long Straddle strategy (one long call + one long put with same strike price) using pricing by duplication
}
\usage{
Straddle(S, X, Time, r, r_d, sigma, ratio = 1)
}
\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{X}{ the exercise price, a numeric value. }
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
A strangle is a combination of
\enumerate{ 
\item a long put 
\item a long call
}
with the same strike price X. If the strike prices of the 2 options differ (i.e. X1 < X2), 
then the strategy is called a long strangle. 
}

\value{
the price of the Straddle, either scalar or vector
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{\code{\link[fOptions]{GBSOption}}, \code{\link{Strangle}}}

\examples{
S <- seq(0, 100)
prices <- Straddle(S, X=50, Time=0, r=0.05, r_d=0, sigma=0.2, ratio = 1)
plot(S, prices, type="l", xlab="underlying price", ylab="payoff") 
 
## Straddle payoff diagram
S <- seq(0, 100)
ps1 <- Straddle(S, X=45, Time=1, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps2 <- Straddle(S, X=45, Time=0, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps3 <- Straddle(S, X=45, Time=1, r=0.01, r_d=0, sigma=0.4, ratio=1)

plot(S, ps2, type="l", col="red", xlab="underlying price", 
  ylab="payoff", main="Straddle")
lines(S, ps1, col="blue")
lines(S, ps3, col="green")
abline(v=45, lty=2, col="gray80")
}

\keyword{math}