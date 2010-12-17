\name{Strangle}
\alias{Strangle}

\title{ Strangle valuation}
\description{
  valuation of a long strangle strategy (one long call + one long put with different strike prices) using pricing by duplication
}
\usage{
Strangle(S, X1, X2, Time, r, r_d, sigma, ratio = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{S}{ the asset price, a numeric value or vector. }
  \item{X1}{ the exercise price of the long put, a numeric value. }
  \item{X2}{ the exercise price of the long call, a numeric value. }
  \item{Time}{ time to maturity measured in years. }
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
\item a long put with strike price X1 and 
\item a long call with strike price X2 (X1 < X2).
}  
When X1 = X2 the strategy becomes a straddle. 
}
\value{
the price of the Strangle
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{\code{\link[fOptions]{GBSOption}}, \code{\link{Straddle}}}

\examples{
##
Strangle(S=50, X1=40, X2=60, Time=1, r=0.01, r_d=0, sigma=0.3, ratio=1)

## payoff diagram
S <- 1:100
ps1 <- Strangle(S, X1=45, X2=55, Time=1, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps2 <- Strangle(S, X1=45, X2=55, Time=0, r=0.01, r_d=0, sigma=0.3, ratio=1)
ps3 <- Strangle(S, X1=45, X2=55, Time=1, r=0.01, r_d=0, sigma=0.4, ratio=1)

plot(S, ps2, type="l", col="red", xlab="underlying price", 
  ylab="payoff", main="Strangle")
lines(S, ps1, col="blue")
lines(S, ps3, col="green")
abline(v=c(45, 55), lty=2, col="gray80")
}
\keyword{ math }