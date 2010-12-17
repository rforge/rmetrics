\name{Warrant}
\alias{Warrant}

\title{ Warrant valuation using pricing by duplication }

\description{
convenience method for standard Warrant pricing
}

\usage{
Warrant(type, S, X, Time, r, r_d, sigma, ratio = 1)
}

\arguments{
  \item{type}{ "call" or "put" }
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
This is simply a convenience wrapper function for the \code{\link[fOptions]{GBSOption}} method
which can also scale with a \code{ratio}.
  
Classification according to the SVSP Swiss Derivative Map 2008: Warrants (110)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Warrants (2100)
}

\value{
the price (scalar or vector) of the Warrant
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/} 
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}
\seealso{ \code{\link[fOptions]{GBSOption}} in fOptions package}

\examples{
##
Warrant("c", S=40, X=42, Time=1, r=0.035, r_d=0, sigma=0.3, ratio=0.1)
}

\keyword{math}
