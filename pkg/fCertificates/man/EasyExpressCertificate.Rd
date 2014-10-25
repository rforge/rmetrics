\name{EasyExpressCertificate}
\alias{EasyExpressCertificate}

\title{ Easy Express Certificate valuation using pricing by duplication }

\description{
  values a Easy Express Certificate using pricing by duplication
}

\usage{
 EasyExpressCertificate(S, S0, B, Time, r, r_d, sigma, ratio = 1)
}

\arguments{
  \item{S}{ the asset price, a numeric value. }
  \item{S0}{ the fix amount payed at maturity if underlying is above B }
  \item{B}{ the barrier ("Sicherheitslevel"), a numeric value. }
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
There are more than one duplication of an Easy Express Certificate. 
One is a combination of
\enumerate{ 
\item A zero bond/cash component that pays S0 at maturity 
\item A short cash-or-nothing put (\code{\link[fExoticOptions]{CashOrNothingOption}}) with strike B and cash rebate S0-B  
\item A short plain vanilla put with strike price B
}
Also known as:
\itemize{
\item Zanonia-Easy-Zertifikat
}

Classification according to the SVSP Swiss Derivative Map 2008: Express Certificates (360)\cr
Classification according to the SVSP Swiss Derivative Map 2010: Express Certificates (1260)  
}

\value{
the price (scalar or vector) of the EasyExpressCertificate
}

\references{ 
SVSP Swiss Derivative Map \url{http://www.svsp-verband.ch/map/}  
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\seealso{
\code{\link[fExoticOptions]{CashOrNothingOption}} in fExoticOptions package
}

\examples{
##
EasyExpressCertificate(S=80, S0=100, B=70, Time=1, 
  r=0.045, r_d=0, sigma=0.4, ratio = 1)
  
## payoff diagramm
S <- seq(0, 140)
p <- EasyExpressCertificate(S, S0=100, B=70, Time=1, 
  r=0.045, r_d=0, sigma=0.4, ratio = 1)
p2 <- EasyExpressCertificate(S, S0=100, B=70, Time=0, 
  r=0.045, r_d=0, sigma=0.4, ratio = 1)
plot(S, p,  type="l", col="red", , ylim=range(p, p2, na.rm=TRUE), 
  xlab="underlying price", ylab="payoff", main="Easy Express")
lines(S, p2, col="blue")
abline(v=70, lty=2, col="gray80")  
}

\keyword{math}
