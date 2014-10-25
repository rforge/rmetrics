\name{implyVolatility}
\Rdversion{1.1}
\alias{implyVolatility}
\title{
imply the volatility of the certificate with Newton/Raphson
}
\description{
The function implies the volatility of the certificate with one-dimensional Newton/Raphson method
}
\usage{
implyVolatility(price, f, interval = c(0, 1), sigma = NULL, doPlot=FALSE, ...)
}

\arguments{
  \item{price}{current price of the certificate}
  \item{f}{The pricing function of the certificate, e.g. BonusCertificate}
  \item{interval}{interval to search for implied volatility }
  \item{sigma}{ start value for the volatility }
  \item{doPlot}{flag whether to plot price function for convergence diagnostics. Defaults to FALSE}
  \item{\dots}{ additional parameters passed 
    to the pricing function, e.g. S=100, X=100 etc.
  }
}

\value{
returns the implied volatility if it can be implied. Otherwise NA.
}

\author{ Stefan Wilhelm \email{wilhelm@financial.com}}

\examples{
p <- DiscountCertificate(S=100, X=110, Time=1, r=0.01, r_d=0, sigma=0.5)
implyVolatility(price=p, DiscountCertificate, S=100, X=110, Time=1, r=0.01, r_d=0)
  
p <- DiscountCertificate(S=100, X=110, Time=1, r=0.01, r_d=0, sigma=0.5)
implyVolatility(price=p, DiscountCertificate, doPlot=TRUE, S=100, X=110, Time=1, r=0.01, r_d=0)
}
\keyword{ math }
