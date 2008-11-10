\name{BLCOP data sets}        
\alias{monthlyReturns}

\docType{data}

\title{Monthly equity returns }

\description{A matrix holding time series of monthly returns (calculated from closing prices) for six stocks. The returns span the period from Jaunary 1998 through December 2003.}
\usage{monthlyReturns}
\format{
  A matrix with 6 columns and 71 rows.  The names of the rows hold the dates of each series entry, and the column names are the names of the six equities from
  which the return series are taken.
}

\examples{
CAPMList(monthlyReturns, marketIndex = sp500Returns, riskFree = US13wTB)
}
\keyword{datasets}
