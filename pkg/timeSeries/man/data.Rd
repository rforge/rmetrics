\name{TimeSeriesData}

\alias{TimeSeriesData}

\alias{LPP2005REC}
\alias{MSFT}
\alias{USDCHF}


\title{Time Series Data Sets} 


\description{
 
    Three data sets used in example files.
    
    The data sets are:
    
    \tabular{ll}{   
    \code{LPP2005REC} \tab Swiss pension fund assets returns benchmark, \cr
    \code{MSFT} \tab Daily Microsoft OHLC prices and volume, \cr
    \code{USDCHF} \tab USD CHF intraday foreign exchange xchange rates.}
    
}


\examples{
## Plot LPP2005 Example Data Set -  
   data(LPP2005REC)
   plot(LPP2005REC, type = "l")
   
## Plot MSFT Example Data Set - 
   data(MSFT)
   plot(MSFT[, 1:4], type = "l")
   plot(MSFT[, 5], type = "h")
   
## Plot USDCHF Example Data Set - 
   plot(USDCHF, type = "l")
}
 
 
\keyword{datasets}

