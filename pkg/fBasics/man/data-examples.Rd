\name{fBasicsData}


\alias{fBasicsData}

\alias{Capitalization}
\alias{cars2}
\alias{DowJones30}
\alias{HedgeFund}
\alias{msft.dat}
\alias{nyse}
\alias{PensionFund}
\alias{swissEconomy}
\alias{SWXLP}
\alias{usdthb}

%%% These should be removed
\alias{.HedgeFund1}
\alias{.HedgeFund2}


%--------------------------------------------------------------
\title{fBsiscs Data Sets} 

\description{
  The following data sets are part of this package:
  \tabular{ll}{
    \code{Capitalization} \tab Description Here, \cr
    \code{cars2} \tab Description Here, \cr
    \code{DowJones30} \tab Down Jones 30 Stocks, \cr
    \code{HedgeFund} \tab Description Here, \cr
    \code{msft.dat} \tab Daily Microsoft OHLC prices and volume, \cr
    \code{nyse} \tab NYSE Composite Index, \cr
    \code{PensionFund} \tab Pension Fund Portfolio, \cr
    \code{swissEconomy} \tab Description Here, \cr
    \code{SWXLP} \tab Swiss Pension Fund LPP-2000, \cr
    \code{usdthb} \tab Description Here.
  }    
}

%--------------------------------------------------------------
\details{
 \bold{Capitalization:}\cr
 \code{Capitalization}
 contains ...
 
 \bold{cars2:}\cr
 \code{cars2}
 contains ...
 
 \bold{DowJones30:}\cr
 \code{DowJones30}
 contains daily observations from the Dow Jones 30 Index series.
 Each of the thirty columns represents the closing price 
 of a stock in the Index.
 
 \bold{HedgeFund:}\cr
 \code{HedgeFund}
 contains ...
 
 \bold{msft.dat:}\cr
 \code{msft.dat}
 contains daily prices (open, high, low and close) and
 volumes for the the Microsoft stocks.
 
 \bold{nyse:}\cr
 \code{nyse}
 contains daily records of the NYSE Composite Index.
 
 \bold{PensionFund:}\cr
 \code{PensionFund}
 contains ...
 
 \bold{swissEconomy:}\cr
 \code{swissEconomy}
 contains ...
 
 \bold{SWXLP:}\cr
 \code{SWXLP}
 is a daily data set of the Swiss pension fund benchmark LPP-2000.
 The data set ranges from 2000-01-03 to 2007-05-08.
 The columns are named: SBI, SPI, SII, LP25, LP40, LP60.
 
 \bold{usdthb:}\cr
 \code{usdthb}
 contains ...
}

%--------------------------------------------------------------
\references{
 \bold{Capitalization:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{cars2:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{DowJones30}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{HedgeFund:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{msft.dat:}\cr
 www.yahoo.com
 
 \bold{nyse:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{PensionFund:}\cr
 Pictet (2005);
 \emph{Pension Fund Benchmark};
 www.pictet.ch
 
 \bold{swissEconomy:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{SWXLP:}\cr
 Authors; \emph{Article}; Journal, pages
 
 \bold{usdthb:}\cr
 Authors; \emph{Article}; Journal, pages

 % \bold{Example:}\cr
 % McCullough B.D., Renfro C.G. (1998);
 % \emph{Benchmarks and Software Standards: A Case Study of GARCH Procedures},
 % Journal of Economic and Social Measurement 25, 59--71. 
}

%--------------------------------------------------------------
\examples{
## Plot DowJones30 Example Data Set
   series <- as.timeSeries(DowJones30)
   head(series)
   plot(series[,1:6], type = "l")
}
 
\keyword{datasets}
