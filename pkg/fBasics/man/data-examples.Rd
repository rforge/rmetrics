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
    \code{Capitalization} \tab Market capitalization of domestic companies, \cr
    \code{cars2} \tab Data for various car models, \cr
    \code{DowJones30} \tab Down Jones 30 stocks, \cr
    \code{HedgeFund} \tab Hennessee Hedge Fund Indices, \cr
    \code{msft.dat} \tab Daily Microsoft OHLC prices and volume, \cr
    \code{nyse} \tab NYSE composite Index, \cr
    \code{PensionFund} \tab Swiss Pension Fund LPP-2005, \cr
    \code{swissEconomy} \tab Swiss Economic Data, \cr
    \code{SWXLP} \tab Swiss Pension Fund LPP-2000, \cr
    \code{usdthb} \tab Tick data of USD to THB.
  }    
}

%--------------------------------------------------------------
\details{
 \bold{Capitalization:}\cr
 \code{Capitalization}
 contains market capitalization of domestic companies from
 2003 - 2008 in USD millions.
 
 \bold{cars2:}\cr
 \code{cars2}
 contains the price, country, reliability, mileage, type, weight,
 engine displacement and net horsepower of various car
 models.
 
 \bold{DowJones30:}\cr
 \code{DowJones30}
 contains daily observations from the Dow Jones 30 Index series.
 Each of the thirty columns represents the closing price 
 of a stock in the Index.
 
 \bold{HedgeFund:}\cr
 \code{HedgeFund}
 contains monthly percentual returns of various hedge fund
 strategies from Hennessee Group LLC.
 
 \bold{msft.dat:}\cr
 \code{msft.dat}
 contains daily prices (open, high, low and close) and
 volumes for the Microsoft stocks.
 
 \bold{nyse:}\cr
 \code{nyse}
 contains daily records of the NYSE Composite Index.
 
 \bold{PensionFund:}\cr
 \code{PensionFund}
 is a daily data set of the Swiss pension fund benchmark LPP-2005.
 The data set ranges from 2005-11-01 to 2007-04-11.
 The columns are named: SBI, SPI, SII, LMI, MPI, ALT, LPP25, LPP40, LPP60
 
 \bold{swissEconomy:}\cr
 \code{swissEconomy}
 contains the GDP per capita, exports, imports, interest rates, inflation,
 unemployment and population for the years 1964 - 1999 for Switzerland.
 
 \bold{SWXLP:}\cr
 \code{SWXLP}
 is a daily data set of the Swiss pension fund benchmark LPP-2000.
 The data set ranges from 2000-01-03 to 2007-05-08.
 The columns are named: SBI, SPI, SII, LP25, LP40, LP60.
 
 \bold{usdthb:}\cr
 \code{usdthb}
 Tick data of US Dollar (USD) in Thailand Bhat (THB) colleted from
 Reuters. Format: YYYYMMDDhhmm. Column variables: delay time,
 contributer, bid and ask prices, and quality flag. It covers the 
 Asia FX crisis in June 1997.
}

%--------------------------------------------------------------
\references{
 \bold{Capitalization:}\cr
 \emph{World Federation of Stock Exchanges},
 http://www.world-exchanges.org/statistics.
 
 \bold{cars2:}\cr
 Derived from the car90 dataset within the rpart package.
 The car90 dataset is based on the car.all dataset in S-PLUS.
 Original data comes from:
 April 1990, \emph{Consumer Reports Magazine},
 pages 235-255, 281-285 and 287-288.
 % Package rpart (dataset: car90)
 % This is derived (with permission) from the data set car.all
 % in S-PLUS, but with some further clean up of variable names and definitions. 
 % http://ftp.yzu.edu.tw/CRAN/doc/contrib/Lam-IntroductionToR_LHL.pdf
 % http://www.uni-muenster.de/ZIV.BennoSueselbeck/s-html/helpfiles/car.all.html
 
 \bold{DowJones30}\cr
 http://www.yahoo.com.
 
 \bold{HedgeFund:}\cr
 http://www.hennesseegroup.com/indices/returns/year/2005.html.
 
 \bold{msft.dat:}\cr
 http://www.yahoo.com.
 
 \bold{nyse:}\cr
 http://www.nyse.com.
 
 \bold{PensionFund:}\cr
 SBI, SPI, SII: SIX (Swiss Exchange Zurich);
 LPP25, LPP40, LPP60: Banque Pictet Geneva;
 LMI, MPI, ALT: Recalculated from the indices and benchmarks
 
 \bold{swissEconomy:}\cr
 http://www.oecd.org/ and http://www.imf.org/.
 
 \bold{SWXLP:}\cr
 SBI, SPI, SII: SIX (Swiss Exchange Zurich);
 LPP25, LPP40, LPP60: Banque Pictet Geneva
 
 \bold{usdthb:}\cr
 Reuters Select Feed Terminal (1997).

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
