\name{fSeriesData}

\alias{fSeriesData}

\alias{nyseres}
\alias{recession}

\alias{dem2gbp}
\alias{cac40}
\alias{sp500dge}

\alias{ibmbj}

\alias{nelsonplosser}
\alias{surex1.ts}
\alias{rf.30day}

\alias{black.ts}
\alias{klein}
\alias{kmenta}

\alias{gnptbill}


\title{fSeries Data Sets} 


\description{

    A collection and description of data sets 
    including recession data from the US, indicators 
    for daily SP500 stock data together with an set 
    of trading indicators for technical analysis, 
    daily observations of the Deutschmark / British
    Pound foreign exchange log returns and CAC40 index 
    returns with realized volatility both used in Garch 
    benchmark measurements, the fourteen US economic 
    time series used by Nelson and Plosser in their 
    seminal paper, several public data sets used in 
    the book 'Modelling financial time series with S-Plus' 
    written by E. Zivot and J. Wang, data for Klein's 
    simple econometric model of the US economy, and 
    data from Kmenta constructed to illustrate estimation 
    of a simultaneous-equation models.
	\cr

	
  	\code{recession}
  	\cr
  	Data sets used in the regression analysis.
  	Recession data from the US,
  	3 Month Tbills data from US FED,
  	10 Year Tbonds data from US FED,
  	Stock-Watson experimental recession index.
  	\cr	
  	
  	
  	\code{dem2gbp}
  	\cr
  	The file \code{"dem2gbp"} contains daily observations of the 
	Deutschmark / British Pound foreign exchange log returns. 
	This data set has been promoted as an informal benchmark 
	for GARCH time-series software validation. See McCullough and 
	Renfro [1991], and Brooks, Burke, and Persand (2001) for details.
	The nominal returns are expressed in percent, as published in 
	Bollerslev and Ghysels (2001). The data set is available from 
	the \emph{Journal of Business and Economic Statistics}, (JBES), 
	\emph{ftp://www.amstat.org}. A text file has one column of 
	data listing the percentual log-returns of the DEM/GBP exchange 
	rates. The sample period is from January 3, 1984, to December 
	31, 1991, for a total of 1975 daily observations of FX exchange 
	rates.
	\cr
	
	
	\code{cac40}
	\cr
	Daily CAC40 index returns with realized volatility. The data 
	cover the period January 4, 1928 until August 31, 1991. The
	first colum hold the ISO-8601 formatted dated, and the second 
	the index values. The data were used in the paper of Ding,
	Granger and Engle [1993] introducing the APARCH model. It can 
	be consider as a benchmark data set.
	\cr
	
	
	\code{sp500dge}
	\cr
	Daily SP500 index values. The data 
	cover the period January 1995 until December 1999, and have 1249 
	observations. The first column of the file \code{CAC40} lists 
	the averaged return of the Index, and the second column 
	\code{CAC40VOL} lists the realized volatility. The data are
	used as a benchmark for GARCH modeling by Laurent and Peters
	(2002).\cr
	\code{"\%Y-\%m-\%d"} - ISO-8601 Dates, \cr 
	\code{"SP500"} - SP500 Index Value.
	\cr
		
	
	\code{ibmbj}
	\cr
	The daily stock market price seriesB from Box and Jenkins [1976].
	A numeric vector of closing prices on 369 consecutive trading days 
	starting May 19, 1961.
	\cr
	
	
	\code{nelsonplosser}
	\cr
	This data set contains the fourteen US economic time series 
	used by Nelson and Plosser in their seminal paper. The time 
	series are: \cr
	\code{"\%Y\%m\%d"} - Date index from 18601231 until 19701231, \cr 
	\code{"gnp.r"} - Real GNP, [Billions of 1958 Dollars], [1909 - 1970], \cr 
	\code{"gnp.n"} - Nominal GNP, [Millions of Current Dollars], 
		[1909 - 1970], \cr 
	\code{"gnp.pc"} - Real Per Capita GNP, [1958 Dollars], [1909 - 1970], \cr  
	\code{"ip"} - Industrial Production Index, [1967 = 100], [1860 - 1970], \cr  
	\code{"emp"} - Total Employment, [Thousands], [1890 - 1970], \cr  
	\code{"ur"} - Total Unemployment Rate, [Percent], [1890 - 1970], \cr  
	\code{"gnp.p"} - GNP Deflator, [1958 = 100], [1889 - 1970], \cr  
	\code{"cpi"} - Consumer Price Index, [1967 = 100], [1860 - 1970], \cr  
	\code{"wg.n"} - Nominal Wages, [current Dollars], [1900 - 1970], \cr  
	\code{"wg.r"} - Real Wages, [Nominal wages/CPI], [1900 - 1970], \cr  
	\code{"M"} - Money Stock (M2), [Billions of Dollars, annual averages], 
		[1889 - 1970], \cr  
	\code{"vel"} - Velocity of Money, [1869 - 1970], \cr  
	\code{"bnd"} - Basic Bond Yields of 30-year Corporate Bonds, 
		[Percent per annum], [1900 - 1970], \cr 
	\code{"sp"} - Stock Prices, [Index; 1941 - 43 = 100], [1871 - 1970].
	\cr
		
	
	\code{surex1.ts}
	\cr
 	This file contains monthly sampled exchange rate spot returns and 
 	forward premium data ranging from March 1976 to June 1996, with the 
 	following thirteen column entries: \cr
 	\code{"\%d-\%b-\%Y"} Date in POSIX date format, \cr
 	\code{"USCN.FP.lag1"} one month forward premium between USD and CAD, \cr
	\code{"USCNS.diff"} future returns on spot rate between USD and CAD, \cr 
	\code{"USDM.FP.lag1"} one month forward premium between USD and DEM, \cr 
	\code{"USDMS.diff"} future returns on spot rate between USD and DEM, \cr 
	\code{"USFR.FP.lag1"} one month forward premium between USD and FFR, \cr 
	\code{"USFRS.diff"} future returns on spot rate between USD and FFR, \cr 
	\code{"USIL.FP.lag1"} one month forward premium between USD and ITL, \cr 
	\code{"USILS.diff"} future returns on spot rate between USD and ITL, \cr 
	\code{"USJY.FP.lag1"} one month forward premium between USD and JPY, \cr 
	\code{"USJYS.diff"} future returns on spot rate between USD and JPY, \cr 
	\code{"USUK.FP.lag1"} one month forward premium between USD and GBP, \cr 
	\code{"USUKS.diff"} future returns on spot rate between USD and GBP.
	\cr
	
	
	\code{rf.30day}
	\cr
	This file contains monthly data from July 1926 to December 2000
	representing nominal interest rate on 30-day U. Treasury bills. 
    \cr
    
    
    \code{black.ts}
    \cr
    This file contains real monthly stock return data from January 1978 
    to December 1987 which are constructed from \code{berndt.dat}. The 
    data set holds the following columns: \cr
	\code{"BOISE"} real monthly returns of Boise. \cr
	\code{"CITCRP"} real monthly returns of Citicorp. \cr 
	\code{"CONED"} real monthly returns of Consolidated Edison. \cr
	\code{"CONTIL"} real monthly returns of Continental Illinois. \cr
	\code{"DATGEN"} real monthly returns of Data General. \cr
	\code{"DEC"} real monthly returns of Digital Equipment Company. \cr
	\code{"DELTA"} real monthly returns of Delta Airlines. \cr
	\code{"GENMIL"} real monthly returns of General Mills. \cr
	\code{"GERBER"} real monthly returns of Gerber. \cr
	\code{"IBM"} real monthly returns of International Business Machines. \cr
	\code{"MARKET"} real value-weighted composite monthly returns. \cr
	\code{"MOBIL"} real monthly returns of Mobile. \cr
	\code{"PANAM"} real monthly returns of Pan American Airways. \cr 
	\code{"PSNH"} real monthly returns of Public Service of New Hampshire. \cr 
	\code{"TANDY"} real monthly returns of Tandy. \cr
	\code{"TEXACO"} real monthly returns of Texaco. \cr
	\code{"WEYER"} real monthly returns of Weyerhauser. \cr
	Source: Berndt (1991) 

	
    \code{klein}
	\cr
	This file contains data for Klein's (1950) simple econometric model 
	of the US economy. The Klein data frame has 22 rows and 10 columns:
	Column \cr
	This data frame contains the following columns: 
	\code{year} years 1921-1941, represented in the POSIX data format 
		\%Y-\%m-\%d,  \cr
	\code{c} the consumption, \cr 
	\code{p} the private profits, \cr
	\code{wp} the private wages, \cr
	\code{i} the investment, \cr 
	\code{k.lag} the capital stock, lagged one year, \cr 
	\code{x} the equilibrium demand, \cr 
	\code{wg} the government wages, \cr
	\code{g} the government non-wage spending, \cr 
	\code{tax} indirect business taxes and net exports. \cr
 	Source: Greene (1993)

		
	\code{kmenta}
	\cr
 	This file contains partly contrived data from Kmenta (1986), 
 	constructed to illustrate estimation of a simultaneous-equation 
 	model. The data set has 20 rows and 6 columns, where the first
 	holds the ISO-8601 formatted date as "\%Y-\%m-\%d": 
 	The remaining columns are: \cr
 	\code{q} food consumption per capita, \cr
	\code{p} ratio of food prices to general consumer prices, \cr
	\code{d} disposable income in constant dollars, \cr
	\code{f} ratio of preceding year's prices received by farmers 
		to general consumer prices, \cr
	\code{a} time in years (numbered from 1 to 20). \cr
	The exogenous variables \code{d}, \code{f}, and \code{a} are based 
	on real data; the endogenous variables \code{p} and \code{q} were 
	generated by simulation. 

}


\format{
	
    All files are in CSV Excel spreadsheet format. The
    delimiter is a semicolon. 
    
}


\references{

Berndt E.R. (1991);
	\emph{The Practice of Econometrics: Classic and Contemporary},
	Addison-Wesley Publishing Co. 
	
Box G.E.P., Jenkins J.M. (1976);
	\emph{Time Series Analysis: Forecasting and Control},
	Holden Day, San Francisco.

Brooks C., Burke S.P., Persand G. (2001);
	\emph{Benchmarks and the Accuracy of GARCH Model Estimation},
	International Journal of Forecasting 17, 45--56.

Ding Z., Granger C.W.J., Engle R.F. (1993);
	\emph{A Long Memory Property of Stock Market Returns And a New
	Model},
	Journal of Empirical Finance 1, 83--106.
	
McCullough B.D., Renfro C.G. (1998);
	\emph{Benchmarks and Software Standards: A Case Study of GARCH 
	Procedures},
	Journal of Economic and Social Measurement 25, 59--71. 
	
Greene W.H. (1993);
	\emph{Econometric Analysis},
	Second Edition, Macmillan. 

Klein, L. (1950);
	 \emph{Economic Fluctuations in the United States 1921--1941},
	 Wiley. 

Kmenta J. (1997); 
	\emph{Elements of Econometrics}, 
	Second Edition, University of Michigan Publishing. 
	
Laurent S., Peters J.P. (2002); 
	\emph{G@RCH 2.2: An Ox Package for Estimating and Forecasting  
	Various ARCH Models}, 
	Journal of Economic Surveys 16, 447--485. 
	
Nelson C.R., Plosser C.I. (1982); 
	\emph{Trends and Random Walks in Macroeconomic Time Series},
	Journal of Monetary Economics, 10, 139--162. 

Zivot E. (2000);
	\emph{Cointegration and forward and spot exchange rate regressions},
	Journal of International Money and Finance 19, 785--812, and 387--401. 

}


\examples{
## SOURCE("fBasics.36A-fSeriesData")

## NYSE Residuals:
   data(nyseres)
   ts.plot(nyseres, xlab = "Index", ylab = "log-Returns",
   	 main = "NYSE: log-Returns")
} 
 
 
\keyword{datasets}

