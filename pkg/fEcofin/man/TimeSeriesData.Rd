\name{TimeSeriesData}

\alias{TimeSeriesData}

\alias{bmwRet}
\alias{CPI.dat}
\alias{danishClaims}
\alias{dem2gbp}
\alias{DEM98NYC}
\alias{DEM98ZRH}
\alias{DowJones30}
\alias{ford.s}
\alias{hp.s}
\alias{IP.dat}
\alias{klein}
\alias{kmenta}
\alias{msft.dat}
\alias{nelsonplosser}
\alias{nyse}
\alias{recession}
\alias{shiller.dat}
\alias{shiller.annual}
\alias{singleIndex.dat}
\alias{sp500dge}
\alias{sp500index}
\alias{surex1.ts}
\alias{usdchf}
\alias{usdthb}
\alias{yhoo.df}


\title{Time Series Data Sets} 


\description{

    A collection and description of data sets 
    used in the examples of the Rmetrics packages.
    \cr
    
    The data sets are:
    
    \tabular{ll}{   
    \code{bmwRet} \tab Daily BMW Stock Returns, \cr
    \code{CPI.dat} \tab US Consumer Price Index, \cr
    \code{IP.dat} \tab US Industrial Production Index, \cr
    \code{danishClaims} \tab Danish Fire Losses, \cr
    \code{dem2gbp} \tab DEM/GBP Foreign Exchange Rate, \cr
    \code{DEM98NYC} \tab BID prices of USDDEM FX rates recorded at NYC, \cr
    \code{DEM98ZRH} \tab BID prices of USDDEM FX rates recorded at ZRH, \cr
    \code{DowJones30} \tab Down Jones 30 Stocks, \cr
    \code{ford.s} \tab Daily Ford Stock Prices, \cr
    \code{hp.s} \tab Daily Hewlett-Packard Stock Prices, \cr
    \code{klein} \tab Klein's US Economic Data Set, \cr
    \code{kmenta} \tab Kmenta's US Economic Data Set, \cr
    \code{msft.dat} \tab Microsoft Stock Prices, \cr
    \code{nelsonplosser} \tab Nelson-Plosser US Economic Time Series, \cr
    \code{nyse} \tab NYSE Composite Index, \cr
    \code{recession} \tab US Recession Data Set, \cr
    \code{shiller.dat} \tab Shiller's Data Set, \cr
    \code{shiller.annual} \tab Shiller's Annual Data Set, \cr
    \code{singleIndex.dat} \tab US Index and Price Data Records, \cr
    \code{sp500dge} \tab Daily DGE SP500 Returns, \cr
    \code{sp500index} \tab Daily SP500 Index Returns, \cr
    \code{surex1.ts.dat} \tab Exchange Rate Spot Returns, \cr
    \code{yhoo.df} \tab Yahoo Stock Prices.}
   
}
    
    
\details{

       
    \bold{Daily BMW Stock Returns:}\cr
    \cr
    \code{bmwRet}
    is a daily data set of the German BMW tock returns.\cr
    The data set ranges from 1973-01-03 to 1996-07-23.\cr
    The columns are named: BMW.RET.\cr
    Source:\cr
    
    
    \bold{US Consumer Price Index:}\cr
    \cr
    \code{CPI.dat}\cr
    contains data representing seasonally adjusted US Industrial 
        Production Index.\cr 
    The data set ranges from  \cr
    The columns are named: \cr
    Source:\cr
    
    
    \bold{Industrial Production Index::}\cr
    \cr
    \code{IP.dat}\cr
    contains data representing seasonally adjusted US Consumer 
        Price Index.\cr   
    The data set ranges from\cr 
    The columns are named:\cr 
    Source:\cr
    
    
    \bold{Danish Fire Losses:}\cr
    \cr
    \code{danishClaims}
    contains data representing daily 
        danish fire losses in Million Danish Kronors.\cr 
    The data set ranges from 1980-01-03 to 1990-12-31.\cr
    The columns are named:\cr 
    Source:\cr
 
       
    \bold{DEM/GBP Foreign Exchange Rate:}\cr
    \cr
    \code{dem2gbp}
    contains daily observations of the 
        Deutschmark / British Pound foreign exchange log returns. 
        This data set has been promoted as an informal benchmark 
        for GARCH time-series software validation. See McCullough and 
        Renfro [1991], and Brooks, Burke, and Persand (2001) for details.
        The nominal returns are expressed in percent, as published in 
        Bollerslev and Ghysels (2001).\cr  
    The data set ranges from 1984-01-03 to 1991-12-31.\cr
    The columns are named: \cr
    Source: Journal of Business and Economic Statistics, 
    \emph{ftp://www.amstat.org}.\cr
    
    
    \bold{BID Prices of USDDEM FX Rates:}\cr
    \cr
    \code{DEM98NYC} and  \code{DEM98ZRH}
    contain intra-daily foreign exchange bid prices for the USDDEM
        exchange rate for the third week in March 1998. One file is 
        recorded in local New York City time, the other in local
        Zurich time.
    The data set ranges from:\cr
    The columns are named: BID\cr 
    Source: Diethelm Wuertz and Remo Schnidrig.\cr
    
    
    \bold{Down Jones 30 Stocks:}\cr
    \cr
    \code{DowJones30}
    contains daily observations from the Dow Jones 30 Index series.
        Each of the thirty columns represents the closing price 
        of a stock in the Index.\cr
    The data set ranges from 1991-01-02 to 2001-01-02.\cr
    The columns are named: \cr
    Source:\cr
    
    
    \bold{Daily Ford Stock Prices:}\cr
    \cr
    \code{ford.s}
    contains data representing 2000 daily stock returns for the Ford 
        shares traded at NYSE.\cr  
    The data set ranges from 1084-01-02 to 1991-12-31.\cr
    The columns are named:\cr
    Source:\cr
    
    
    \bold{Daily Hewlett-Packard Stock Prices:}\cr
    \cr
    \code{hp.s}
    contains data representing 2000 daily stock returns for the HP 
        shares traded at NYSE.\cr
    The data set ranges from 1084-01-02 to 1991-12-31.\cr  
    The columns are named:\cr 
    Source:\cr
    
    
    \bold{Klein's US Economic Data Set:}\cr
    \cr
    \code{klein}
    contains data for Klein's (1950) simple econometric model of the 
        US economy. The Klein data frame has 22 rows and 10 columns.\cr 
    The data set ranges from  
    The columns are named: year, c, p, wp, i, k.lag, x, wg, g, tax.
        They denote:\cr
        \code{year} years 1921-1941, in the POSIX data format \%Y-\%m-\%d,  \cr
        \code{c} the consumption, \cr 
        \code{p} the private profits, \cr
        \code{wp} the private wages, \cr
        \code{i} the investment, \cr 
        \code{k.lag} the capital stock, lagged one year, \cr 
        \code{x} the equilibrium demand, \cr 
        \code{wg} the government wages, \cr
        \code{g} the government non-wage spending, \cr 
        \code{tax} indirect business taxes and net exports. \cr
    Source: Greene (1993)\cr
    
    
    \bold{Kmenta's US Economic Data Set:}\cr
    \cr
    \code{kmenta}
    contains partly contrived data from Kmenta (1986), constructed to 
        illustrate estimation of a simultaneous-equation model. The data 
        set has 20 rows and 6 columns, where the first holds the ISO-8601 
        formatted date as "\%Y-\%m-\%d".
        The exogenous variables in the first four columns are based 
        on real data; the endogenous variables in the remaining two
        columns were generated by simulation.\cr
    The data set ranges from  \cr
    The columns are named: q, p, d, f, a.\cr
        They denote:\cr
        \code{q} food consumption per capita, \cr
        \code{p} ratio of food prices to general consumer prices, \cr
        \code{d} disposable income in constant dollars, \cr
        \code{f} ratio of preceding year's prices received by farmers 
            to general consumer prices, \cr
        \code{a} time in years (numbered from 1 to 20). \cr
    Source:\cr
    
    
    \bold{Microsoft Stock Prices:}\cr
    \cr
    \code{msft.dat}
    contains daily stock prices and volumes for the the Microsoft stocks.
    The data set ranges from 2000-09-27 to 2001-09-27\cr 
    The columns are named: Open, High, Low, Close, Volume.\cr
    Source: www.yahoo.com\cr

  
    \bold{Nelson-Plosser US Economic Time Series:}\cr
    \cr
    \code{nelsonplosser}
    contains the data set listing fourteen US economic time series used 
        by Nelson and Plosser in their seminal paper.\cr  
    The data set ranges from 1860-12-31 until 1970-12-31.\cr
    The columns are named: gnp.r, gnp.n, gnp.pc, ip, emp, ur, gnp.p,
        cpi, wg.n, wg.r, M, vel, bnd, sp.\cr
        They denote:\cr
        \code{gnp.r}-Real GNP, [Billions of 1958 Dollars], [1909-1970],\cr 
        \code{gnp.n}-Nominal GNP, [Millions of Current USD], [1909-1970],\cr 
        \code{gnp.pc}-Real Per Capita GNP, [1958 Dollars], [1909-1970],\cr  
        \code{ip}-Industrial Production Index, [1967 = 100], [1860-1970],\cr  
        \code{emp}-Total Employment, [Thousands], [1890-1970],\cr  
        \code{ur}-Total Unemployment Rate, [Percent], [1890-1970],\cr  
        \code{gnp.p}-GNP Deflator, [1958 = 100], [1889-1970],\cr  
        \code{cpi}-Consumer Price Index, [1967 = 100], [1860-1970],\cr  
        \code{wg.n}-Nominal Wages, [current Dollars], [1900-1970],\cr  
        \code{wg.r}-Real Wages, [Nominal wages/CPI], [1900-1970],\cr  
        \code{M}-Money Stock (M2), [Billions USD, annual avgs], [1889-1970],\cr  
        \code{vel}-Velocity of Money, [1869-1970],\cr  
        \code{bnd}-Basic Yields 30-year Corporate Bonds, [\% pa], [1900-1970],\cr 
        \code{sp}-Stock Prices, [Index; 1941-43 = 100], [1871-1970].\cr
    Source:\cr
      
    
    \bold{NYSE Composite Index:}\cr
    \cr
    \code{nyse}
    contains daily records of the NYSE Composite Index.\cr
    The data set ranges from  \cr
    The columns are named: \cr
    Source: NYSE.\cr

    
    \bold{US Recession Data Set:}\cr
    \cr
    \code{recession}
    holds the data set used in the regression analysis of US recession.
        The data include short and long term interest rates from the US,
        the 3 Month Tbills data from US FED,
        the 10 Year Tbonds data from US FED, and also the 
        Stock-Watson experimental recession index.\cr
    The data set ranges from \cr 
    The columns are named: cr
    Source:\cr 
        
    \bold{Shiller's Data Set:}\cr
    \cr
    \code{shiller.dat}
    holds the data used in the book "Irrational Exuberance" by Robert 
        Shiller.\cr   
    The data set ranges from January 1871 ato March 2001.\cr 
    The columns are named:\cr
        They denote:\cr
        \code{price} - monthly nominal US SP stock market prices,\cr
        \code{dividend} - nominal SP Composite Index dividends,\cr
        \code{earnings} - nominal SP Composite Index earnings,\cr
        \code{cpi} - US Consumer Price Indexes,\cr
        \code{real.price} - real US stock market prices,\cr
        \code{real.dividend} - real SP Composite Index dividends,\cr
        \code{real.earnings} - real SP Composite Index earnings,\cr
        \code{pe.10} - price-earnings ratios.\cr
    Source: Robert Shiller.\cr
    
    
    \bold{Shiller's Annual Data Set:}\cr
    \cr
    \code{shiller.annual}
    holds the annual data used in the book "Irrational Exuberance" by Robert 
        Shiller.\cr     
    The data set ranges from January 1871 ato March 2001. \cr  
    The columns are named:\cr
        They denote:\cr
        They denote:\cr
        \code{price} - monthly nominal US SP stock market prices,\cr
        \code{dividend} - nominal SP Composite Index dividends,\cr
        \code{earnings} - nominal SP Composite Index earnings,\cr
        \code{cpi} - US Consumer Price Indexes,\cr
        \code{real.price} - real US stock market prices,\cr
        \code{real.dividend} - real SP Composite Index dividends,\cr
        \code{real.earnings} - real SP Composite Index earnings,\cr
        \code{pe.10} - price-earnings ratios,\cr
        \code{dp.ratio} - dividend-price ratios,\cr
        \code{dp.yield} - dividend-price yield.\cr
    Source: Robert Shiller.\cr
    
    
    \bold{US Index and Price Data Records:}\cr
    \cr
    \code{singleIndex.dat}
    holds monthly index and price data records. Included are monthly closing 
        prices for Microsoft Corporation (MSFT) and SP500 Index (SP500).\cr
    The data set ranges from January 1990 to January 2001.\cr
    The columns are named: \cr 
    Source: \cr
    
    
    \bold{Daily DGE SP500 Returns:}\cr
    \cr
    \code{sp500dge}
    lists daily returns from the SP500 as used in 
        the paper of Ding, Granger and Engle.\cr  
    The data set ranges from\cr 
    The columns are named: 
    Source: Ding, Granger and Engle.\cr
    
    
    \bold{Daily SP500 Index Returns:}\cr
    \cr
    \code{sp500index}
    lists daily SP500 index values. \cr  
    The data set ranges from January 1995 until December 1999.\cr   
    The columns are named:\cr
    Source:\cr 
    
    
    \bold{Exchange Rate Spot Returns:}\cr
    \cr
    \code{surex1.ts.dat}
    contains exchange rate spot returns and forward premium data as used 
        in the article of Eric Zivot (2000).\cr
    The data set ranges from \cr 
    The columns are named:\cr
    Source:\cr
    
     
    \bold{Yahoo Stock Prices:}\cr
    \cr
    \code{yhoo.df}
    contains data representing daily transaction information of Yahoo 
        stocks.\cr 
    The data set ranges from \cr
    The columns are named: Date, Open, High, Low, Close, Volume.\cr
    Source: \cr

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
## Load Example Data Set:
   data(kmenta)
   kmenta
}
 
 
\keyword{datasets}

