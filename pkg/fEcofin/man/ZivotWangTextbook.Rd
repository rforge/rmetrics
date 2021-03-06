\name{ZivotWangData}

\alias{ZivotWangData}

\alias{CPI.dat}
\alias{DowJones30}
\alias{ford.s}
\alias{hp.s}
\alias{IP.dat}
\alias{msft.dat}
\alias{nelsonplosser}
\alias{nyse}
\alias{shiller.dat}
\alias{shiller.annual}
\alias{singleIndex.dat}
\alias{sp500dge}
\alias{surex1.ts}
\alias{yhoo.df}


\title{Selected Data Sets from Zivot and Wang} 


\description{

    Data sets from the textbook "Modeling Financial Time Series 
    with S-PLUS" written by Eric Zivot and Jiahui Wang.
    
    The data sets are:
    
    \tabular{ll}{   
    \code{CPI.dat} \tab US Consumer Price Index, \cr
    \code{DowJones30} \tab Down Jones 30 Stocks, \cr
    \code{ford.s} \tab Daily Ford Stock Prices, \cr
    \code{hp.s} \tab Daily Hewlett-Packard Stock Prices, \cr
    \code{IP.dat} \tab US Industrial Production Index, \cr
    \code{msft.dat} \tab Microsoft Stock Prices, \cr
    \code{nelsonplosser} \tab Nelson-Plosser US Economic Time Series, \cr
    \code{nyse} \tab NYSE Composite Index, \cr
    \code{shiller.dat} \tab Shiller's Data Set, \cr
    \code{shiller.annual} \tab Shiller's Annual Data Set, \cr
    \code{singleIndex.dat} \tab US Index and Price Data Records, \cr
    \code{sp500dge} \tab Daily DGE SP500 Returns, \cr
    \code{surex1.ts.dat} \tab Exchange Rate Spot Returns, \cr
    \code{yhoo.df} \tab Yahoo Stock Prices.}
   
}
    
    
\details{
    
    \bold{US Consumer Price Index:}\cr
    \cr
    \code{CPI.dat}\cr
    contains data representing seasonally adjusted US Industrial 
        Production Index.\cr 
    The data set ranges from  \cr
    The columns are named:  
    
    
    \bold{Industrial Production Index:}\cr
    \cr
    \code{IP.dat}\cr
    contains data representing seasonally adjusted US Consumer 
        Price Index.\cr   
    The data set ranges from \cr 
    The columns are named: 
    
    
    \bold{Down Jones 30 Stocks:}\cr
    \cr
    \code{DowJones30}
    contains daily observations from the Dow Jones 30 Index series.
        Each of the thirty columns represents the closing price 
        of a stock in the Index.\cr
    The data set ranges from 1991-01-02 to 2001-01-02.\cr
    The columns are named: 
    
    
    \bold{Daily Ford Stock Prices:}\cr
    \cr
    \code{ford.s}
    contains data representing 2000 daily stock returns for the Ford 
        shares traded at NYSE.\cr  
    The data set ranges from 1084-01-02 to 1991-12-31.\cr
    The columns are named: 
    
    
    \bold{Daily Hewlett-Packard Stock Prices:}\cr
    \cr
    \code{hp.s}
    contains data representing 2000 daily stock returns for the HP 
        shares traded at NYSE.\cr
    The data set ranges from 1084-01-02 to 1991-12-31.\cr  
    The columns are named: 
    
    
    \bold{Microsoft Stock Prices:}\cr
    \cr
    \code{msft.dat}
    contains daily stock prices and volumes for the the Microsoft stocks.
    The data set ranges from 2000-09-27 to 2001-09-27\cr 
    The columns are named: Open, High, Low, Close, Volume.\cr
    Source: www.yahoo.com 

  
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
      
    
    \bold{NYSE Composite Index:}\cr
    \cr
    \code{nyse}
    contains daily records of the NYSE Composite Index.\cr
    The data set ranges from  \cr
    The columns are named: 

        
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
    Source: Robert Shiller. 
    
    
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
    Source: Robert Shiller. 
    
    
    \bold{US Index and Price Data Records:}\cr
    \cr
    \code{singleIndex.dat}
    holds monthly index and price data records. Included are monthly closing 
        prices for Microsoft Corporation (MSFT) and SP500 Index (SP500).\cr
    The data set ranges from January 1990 to January 2001.\cr
    The columns are named: \cr 
    
    
    \bold{Daily DGE SP500 Returns:}\cr
    \cr
    \code{sp500dge}
    lists daily returns from the SP500 as used in 
        the paper of Ding, Granger and Engle.\cr  
    The data set ranges from\cr 
    The columns are named: 
    Source: Ding, Granger and Engle. 
     
    
    \bold{Exchange Rate Spot Returns:}\cr
    \cr
    \code{surex1.ts.dat}
    contains exchange rate spot returns and forward premium data as used 
        in the article of Eric Zivot (2000).\cr
    The data set ranges from \cr 
    The columns are named: 
    
     
    \bold{Yahoo Stock Prices:}\cr
    \cr
    \code{yhoo.df}
    contains data representing daily transaction information of Yahoo 
        stocks.\cr 
    The data set ranges from \cr
    The columns are named: Date, Open, High, Low, Close, Volume.

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
    
Nelson C.R., Plosser C.I. (1982); 
    \emph{Trends and Random Walks in Macroeconomic Time Series},
    Journal of Monetary Economics, 10, 139--162. 

Zivot E. (2000);
    \emph{Cointegration and forward and spot exchange rate regressions},
    Journal of International Money and Finance 19, 785--812, and 387--401. 

}


\examples{
## Load Example Data Set:
   data(CPI.dat)
   head(CPI.dat)
}
 
 
\keyword{datasets}

