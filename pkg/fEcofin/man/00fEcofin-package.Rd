\name{fEcofin-package}


\alias{fEcofin-package}


\alias{fEcofin}


\docType{package}


\title{Economic and Financial Data Package}


\description{

    Package of econometric and financial data sets.
}


\details{

    \tabular{ll}{
        Package:    \tab fEcofin\cr
        Type:       \tab Package\cr
        Version:    \tab 261.73.1\cr
        Date:       \tab 2008\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }

}


\section{Overview of Topics:}{

    1. CIA Factbook Data Sets\cr
    2. Data Sets for the fBond Package\cr
    3. Data Sets for the fPortfolio Package\cr
    4. Data Sets from the PerformanceAnalytics Package\cr
    5. Data Sets from the R for Quants Textbook\cr
    6. Data Sets used in several Rmetrics Packages\cr
    7. World Federation of Stock Exchanges Data Sets\cr
    8. Data Sets from Zivot-Wang's Textbook.
    
}
    
    
\section{1. CIA Factbook Data Sets}{

    
    A collection and description of functions to extract 
    financial and economic market statistics from the
    data available in the CIA World Factbook.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{ciaCountries} \tab Returns a list of CIA country codes, \cr
    \code{ciaIndicators} \tab Returns a list of CIA indicator codes, \cr
    \code{ciaByCountry} \tab Returns all Indicators by country, \cr 
    \code{ciaByIndicator} \tab Returns for all countries indicator ranking. }
    
}


\section{2. Data Sets for the fBond Package}{

    contains demo data sets from the Bond Market.
    
    The data sets are:
    
    \tabular{ll}{
    \code{bundesbankNSS} \tab Nelson-Siegel-Svensson Coefficients, \cr
    \code{mk.zero2} \tab US zero-coupon yield curve, \cr
    \code{mk.maturity} \tab US term structure maturities. }
    
}


\section{3. Data Sets for the fPortfolio Package}{

    contains demo data sets for portfolio optimization.
    
    The data sets are:
    
    \tabular{ll}{   
    \code{altInvest} \tab Monthly Alternative Investment Data Set, \cr
    \code{annualInvest} \tab Annual Investment Data Set, \cr
    \code{assetsCorr} \tab US Asset Correlation Matrix, \cr
    \code{berndtInvest} \tab Berndt's Investment Data Set, \cr
    \code{jobstCov} \tab Covariance Matrix of 30 Stocks, \cr 
    \code{largecap.ts} \tab Monthly US Largecap Equities, \cr
    \code{microcap.ts} \tab Monthly US Microcap Equities, \cr
    \code{midcap.ts} \tab Monthly US Midcap Equities, \cr
    \code{smallcap.ts} \tab Monthly US Smallcap Equities, \cr
    \code{midcapD.ts} \tab Daily US Midcap Equities, \cr
    \code{returns.three.ts} \tab Returns, \cr
    \code{SWXLP} \tab Swiss Pension Fund LPP-2000, \cr
    \code{LPP2005REC} \tab Swiss Pension Fund LPP-2005. \cr
    \code{equityFunds} \tab Equity Funds. }
    
}


\section{4. Data Sets from the Performance Analytics Package}{


    contains data sets for use in the examples of portfolio performance
    analytics.
    
    The data sets are:
    
    \tabular{ll}{
    \code{edhec.tS} \tab composite hedge fund style index returns, \cr
    \code{managers.tS} \tab fixed income benchmarks. }
   
}    


\section{5. Data Sets from the R for Quants Textbook}{

    contains data sets from the book "Basic R for Quants".

}


\section{6. Data Sets used in several Rmetrics Packages}{

    contains general data sets used in many Rmetrics packages.

}


\section{7. World Federation of Stock Exchanges Data Sets}{


    contains data sets of financial and economic market 
    statistics from exchange data collected by the World 
    Federation of Stock Exchanges.
    \cr
    
    The data sets are:
    
    \tabular{ll}{
    \code{wfe1} \tab Market capitalization of domestic companies, \cr
    \code{wfe2} \tab Total number of companies with shares listed, \cr
    \code{wfe3} \tab Total value of share trading, \cr 
    \code{wfe4} \tab Market value of bonds listed, \cr
    \code{wfe5} \tab Total value of bond trading, and \cr
    \code{wfe6} \tab Price earning ratio an gross dividend yield. }
    
}


\section{8. Data Sets from Zivot-Wang's Textbook}{

    contains demo time series data sets for economic and 
    financial market analysis.
    
    The data sets are:
    
    \tabular{ll}{   
    \code{bmwRet} \tab Daily BMW Stock Returns, \cr
    \code{CPI.dat} \tab US Consumer Price Index, \cr
    \code{IP.dat} \tab US Industrial Production Index, \cr
    \code{danishClaims} \tab Danish Fire Losses, \cr
    \code{dem2gbp} \tab DEM/GBP Foreign Exchange Rate, \cr
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


\keyword{package}

