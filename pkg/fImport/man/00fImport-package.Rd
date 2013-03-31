\name{fImport-package}


\alias{fImport-package}


\alias{fImport}


\docType{package}


\title{Import Data Package}


\description{

    Package of some utility functions to download and manage data sets
    from the Internet or from other sources.
}


\details{

    \tabular{ll}{
        Package:    \tab fImport\cr
        Type:       \tab Package\cr
        Date:       \tab 2008, 2009, 2010\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2008 Diethelm Wuertz and Rmetrics Foundation\cr
        URL:        \tab \url{http://www.rmetrics.org}
    }
    
    Major package content:
    
    1) Download functions implementing the web text browsers Lynx, Links, and 
    W3M for an easy and straightforward download of data from the Internet.   
    
    2) Helpful utility functions to split numerical matrices, date character
    vectors, and strings from downloads to create in a very easy way timeSeries 
    objects.
    
    3) Examples of import and series funcions for downloading data from the 
    Federal Reserve, from Oanda, and from Yahoo Finance.
    
    4) Instruments listings from the American Stock Exchange,
    from the FED H15 Report, from the NASDAQ Stock Market,
    from the New York Stock Exchange, of OANDAs Foreign Exchange Rates,
    of STOXX Indices, and from the Swiss Stock Exchange,
       
    For the download of spread sheets from the Internet we refer to the functions
    \code{gdata::read.xls} and \code{xlsx::read.xlsx} for the R packages 
    \code{gdata} and \code{xlsx} respectively.
      
}
    
  
\section{Examples - Time Series Data:}{

    The package makes functions available to download financial market
    data from the internet. Functions are available for the follwing 
    web sites:
    
    1. Data from research.stlouisfed.org\cr
    2. Data from www.oanda.com\cr
    3. Data from chart.yahoo.com
    
    There are two kinds of functions available, the first kind is called 
    \code{*Series} which downloads a \code{"timeSeries"} object from a
    web site wher the star \code{*} is a placeholder for the web site
    (fred, oanda, and yahoo), and the second kind is called
    \code{*Import} which downloads an S4 object of class \code{"fWEBDATA"}
    wit a \code{@Data} slot which keeps the \code{"timeSeries"} object
    and further slots which keep additional download information.
    
    IMPORTANT NOTE: The download from the web site www.forecasts.org has
    been withdrawn since the time series are no longer updated. Note, all 
    the series offered by the "forecasts.org" web site are also available
    from the FRED St. Louis database.


    The functions    
    
    1. \code{fredSeries}\cr
    2. \code{oandaSeries}\cr
    3. \code{yahooSeries}

    allow for an easy download of economic and financial time series data 
    as objects of class \code{"timeSeries"}.   
    
    Note, with version 280.73 major changes were made in the argument list of 
    the downloading functions. We apologize for any inconveniences caused
    by these changes. The reason was that now all functions have the same
    arguments which makes their usage much easier. For details we refer to 
    the help pages of the functions mentioned above.

    If you like to keep additional download information, use the functions
    
    1. \code{fredImport}\cr
    2. \code{oandaImport}\cr
    3. \code{yahooImport}   

    which return objects of class \code{"fWEBDATA"}. Feel free to extend
    and modify these functions to your personal needs.
    
}    


\section{Examples - Further Yahoo Utilities:}{

    The function \code{yahooKeystats} allows to download key statistics from 
    Yahoo's web site, and the function \code{yahooBriefings} allows to 
    download a briefing from Yahoo's web site.
        
    Note: These two function are no longer supported (since March 2012). The 
    reason for this are the too many changes on the Yhaoo web site which made
    the download very prone to many failures. Feel free to use the original 
    code (which is still available) to update and adapt the function to your 
    specific needs.
    
}   


\section{About Rmetrics:}{

    This Rmetrics package is written for educational support in teaching 
    "Computational Finance and Financial Engineering" and licensed
    under the GPL.
    
}


\references{

    Diethelm Wuertz, Yohan Chalabi, and Andrew Ellis, (2010);
    \emph{Financial Market Data for R/Rmetrics}, 
    Rmetrics eBook, Rmetrics Association and Finance Online, Zurich,
    www.rmetrics.org.
       
}

   
\keyword{data} 
       
    