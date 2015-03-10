\name{fImport-package}


\alias{fImport-package}
\alias{fImport}


\docType{package}


\title{Import Data Package}


\description{

    The Rmetrics "fImport" package is a collection of utility functions 
    to download and manage data sets from the Internet or from other 
    sources.
    
}


\details{

    \tabular{ll}{
        Package:    \tab fImport\cr
        Type:       \tab Package\cr
        Version:    \tab R 3.1.2\cr
        Date:       \tab 2015\cr
        License:    \tab GPL Version 2 or later\cr
        Copyright:  \tab (c) 1999-2015 Rmetrics Assiciation\cr
        URL:        \tab \url{https://www.rmetrics.org}
    }
    
}


\section{1 Introduction}{
    
    The major content of this package is to provide download functions 
    for financial market data from the Internet. For this we have 
    implemented the web text browsers "Lynx"", "Links"", and "W3M"" 
    for an easy and straightforward download of data from the Internet.   
    
    Furthermore helpful utility functions are included to split numerical 
    data matrices, to split date character vectors, and to split strings 
    from downloads. This allows to create in a very easy way 
    \code{timeSeries} objects.
    
    Examples are provided for downloading data from the Federal Reserve
    data base in St. Louis, from Oanda FX, and from Yahoo Finance
    web portal.
    
    The data part contains instruments listings from the American Stock 
    Exchange, from the FED H15 Report, from the NASDAQ Stock Market,
    from the New York Stock Exchange, of OANDAs Foreign Exchange Rates,
    of STOXX Indices, and from the Swiss Stock Exchange.
       
    For the download of spread sheets from the Internet we refer to the 
    functions \code{gdata::read.xls} and \code{xlsx::read.xlsx} for the 
    contributed \R packages \code{gdata} and \code{xlsx} respectively.
    
}


\section{2 Download Functions}{

    The package makes functions available to download financial market
    data from the internet. Currently functions are available for the 
    follwing web sites. The functions are:
    
    \preformatted{
    fredSeries      downloads data from research.stlouisfed.org
    oandaSeries     downloads data from www.oanda.com
    yahooSeries     downloads data from chart.yahoo.com
    }
    
    The economic and financial time series data are extractes as objects 
    of class \code{"timeSeries"}.   

}


\section{2 Readers and Web downloaders}{

    The package comes with the folowwing tailored readers and web 
    downloaders:
    
    \preformatted{
    read.lines      a synonyme function call to readLines
    read.links      uses the links browser to read from a web page
    read.lynx       uses the lynx browser to read from a web page
    read.w3m        uses the w3m browser to read from a web page
    }

}


\section{3 Split Function Utilities}{

    This section provides functions to split numerical data matrices, 
    to split date character vectors, and to split strings 
    from downloads:
    
    \preformatted{
    dataSplit       splits a data matrix from a downloaded file
    charvecSplit    splits a charvec vector from a downloaded file
    stringSplit     splits a string vector from a downloaded file
    }

}


\section{About Rmetrics}{

    The \code{fImport} Rmetrics package is written for educational 
    support in teaching "Computational Finance and Financial Engineering" 
    and licensed under the GPL.
    
}


\keyword{package} 


