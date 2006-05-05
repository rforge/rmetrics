\name{TimeSeriesImport}

\alias{TimeSeriesImport}

\alias{fWEBDATA}
\alias{fWEBDATA-class}

\alias{show.fWEBDATA}
\alias{show,fWEBDATA-method}

\alias{economagicImport}
\alias{yahooImport}
\alias{fredImport}
\alias{forecastsImport}

\alias{yahooSeries}

\alias{keystatsImport}
\alias{print.keystats}


\title{Import Market Data from the Internet}


\description{
    
    A collection and description of functions to import 
    financial and economic market from the Internet.
    Download functions are available for economic and
    financial market data form Economagic's, from 
    Yahoo's, and from the Federal Reserve's Internet
    sites.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{economagicImport} \tab Economic series from Economagic's Web site, \cr
    \code{yahooImport} \tab daily stock market data from Yahoo's Web site, \cr
    \code{yahooSeries} \tab easy to use download from Yahoo, \cr
    \code{keystatsImport} \tab key statistics from Yahoo's Web site, \cr
    \code{fredImport} \tab time series from St. Louis FRED Web site, \cr
    \code{forecastsImport} \tab monthly data from the Financial Forecast Center. }
    
}


\usage{
economagicImport(query, file = "tempfile", 
    source = "http://www.economagic.com/em-cgi/data.exe/",  
    frequency = c("quarterly", "monthly", "daily"), save = FALSE, 
    colname = "VALUE", try = TRUE)  
yahooImport(query, file = "tempfile", 
    source = "http://chart.yahoo.com/table.csv?", save = FALSE,
    sep = ";", swap = 20, try = TRUE)
keystatsImport(query, file = "tempfile", 
    source = "http://finance.yahoo.com/q/ks?s=", save = FALSE, 
    try = TRUE) 
fredImport(query, file = "tempfile", 
    source = "http://research.stlouisfed.org/fred2/series/", 
    frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
forecastsImport(query, file = "tempfile", 
    source = "http://www.forecasts.org/data/data/", save = FALSE, 
    try = TRUE) 
    
yahooSeries(symbols = c("^DJI", "IBM"), from = NULL, to = NULL, 
    nDaysBack = 365, quote = "Close", aggregation = c("d", "w", "m"), 
    returnClass = c("timeSeries", "ts", "matrix", "data.frame"), 
    getReturns = FALSE, ...)
        
show.fWEBDATA(object)
\method{print}{keystats}(x, \dots)
}


\arguments{

    \item{aggregation}{
        [yahooSeries] - \cr
        a character string denoting the aggregation level of the 
        downloaded data records, \code{d} for daily, \code{w} for
        weekly and \code{m} for monthly data records.
        }
    \item{colname}{
        [economagicImport] - \cr
        a character string which defines the name of the value
        column. By default "VALUE".
        }
    \item{file}{
        a character string with filename, usually
        having extension ".csv", where to save the downloaded data.
        }
    \item{frequency}{
        a character string, one of "quarterly", "monthly", or "daily",
        defining the frequency of the data records.
        }
    \item{from, to}{
        [yahooSeries] - \cr
        an ISO-8601 formatted character string of the starting (end) 
        date, e.g. "2005-01-01".
        }
    \item{getReturns}{
        [yahooSeries] - \cr
        a logical flag. Should return values be computed using the 
        function \code{returnSeries}?
        }
    \item{nDaysBack}{
        [yahooSeries] - \cr
        an integer giving the length of the download period in number 
        of days starting n days back from today. Only in use if 
        \code{from} and \code{to} are not specified.
        }
    \item{object}{
        an S4 object of class \code{"fWEBDATA"}.
        }
    \item{query}{
        a character string, denoting the location of the data at the 
        web site.
        }
    \item{save}{
        a logical value, if set to TRUE the downloaded data file will 
        be stored under the path and file name specified by the
        string \code{file}. By default FALSE.
        }
    \item{quote}{
        [yahooSeries] - \cr
        a character value or vector of strings giving the column name(s) 
        of those instruments to be extracted from the download. 
        }
    \item{returnClass}{
        [yahooSeries] - \cr
        a character string naming the class of the object to be 
        returned. By default the function \code{yahooSeries} returns 
        a \code{"timeSeries"} object, alternatives are: \code{"ts"}, 
        \code{"matrix"}, or \code{"data.frame"}.
        }
    \item{sep}{
        a character value, defining the field separator for the 
        destination file which saves the downloaded data records.
        By default a semicolon.
        }
    \item{source}{
        a character string with the download URL.
        }
    \item{symbols}{
        [yahooSeries] - \cr
        a character string value or vector, the Yahoo symbol name(s).
        }
    \item{swap}{
        [yahooImport] - \cr
        an integer value which determines when we swap from the
        19th to 20th century, by default 20, i.e. we swap 1920.
        This is necessary since Yahoo does not list the century in
        its dates, e.g. "15-Aug-02".
        }
    \item{try}{
        a logical value, if set to TRUE the Internet access will be
        checked.
        }
    \item{x}{
        [print.keystats] - \cr
        an object of class \code{keystats} as returned by the function
        \code{keystatsImport}.
        }
    \item{\dots}{
        optional arguments passed to the \code{print.keystats} function.
        }        
}


\value{

    The functions \code{economagicImport}, \code{fredImport}, and
    \code{yahooImport} return an S4 object of class \code{fWEBDATA} 
    with the following slots:
    
    \item{@call}{
        the function call.  
        }
    \item{@data}{
        the data as downloaded formatted as a data.frame.
        }
    \item{@param}{
        a character vector whose elements contain the values of selected 
        parameters of the argument list.
        }
    \item{@title}{
        a character string with the name of the download. This can be 
        overwritten specifying a user defined input argument.
        }
    \item{@description}{
        a character string with an optional user defined description. 
        By default just the current date when the test was applied will
        be returned.}
        
    The function \code{yahooSeries} returns by default an S4 object of 
    class  \code{timeSeries} or alternatively an object specified by
    the function argument \code{returnClass}.
    
    The function \code{keystatsImport} returns an S3 object of class 
    \code{keystsats} with two entries: \code{$query} holds the query
    name, and \code{keystats} holds the dataframe with the statistical
    values. 
    
}


\details{

    \bold{Import data from www.economagic.com}
    \cr\cr
    Frequently requested data files from Economagic for the US economy include:

    \tabular{ll}{
    \code{[query]}                    \tab Description: \cr
                                      \tab \cr
    \code{var/leading-ind-long}       \tab Index of Leading Economic Indicators  \cr
    \code{beana/t102l01}              \tab Real Gross Domestic Product  \cr
    \code{fedstl/trsp500}             \tab SP 500 Total Return \cr
    \code{fedstl/gnp}                 \tab Gross National Product in Current Dollars \cr
    \code{var/cpiu-long}              \tab Consumer Price Index - All Urban Consumers \cr
    \code{feddal/ru}                  \tab Unemployment Rate \cr
    \code{fedstl/indpro}              \tab Total Industrial Production Index \cr
    \code{fedstl/exjpus+2}            \tab FX Rate: Japanese Yen to one US Dollar \cr
    \code{fedstl/fedfunds+2}          \tab Federal Funds Rate \cr
    \code{fedstl/mdiscrt+2}           \tab Discount Rate \cr
    \code{fedbog/tcm30y+2}            \tab 30-Year Treasury Constant Maturity Rate \cr
    \code{fedstl/mprime+2}            \tab Bank Prime Loan Rate \cr
    \code{fedstl/tb3ms+2}             \tab 3-Month Treasury Bills - Secondary Market \cr
    \code{fedstl/tb6ms+2}             \tab 6-Month Treasury Bills - Secondary Market \cr
    \code{fedbog/cm+2}                \tab 30 Year Federal Home Loan Mortgages \cr
    \code{var/west-texas-crude-long}  \tab Price of West Texas Intermediate Crude }

  
    \bold{Import data from chart.yahoo.com:}
    \cr\cr
    The query string is given as 
    \cr\cr
            \code{s=SYMBOL&a=DD&b=MM&c=CCYY&g=d&q=q&z=SYMBOL&x=.csv} 
    \cr\cr
    where \code{SYMBOL} has to replaced by the symbol name of the 
    instrument, and \code{DD}, \code{MM}, and \code{CCYY} by the 
    day, month-1 and century/year when the time series should start.

    Here are some examples of symbols:

    \tabular{ll}{
    \code{[query]}  \tab Description: \cr
                    \tab \cr
    \code{^DJI}     \tab Dow Jones 30 Industrial Averages \cr
    \code{^NYA}     \tab New York Stock Exchange Composite \cr
    \code{^NDX}     \tab Nasdaq 100 Index \cr
    \code{^IXIC}    \tab Nasdaq Composite Index \cr
    \code{^TYX}     \tab US 30Y Treasury Bond Index \cr
    \code{IBM}      \tab BM DJIA Stock \cr
    \code{KO}       \tab Coca-Cola DJIA Stock }

    The meaning of the tokens in the query string are the following:

    \tabular{ll}{
    Token           \tab Description  \cr
                    \tab \cr
    \code{s}        \tab Selected Ticker-Symbol \cr
    \code{a}        \tab First Quote starts with Month (mm) \cr
    \code{b}        \tab First Quote starts with Day (dd) \cr
    \code{c}        \tab First Quote starts with Year (ccyy) \cr
    \code{d}        \tab Last Quote ends with Month (mm) \cr
    \code{e}        \tab Last Quote ends with Day (dd) \cr
    \code{f}        \tab Last Quote ends with Year (ccyy) \cr
    \code{z}        \tab Selected Ticker-Symbol }

    Note, that month tokens range between 0 and 11 for January to 
    December!
    \cr
    
    
    \bold{Key statistics data from finance.yahoo.com:}
    \cr\cr
    The functions downloads the key statistics for the specified
    equity query and returns the result as a two column data frame. 
    The key names included are: "Market Cap", "Enterprise Value",
    "Trailing P/E", "Forward P/E", "PEG Ratio", "Price/Sales",
    "Price/Book", "Enterprise Value/Revenue", "Enterprise Value/EBITDA",
    "Annual Dividend", "Dividend Yield", "Beta",
    "52-Week Change", "52-Week High", and "52-Week Low".
            
}


\note{
  
    \bold{Internet Download:}
    \cr\cr
    Note, that if the service provider changes the data file format 
    it may become necessary to modify and update the functions. 
    \cr
    The R package \code{tseries} from Adrian Trapletti offers an 
    alternative function to download stock market data and indexes 
    from Yahoo's Internet site.
    %\cr
    
    %\bold{Functions added to Splus Version:}
    %\cr\cr
    %These functions are only used when running Rmetrics under SPlus. 
    %Note, that only part of the functionality is supported which is 
    %required running these R functions for Rmetrics under Splus.
  
}


\examples{
\dontrun{
## SOURCE("fBasics.012A-TimeSeriesData")

## economagicImport -
   xmpBasics("\nStart: Daily Foreign Exchange Rates > ")
   USDEUR = economagicImport(query = "fedny/day-fxus2eu", 
     frequency = "daily", colname = "USDEUR")
   # Print Data Slot if Internet Download was Successful:
   if (!is.null(USDEUR)) print(USDEUR@data[1:20, ])

## economagicImport -
   xmpBasics("\nNext: USFEDFUNDS Monthly US FedFunds Rates > ")
   USFEDFUNDS = economagicImport(query = "fedstl/fedfunds+2", 
     frequency = "monthly", colname = "USFEDFUNDS")
   if (!is.null(USFEDFUNDS)) print(USFEDFUNDS@data[1:20, ])
   
## economagicImport -
   xmpBasics("\nNext: USDGNP Quarterly GNP Data Records > ")
   USGNP = economagicImport(query = "fedstl/gnp", 
     frequency = "quarterly", colname = "USGNP")
   if(!is.null(USGNP)) print(USGNP@data[1:20, ])  
     
## yahooImport -
   xmpBasics("\nNext: IBM Shares from Yahoo > ")
   # [test 19/20 century change 01-12-1999 -- 31-01-2000]
   query = "s=IBM&a=11&b=1&c=1999&d=0&q=31&f=2000&z=IBM&x=.csv"
   IBM = yahooImport(query)  
   if (!is.null(IBM)) print(IBM@data[1:20, ])
   
## keystatsImport -
   xmpBasics("\nNext: Key Statistics IBM Shares from Yahoo > ")  
   keystatsImport("IBM")  
   
## fredImport -
   xmpBasics("\nNext: DPRIME Daily Bank Prime Load Rate Records > ")
   DPRIME = fredImport("DPRIME")
   if (!is.null(DPRIME)) print(DPRIME@data[1:20, ])       
}
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\keyword{data}

