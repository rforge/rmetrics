\name{Yahoo}

\alias{Yahoo}

\alias{yahooImport}
\alias{yahooSeries}
\alias{keystatsImport}
\alias{briefingImport}


\title{Import Market Data from Yahoo}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from Yahoo's
    web site.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{yahooImport} \tab daily stock market data from Yahoo's Web site, \cr
    \code{yahooSeries} \tab easy to use download from Yahoo, \cr 
    \code{keystatsImport} \tab key statistics from Yahoo's Web site. \cr
    \code{briefingImport} \tab more key statistics from Yahoo's Web site. }
   
}


\usage{ 
yahooImport(query, file = "tempfile", 
    source = "http://chart.yahoo.com/table.csv?", save = FALSE,
    sep = ";", swap = 20, try = TRUE)
   
yahooSeries(symbols = c("^DJI", "IBM"), from = NULL, to = NULL, 
    nDaysBack = 365, quote = c("Open", "High", "Low", "Close", "Volume"), 
    aggregation = c("d", "w", "m"), returnClass = c("timeSeries", "ts", 
    "matrix", "data.frame"), getReturns = FALSE, \dots)
    
keystatsImport(query, file = "tempfile", 
    source = "http://finance.yahoo.com/q/ks?s=", save = FALSE, 
    try = TRUE) 
    
briefingImport(query, file = "tempfile", 
    source = "http://finance.yahoo.com/q/ud?s=", 
    save = FALSE, try = TRUE) 
}


\arguments{

    \item{aggregation}{
        a character string denoting the aggregation level of the 
        downloaded data records, \code{d} for daily, \code{w} for
        weekly and \code{m} for monthly data records.
        }
    \item{file}{
        a character string with filename, usually
        having extension ".csv", where to save the downloaded data.
        }
    \item{from, to}{
        an ISO-8601 formatted character string of the starting (end) 
        date, e.g. "2005-01-01".
        }
    \item{getReturns}{
        a logical flag. Should return values be computed using the 
        function \code{returnSeries}?
        }
    \item{nDaysBack}{
        an integer giving the length of the download period in number 
        of days starting n days back from today. Only in use if 
        \code{from} and \code{to} are not specified.
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
        a character string naming the class of the object to be 
        returned. By default the function returns 
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
        a character string value or vector, the Yahoo symbol name(s).
        }
    \item{swap}{
        an integer value which determines when we swap from the
        19th to 20th century, by default 20, i.e. we swap 1920.
        This is necessary since Yahoo does not list the century in
        its dates, e.g. "15-Aug-02".
        }
    \item{try}{
        a logical value, if set to TRUE the Internet access will be
        checked.
        }
    \item{\dots}{
        optional arguments to be passed.
        }        
}


\value{

    The function \code{yahooImport} returns an S4 object of class 
    \code{fWEBDATA} with the following slots:
    
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
        
    The function \code{yahooSeries} returns an S4 object of 
    class  \code{timeSeries} or alternatively an object specified by
    the function argument \code{returnClass}.
    \cr
    
    The function \code{keystatsImport} returns a data frame with 
    key statistics downloaded from yahoo's web site.
    
}


\details{
  
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
            
}


\note{
  
    \bold{Internet Download Functions:}
    \cr\cr
    IMPORTANT NOTE: If the service provider changes the data file format 
    it may become necessary to modify and update the functions. 
    \cr
    
    The R package \code{tseries} from Adrian Trapletti offers an 
    alternative function to download stock market data and indexes 
    from Yahoo's Internet site.
  
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
## yahooImport -
   yahooSeries("IBM")
}


\keyword{data}

