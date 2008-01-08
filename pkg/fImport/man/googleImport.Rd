\name{Google}

\alias{Google}

\alias{googleImport}
\alias{googleSeries}


\title{Import Market Data from Google Finance}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from Google's
    finance web site.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{googleImport} \tab daily market data from Google's Web site, \cr
    \code{googleSeries} \tab easy to use download from Google. }
   
}


\usage{ 
googleImport(query, file = "tempfile", 
    source = "http://finance.google.com/finance/historical?", 
    save = FALSE, sep = ";", try = TRUE)
   
googleSeries(symbols = c("IBM", "MSFT"), from = NULL, to = NULL, 
    nDaysBack = 365, quote = c("Open", "High", "Low", "Close", "Volume"), 
    aggregation = c("d", "m"), returnClass = c("timeSeries", "ts", 
    "matrix", "data.frame"), getReturns = FALSE, \dots) 
}


\arguments{

    \item{aggregation}{
        a character string denoting the aggregation level of the 
        downloaded data records, \code{d} for daily, \code{w} for
        weekly (not yet implemented), and \code{m} for monthly data 
        records.
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
        [googleSeries] - \cr
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
        a character string value or vector, the Google symbol name(s).
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

    The function \code{googleImport} returns an S4 object of class 
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
    
}


\details{
  
    \bold{Import data from finance.google.com:}
    \cr\cr
    The query string is given as 
    \cr\cr
            \code{s=SYMBOL&a=DD&b=MM&c=CCYY&g=d&q=q&z=SYMBOL&x=.csv} 
            \code{q=SYMBOL&startdate=MON+DD+YYYY&enddate=MON+DD+YYYY&output=csv}
    \cr\cr
    where \code{SYMBOL} has to replaced by the symbol name of the 
    instrument, and \code{DD}, \code{MM}, and \code{YYYY} by the 
    day, month-1 and century/year when the time series should start.

    Here are some examples of symbols:

    \tabular{ll}{
    \code{[query]}  \tab Description: \cr
                    \tab \cr
    \code{"IBM"}    \tab IBM DJIA Stock \cr
    \code{"MSFT"}   \tab Microsoft DJIA Stock }

    The meaning of the tokens in the query string are the following:

    \tabular{ll}{
    Token            \tab Description  \cr
                     \tab \cr
    \code{q}         \tab Selected Ticker-Symbol \cr
    \code{startdate} \tab Starting Date \cr
    \code{enddate}   \tab End date }
            
}


\note{
  
    \bold{Internet Download Functions:}
    \cr\cr
    IMPORTANT NOTE: If the service provider changes the data file format 
    it may become necessary to modify and update the functions. 
  
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
\dontrun{
## googleImport -
   googleSeries("IBM")     
}
}


\keyword{data}

