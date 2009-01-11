\name{Yahoo}

\alias{Yahoo}

\alias{yahooImport}
\alias{yahooSeries}


\title{Import Market Data from Yahoo}


\description{
    
    Imports financial time series data from chart.yahoo.com.
   
}


\usage{ 
yahooSeries(symbols, from = NULL, to = Sys.timeDate(), 
    nDaysBack = 366, \dots) 
    
yahooImport(query, file = "tempfile", source = NULL,
    frequency = c("daily", "weekly", "monthly"), 
    from = NULL, to = Sys.timeDate(), nDaysBack = 366, 
    save = FALSE, sep = ";", try = TRUE) 
}


\arguments{

    \item{file}{
        a character string with filename, usually having extension 
        ".csv", where to save the downloaded data.
        }
    \item{frequency}{
        a character string, one of "auto", "quarterly", "monthly", 
        or "daily", defining the frequency of the data records.
        Note, the import function tries autodetect the frequency
        of the time series to be dowwnloaded. This may fail, in
        such case specify the frequency explicitely.
        }
    \item{from}{
        the date from when to extract the time series.
        }
    \item{nDaysBack}{
        the number of days back.
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
    \item{sep}{
        a charcter value specifying the column separator.
        }
    \item{source}{a character string setting the URL of the source.
        If \code{NULL}, then the URL will be set automatically to its
        default value.
        }
    \item{symbols} {
        a character string with the symbols to be downloaded.
        }
    \item{to}{
        the end date of the data download, by default the current 
        date.
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
    
    IMPORTANT NOTE: If the service provider changes the data file format 
    it may become necessary to modify and update the functions. 
     
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
\dontrun{
## yahooImport -
   yahooSeries("IBM")
}
}


\keyword{data}

