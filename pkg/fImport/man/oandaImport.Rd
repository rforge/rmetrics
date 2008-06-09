\name{Oanda}

\alias{Oanda}


\alias{oandaImport}
\alias{oandaSeries}


\title{Import FX Market Data from OANDA}


\description{
    
    Imports FX market data from www.oanda.com.
   
}


\usage{
oandaSeries(symbols, from = NULL, to = Sys.timeDate(), 
    nDaysBack = 366, \dots) 
    
oandaImport(query, file = "tempfile", source = NULL, frequency = "daily", 
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
    \item{from} {
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
    \item{to} {
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

    The function \code{fredImport} returns an S4 object of class 
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
        
    The function \code{fredSeries} returns an S4 object of 
    class  \code{timeSeries} or alternatively an object specified by
    the function argument \code{returnClass}.
    \cr
    
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
## oandaImport -
   oandaSeries("USD/EUR")      
}
}

\keyword{data}

