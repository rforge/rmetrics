\name{Economagic}


\alias{economagicSeries}
\alias{economagicImport}


\title{Import Market Data from www.economagic.com}


\description{
    
    Imports financial time series data from www.economagic.com.
   
}


\usage{
economagicSeries(symbols, from = NULL, to = Sys.timeDate(), 
    nDaysBack = 366, \dots)
    
economagicImport(query, file = "tempfile", source = NULL,
    frequency = c("auto", "quarterly", "monthly", "daily"),
    from = NULL, to = Sys.timeDate(), nDaysBack = NULL,
    save = FALSE, sep =";", try = TRUE)
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

    The function \code{economagicImport} returns an S4 object of class 
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
        
    The function \code{economagicSeries} returns an S4 object of 
    class  \code{timeSeries}.
    
}


\details{

    \bold{Import data from www.economagic.com}
    \cr\cr
    Frequently requested data files from Economagic for the US economy include:

    \tabular{ll}{
    \code{[query]}                   \tab Description: \cr
                                     \tab \cr
    \code{var/leading-ind-long}      \tab Index of Leading Economic Indicators  \cr
    \code{beana/t102l01}             \tab Real Gross Domestic Product  \cr
    \code{fedstl/trsp500}            \tab SP 500 Total Return \cr
    \code{fedstl/gnp}                \tab Gross National Product in Current Dollars \cr
    \code{var/cpiu-long}             \tab Consumer Price Index - All Urban Consumers \cr
    \code{feddal/ru}                 \tab Unemployment Rate \cr
    \code{fedstl/indpro}             \tab Total Industrial Production Index \cr
    \code{fedstl/exjpus+2}           \tab FX Rate: Japanese Yen to one US Dollar \cr
    \code{fedstl/fedfunds+2}         \tab Federal Funds Rate \cr
    \code{fedstl/mdiscrt+2}          \tab Discount Rate \cr
    \code{fedbog/tcm30y+2}           \tab 30-Year Treasury Constant Maturity Rate \cr
    \code{fedstl/mprime+2}           \tab Bank Prime Loan Rate \cr
    \code{fedstl/tb3ms+2}            \tab 3-Month Treasury Bills - Secondary Market \cr
    \code{fedstl/tb6ms+2}            \tab 6-Month Treasury Bills - Secondary Market \cr
    \code{fedbog/cm+2}               \tab 30 Year Federal Home Loan Mortgages \cr
    \code{var/west-texas-crude-long} \tab Price of West Texas Intermediate Crude }
      
}


\note{
  
    \bold{Internet Download Functions:}

    IMPORTANT NOTE: If the service provider changes the data file format 
    the download may fail and then it becomes necessary to modify and 
    update the functions. 
  
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
}


\examples{
\dontrun{
## economagicSeries -
   
   # USDEUR Foreign Exchange Rate:
   economagicSeries("fedny/day-fxus2eu", frequency = "daily")
   
   # USFEDFUNDS US FedFunds Rate:
   economagicSeries("fedstl/fedfunds+2", frequency = "monthly")
}
}


\keyword{data}

