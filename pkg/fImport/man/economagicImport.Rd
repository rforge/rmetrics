\name{Economagic}

\alias{Economagic}

\alias{economagicImport}
\alias{economagicSeries}


\title{Import Market Data from Economagic}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from Economagic.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{economagicImport} \tab Economic series from Economagic's Web site, \cr
    \code{economagicSeries} \tab easy to use download from Economagic.}
   
}


\usage{
economagicImport(query, file = "tempfile", 
    source = "http://www.economagic.com/em-cgi/data.exe/",  
    frequency = c("quarterly", "monthly", "daily"), save = FALSE, 
    colname = "VALUE", try = TRUE)  

economagicSeries(query, frequency = c("quarterly", "monthly", "daily"),
    returnClass = c("timeSeries", "ts", "matrix", "data.frame"), 
    getReturns = FALSE, \dots)   
}


\arguments{

    \item{getReturns}{
        a logical flag. Should return values be computed using the 
        function \code{returnSeries}?
        }
    \item{colname}{
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
    \item{query}{
        a character string, denoting the location of the data at the 
        web site.
        }
    \item{returnClass}{
        a character string naming the class of the object to be 
        returned. By default the function returns 
        a \code{"timeSeries"} object, alternatives are: \code{"ts"}, 
        \code{"matrix"}, or \code{"data.frame"}.
        }
    \item{save}{
        a logical value, if set to TRUE the downloaded data file will 
        be stored under the path and file name specified by the
        string \code{file}. By default FALSE.
        }
    \item{source}{
        a character string with the download URL.
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
    class  \code{timeSeries} or alternatively an object specified by
    the function argument \code{returnClass}.
    \cr
    
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
## economagicSeries -
   # USDEUR Foreign Exchange Rate:
   economagicSeries("fedny/day-fxus2eu", frequency = "daily")
   # USFEDFUNDS US FedFunds Rate:
   economagicSeries("fedstl/fedfunds+2", frequency = "monthly")
}
}

\keyword{data}

