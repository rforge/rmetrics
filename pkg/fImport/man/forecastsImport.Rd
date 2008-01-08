\name{Forecasts}

\alias{Forecasts}

\alias{forecastsImport}
\alias{forecastsSeries}


\title{Import Market Data from www.forecasts.org}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from the
    www.forecast.org web site.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{forecastsImport} \tab monthly data from the Financial Forecast Center, \cr 
    \code{forecastsSeries} \tab easy to use download from Financial Forecast Center.}
   
}


\usage{
forecastsImport(query, file = "tempfile", 
    source = "http://www.forecasts.org/data/data/", save = FALSE, 
    try = TRUE) 
      
forecastsSeries(query, returnClass = c("timeSeries", "ts", "matrix", 
    "data.frame"), getReturns = FALSE, \dots)    
    
}


\arguments{

    \item{getReturns}{
        a logical flag. Should return values be computed using the 
        function \code{returnSeries}?
        }
    \item{file}{
        a character string with filename, usually
        having extension ".csv", where to save the downloaded data.
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

    The function \code{forecastsImport} returns an S4 object of class 
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
        
    The function \code{forecastsSeries} returns an S4 object of 
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
## forecastsImport -
   NA
}


\keyword{data}

