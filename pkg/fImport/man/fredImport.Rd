\name{Fred}

\alias{Fred}


\alias{fredImport}
\alias{fredSeries}


\title{Import Market Data from the Fred}


\description{
    
    A collection and description of functions to import 
    financial and economic market data from the Fred
    Website.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{fredImport} \tab time series from St. Louis FRED Web site, \cr
    \code{fredSeries} \tab easy to use download from St. Louis FRED}
   
}


\usage{
fredImport(query, file = "tempfile", 
    source = "http://research.stlouisfed.org/fred2/series/", 
    frequency = "daily", save = FALSE, sep = ";", try = TRUE) 
  
fredSeries(query = "DPRIME", frequency = "daily", returnClass = 
    c("timeSeries", "ts", "matrix", "data.frame"), getReturns = FALSE, \dots)    
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
    \item{sep}{
        a character value, defining the field separator for the 
        destination file which saves the downloaded data records.
        By default a semicolon.
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
## fredImport -
   DPRIME = fredImport("DPRIME")
   if (!is.null(DPRIME)) print(DPRIME@data[1:20, ])       
}


\keyword{data}

