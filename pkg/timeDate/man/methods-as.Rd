\name{as.timeDate}

\title{Any to 'timeDate' Coercion}

\alias{Any to 'timeDate' Coercion}
\alias{as.character.timeDate}
\alias{as.double.timeDate}
\alias{as.list.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
\alias{as.POSIXlt.timeDate}
\alias{as.Date.timeDate}
\alias{as.timeDate}
\alias{as.timeDate.default}
\alias{as.timeDate.POSIXt}
\alias{as.timeDate.Date}
\alias{as.timeDate.timeDate}
\alias{coerce,ANY,timeDate-method}
\alias{coerce,Date,timeDate-method}
\alias{coerce,POSIXt,timeDate-method}
\alias{coerce,timeDate,character-method}
\alias{coerce,timeDate,numeric-method}
\alias{coerce,timeDate,data.frame-method}
\alias{coerce,timeDate,list-method}
\alias{coerce,timeDate,POSIXct-method}
\alias{coerce,timeDate,POSIXlt-method}
\alias{coerce,timeDate,Date-method}

\description{
  
  Coerce and transform objects of class 'timeDate'.
  
}


\usage{
\method{as.character}{timeDate}(x, \dots)

\method{as.double}{timeDate}(x, 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), ...)
\method{as.data.frame}{timeDate}(x, \dots)

\method{as.POSIXct}{timeDate}(x, tz = "", \dots)

% \S4method{as.POSIXlt}{timeDate}(x, tz = "", \dots)
\method{as.POSIXlt}{timeDate}(x, tz = "", \dots)

\method{as.Date}{timeDate}(x, method = c("trunc", "round", "next"), \dots)

\method{as.timeDate}{default}(x, zone = "", FinCenter = "")  

\method{as.timeDate}{POSIXt}(x, zone = "", FinCenter = "")

\method{as.timeDate}{Date}(x, zone = "", FinCenter = "")
\method{as.timeDate}{timeDate}(x, zone = x@FinCenter, FinCenter = "")  
}


\arguments{
  \item{FinCenter}{
    a character with the the location of the  
    financial center named as "continent/city". 
  }

  \item{method}{
    a character string denoting the method how to determine the
    dates.
  }    
  
  \item{tz}{
    inputs the time zone to POSIX objects, i.e. the time zone, 
    \code{zone}, or financial center string, \code{FinCenter}, 
    as used by \code{timeDate} objects.
  }

  \item{units}{
    a character string denoting the date/time units in which the 
    results are desired.
  }

  \item{x}{
    an object of class \code{timeDate}.
  }

  \item{zone}{
    the time zone or financial center where the data were recorded.
  }

  \item{\dots}{
    arguments passed to other methods.
  }

}


\value{
  
  \code{as.timeDate.POSIXt} returns an object of class \code{timeDate}.
  
  \code{as.timeDate.Date} returns an object of class \code{timeDate}.
  
}


\examples{
## timeDate - 
   tC = timeCalendar()

## Convert 'timeDate' to a character strings:
   as.character(tC)
   
## Coerce a 'Date' object into a 'timeDate' object:
   as.timeDate(Sys.Date())
}


\keyword{chron}

