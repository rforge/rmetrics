\name{as.timeDate}


\title{Coercion into 'timeDate' Objects}


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
\method{as.timeDate}{default}(x, zone = "", FinCenter = "")  

\method{as.timeDate}{POSIXt}(x, zone = "", FinCenter = "")

\method{as.timeDate}{Date}(x, zone = "", FinCenter = "")
\method{as.timeDate}{timeDate}(x, zone = x@FinCenter, FinCenter = "")  
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
    }
    \item{zone}{
        the time zone or financial center where the data were recorded.
    }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
    }
  
}


\value{
  
  \code{as.timeDate.POSIXt} returns an object of class \code{timeDate}.
  
  \code{as.timeDate.Date} returns an object of class \code{timeDate}.
  
}


\examples{
## timeDate - 
   tC = timeCalendar()
   
## Coerce a 'Date' object into a 'timeDate' object:
   as.timeDate(Sys.Date())
}


\keyword{chron}

