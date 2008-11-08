\name{as.timeDate}


\title{Coercion into 'timeDate' Objects}


\alias{as.timeDate}
\alias{as.timeDate.default}
\alias{as.timeDate.POSIXt}
\alias{as.timeDate.Date}
\alias{as.timeDate.timeDate}

\description{
  
  Coerce and transform objects of class 'timeDate'.
  
}


\usage{
\method{as.timeDate}{default}(x, zone = myFinCenter, FinCenter =
myFinCenter)  

\method{as.timeDate}{POSIXt}(x, zone = myFinCenter, FinCenter =
myFinCenter)

\method{as.timeDate}{Date}(x, zone = myFinCenter, FinCenter =
myFinCenter)
\method{as.timeDate}{timeDate}(x, zone = x@FinCenter, FinCenter =
myFinCenter)  
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

