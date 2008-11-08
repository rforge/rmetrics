\name{as}

\alias{Any to 'timeDate' Coercion}

\alias{as.character.timeDate}
\alias{as.double.timeDate}
\alias{as.list.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
% \alias{as.POSIXlt,timeDate-method}
\alias{as.POSIXlt.timeDate}
\alias{as.Date.timeDate}



\title{Any to 'timeDate' Coercion}


\description{
  
  Coerce other time date representations into an object
  of class 'timeDate'.
  
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
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
    }
    \item{units}{
        a character string denoting the date/time units in which the 
        results are desired.
    }
    \item{tz}{
        inputs the time zone to POSIX objects, i.e. the time zone, 
        \code{zone}, or financial center string, \code{FinCenter}, 
        as used by \code{timeDate} objects.
    }    
    \item{method}{
        a character string denoting the method how to determine the
        dates.
    }    
    \item{\dots}{
        arguments passed to other methods.
  }
  
}


\value{
  
  return an object of class \code{timeDate}.
  
}


\examples{
## as.character -
   # Convert 'timeDate' to a character strings:
   as.character(timeCalendar())
}


\keyword{chron}

