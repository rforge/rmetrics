\name{TimeDateCoercion}

\alias{TimeDateCoercion}

\alias{as.timeDate}
\alias{as.timeDate.default}
\alias{as.timeDate.POSIXt}
\alias{as.timeDate.Date}

\alias{as.character.timeDate}
\alias{as.double.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}
\alias{as.POSIXlt.timeDate}
\alias{as.Date.timeDate}

%\alias{julian.POSIXt}

\alias{julian.timeDate}
\alias{atoms.timeDate}
\alias{months.timeDate}


\title{timeDate Class, Coercion and Transformation}


\description{
    
    A collection and description of functions and 
    methods for coercion and transformation of 
    objects of class 'timeDate'.
    \cr
    
    The functions are:
    
    \tabular{ll}{
    \code{as.timeDate.POSIXt} \tab Returns a 'POSIX' object as 'timeDate' object, \cr
    \code{as.timeDate.Date} \tab Returns a 'POSIX' object as 'timeDate' object, \cr
    \code{as.character.timeDate} \tab Returns a 'timeDate' object as character string, \cr
    \code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as data frame, \cr
    \code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as POSIXct object, \cr
    \code{julian.timeDate} \tab Returns Julian day counts since 1970-01-01, \cr
    \code{atoms.timeDate} \tab Returns date/time atoms from a 'timeDate' object, \cr
    \code{months.timeDate} \tab Extract months atom from a 'timeDate' object. } 
       
}


\usage{
\method{as.timeDate}{default}(x, zone = myFinCenter, FinCenter = myFinCenter)
\method{as.timeDate}{POSIXt}(x, zone = myFinCenter, FinCenter = myFinCenter)
\method{as.timeDate}{Date}(x, zone = myFinCenter, FinCenter = myFinCenter) 

\method{as.character}{timeDate}(x, \dots)
\method{as.double}{timeDate}(x, units = c("auto", "secs", "mins", "hours", "days", "weeks"), ...)
\method{as.data.frame}{timeDate}(x, \dots)
\method{as.POSIXct}{timeDate}(x, tz = "")
\method{as.POSIXlt}{timeDate}(x, tz = "")
\method{as.Date}{timeDate}(x, method = c("trunc", "round", "next"), \dots)

\method{julian}{timeDate}(x, origin = timeDate("1970-01-01"), units = c("auto", 
    "secs", "mins", "hours", "days", "weeks"), zone = NULL, FinCenter = NULL, \dots)
\method{atoms}{timeDate}(x, \dots)
\method{months}{timeDate}(x, abbreviate = NULL)
}


\arguments{
  
    \item{abbreviate}{
        [months] - \cr
        currently not used.
        }
    \item{tz}{
        inputs the time zone to POSIX objects, i.e. the time zone, 
        \code{zone}, or financial center string, \code{FinCenter}, 
        as used by \code{timeDate} objects.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
    \item{method}{
        a character string denoting the method how to determine the
        dates.
        }
    \item{origin}{
        a length-one object inheriting from class \code{"timeDate"}
        setting the origin for the julian counter.
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
  
    \code{as.character}\cr
    \code{as.data.frame}
    \cr
    return a \code{timeDate} object trnasformed into a character or a
    data frame formatted object.
    \cr
       
    \code{as.POSIXct}
    \cr
    return a \code{timeDate} object transformed into a POSIX type
    formatted object.
    \cr
    
    \code{julian}
    \cr
    return a \code{timeDate} object as a Julian count.
    \cr
    
    \code{atoms}\cr
    \code{months}
    \cr
    extrac from a \code{timeDate} object the calendar atoms, i.e,
    the year, month, day, and optionally hour, minute and second.
    
}


\seealso{

    \code{timeDateClass}
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{
## as.character 
   tD = timeDate()
   as.character(tD)
   
## as.data.frame -
   as.data.frame(tD)
   
## as.POSIXct -  
   as.POSIXct(tD)

## julian -
   tC = timeCalendar()
   julian(tC)[1:3]
   
## atoms -
   atoms(tC)
   
## months -
   months(tC)
}


\keyword{chron}

