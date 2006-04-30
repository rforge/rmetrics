\name{TimeDateCoercion}

\alias{TimeDateCoercion}

\alias{as.character.timeDate}
\alias{as.data.frame.timeDate}
\alias{as.POSIXct.timeDate}

\alias{julian.POSIXt}
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
    \code{as.character.timeDate} \tab Returns a 'timeDate' object as character string, \cr
    \code{as.data.frame.timeDate} \tab Returns a 'timeDate' object as data frame, \cr
    \code{as.POSIXct.timeDate} \tab Returns a 'timeDate' object as POSIXct object, \cr
    \code{julian.timeDate} \tab Returns Julian day counts since 1970-01-01, \cr
    \code{atoms.timeDate} \tab Returns date/time atoms from a 'timeDate' object, \cr
    \code{months.timeDate} \tab Extract months atom from a 'timeDate' object. }    
}


\usage{
\method{as.character}{timeDate}(x, \dots)
\method{as.data.frame}{timeDate}(x, \dots)
\method{as.POSIXct}{timeDate}(x, tz = "")

\method{julian}{timeDate}(x, FinCenter = myFinCenter, \dots)
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
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
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


\examples{
## SOURCE("fCalendar.023A-TimeDateCoercion")

   tD = timeDate()
   as.character(tD)
   as.data.frame(tD)
   as.POSIXct(tD)

   tC = timeCalendar()
   julian(tC)[1:3]
   atoms(tC)
   months(tC)

}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

