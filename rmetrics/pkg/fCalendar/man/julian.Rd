\name{julian}


\title{Julian Counts and Calendar Atoms}


\alias{julian.timeDate}
\alias{atoms.timeDate}
\alias{months.timeDate}


\description{
    
    Returns Julian day counts, date/time atoms from a 'timeDate' object, 
    and extracts month atoms from a 'timeDate' object.  
       
}


\usage{
\method{julian}{timeDate}(x, origin = timeDate("1970-01-01"), 
    units = c("auto", "secs", "mins", "hours", "days", "weeks"), 
    zone = NULL, FinCenter = NULL, \dots)

\method{atoms}{timeDate}(x, \dots)

\method{months}{timeDate}(x, abbreviate = NULL)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{origin}{
        a length-one object inheriting from class \code{"timeDate"}
        setting the origin for the julian counter.
        }
    \item{units}{
        a character string denoting the date/time units in which the 
        results are desired.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }   
    \item{abbreviate}{
        currently not used.
        }   
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
  
    \code{julian} 
    returns a \code{timeDate} object as a Julian count.
    
    \code{atoms} and \code{months}
    extrac from a \code{timeDate} object the calendar atoms, i.e.
    the year, month, day, and optionally hour, minute and second.
    
}



\examples{
## julian -
   tC = timeCalendar()
   julian(tC)[1:3]
   
## atoms -
   atoms(tC)
   
## months -
   months(tC)
}


\keyword{chron}

