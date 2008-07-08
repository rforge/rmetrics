\name{weekdays}


\title{Extract Parts of a 'timeDate' Object}


\alias{weekdays.timeDate}
\alias{months.timeDate}
\alias{quarters.timeDate}
\alias{julian.timeDate}


\alias{atoms.timeDate}


\description{
    
    Extract the weekday, month or quarter, or the Julian 
    time (days since some origin). 
       
}


\usage{
\method{weekdays}{timeDate}(x, abbreviate = FALSE) 
\method{months}{timeDate}(x, abbreviate = FALSE) 
\method{quarters}{timeDate}(x, \dots) 
\method{julian}{timeDate}(x, 
   origin = timeDate("1970-01-01", zone = "GMT", FinCenter = "GMT"), \dots) 

\method{atoms}{timeDate}(x, \dots)
}


\arguments{
  
    \item{x}{
        an object of class \code{timeDate}.
        }
    \item{abbreviate}{
        a logical value. Should the names be abbreviated?
        }
    \item{origin}{
        an length-one object of class \code{timeDate}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{
  
    \code{weekdays} and \code{months} return a character vector of 
    names in the locale in use. 

    \code{quarters} returns a character vector of \code{"Q1"} to 
    \code{"Q4"}. 

    \code{julian} returns the number of days (possibly fractional) 
    since the origin, with the origin as a \code{origin} attribute.

    \code{atoms}
    extracs from a \code{timeDate} object the calendar atoms, i.e.
    the year, month, day, and optionally hour, minute and second.
    
}



\examples{
## timeDate -
   # Create Dates:
   charvec = "2008-03-31 22:00:00"
   print(charvec)
   NYC = timeDate(charvec, zone = "NewYork", FinCenter = "NewYork")
   ZRH = timeDate(charvec, zone = "NewYork", FinCenter = "Zurich")
    
   NYC
   weekdays(NYC, TRUE)
   months(NYC, TRUE)
   quarters(NYC)
   julian(NYC)
   
   ZRH
   weekdays(ZRH, TRUE)
   months(ZRH, TRUE)
   quarters(ZRH)
   julian(ZRH)

   julian(ZRH)-julian(NYC)
}


\keyword{chron}

