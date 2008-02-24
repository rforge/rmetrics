\name{firstDay}


\alias{firstDay}


\alias{timeFirstDayInMonth}
\alias{timeLastDayInMonth}

\alias{timeFirstDayInQuarter}
\alias{timeLastDayInQuarter}


\title{First and Last Days}


\description{
    
    Computes the first/last day in a given month/quarter.
    
}


\usage{
timeFirstDayInMonth(charvec, format = "\%Y-\%m-\%d", zone = myFinCenter, 
    FinCenter = myFinCenter)
timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", zone = myFinCenter, 
    FinCenter = myFinCenter)

timeFirstDayInQuarter(charvec, format = "\%Y-\%m-\%d", zone = myFinCenter, 
    FinCenter = myFinCenter)
timeLastDayInQuarter(charvec, format = "\%Y-\%m-\%d", zone = myFinCenter, 
    FinCenter = myFinCenter)
}


\arguments{
  
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{format}{
        the format specification of the input character vector.
        }
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
   
}


\details{

    We have implemented functions to generate special \code{"timeDate"} 
    sequences. These are functions to compute the last day in a given 
    month and year.
    
    See also the functions to compute the dates in a month that is a 
    n-day on or after a given date, to compute the dates in a month that 
    is a n-day on or before a specified date, to compute the n-th ocurrances 
    of a n-day for a specified year/month vectors, and finally to compute 
    the last n-day for a specified year/month value or vector. n-days
    are numbered from 0 to 6 where 0 correspond to the Sunday and 6 to 
    the Saturday.

}


\value{

    For the functions \code{timeLastDayInMonth} and \code{timeLastDayInMonth}
    return the last or first day respectively in a given month and year.
    
    The same functionality for quarterly time horizons is returned by the
    functions \code{timeLastDayInQuarter} and \code{timeLastDayInQuarter}.
   
    The returned values are objects of class \code{timeDate}.
    
}


\examples{
## Date as character String:
   charvec = "2006-04-16"
    
## timeLastDayInMonth-
   # What date has the last day in a month for a given date ?
   timeLastDayInMonth(charvec, format = "\%Y-\%m-\%d", 
     zone = myFinCenter, FinCenter = myFinCenter)
   timeLastDayInMonth(charvec)
   timeLastDayInMonth(charvec, FinCenter = "Zurich")
   
## timeFirstDayInMonth -
   # What date has the first day in a month for a given date ?
   timeFirstDayInMonth(charvec)
    
## timeLastDayInQuarter -
   # What date has the last day in a quarter for a given date ?
   timeLastDayInQuarter(charvec)
    
## timeFirstDayInQuarter -
   # What date has the first day in a quarter for a given date ?
   timeFirstDayInQuarter(charvec)
    
## timeNdayOnOrAfter
   # What date has the first Monday on or after March 15, 1986 ?
   timeNdayOnOrAfter("1986-03-15", 1)
    
## timeNdayOnOrBefore
   # What date has Friday on or before April 22, 1977 ?
   timeNdayOnOrBefore("1986-03-15", 5)
    
## timeNthNdayInMonth -
   # What date is the second Monday in April 2004 ?
   timeNthNdayInMonth("2004-04-01", 1, 2)
    
## timeLastNdayInMonth -
   # What date has the last Tuesday in May, 1996 ?
   timeLastNdayInMonth("1996-05-01", 2)
}


\keyword{chron}

