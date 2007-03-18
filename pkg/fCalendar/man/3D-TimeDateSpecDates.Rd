\name{TimeDateSpecDates}

\alias{TimeDateSpecDates}


\alias{timeLastDayInMonth}
\alias{timeFirstDayInMonth}

\alias{timeLastDayInQuarter}
\alias{timeFirstDayInQuarter}

\alias{timeNdayOnOrAfter}
\alias{timeNdayOnOrBefore}
\alias{timeNthNdayInMonth}
\alias{timeLastNdayInMonth}


\title{timeDate Class, Special Date Functions}


\description{
    
    A collection and description of functions for the 
    generation of sequences of dates according to 
    special date rules.
    \cr
    
    
    The functions for special 'timeDate' sequences are:
    
    \tabular{ll}{
    \code{timeFirstDayInMonth} \tab Computes the first day in a given month and year, \cr
    \code{timeLastDayInMonth} \tab Computes the last day in a given month and year, \cr
    \code{timeFirstDayInQuarter} \tab Computes the first day in a given quarter and year, \cr
    \code{timeLastDayInQuarter} \tab Computes the last day in a given quarter and year, \cr
    \code{timeNdayOnOrAfter} \tab Computes date that is a "on-or-after" n-day, \cr  
    \code{timeNdayOnOrBefore} \tab Computes date that is a "on-or-before" n-day, \cr
    \code{timeNthNdayInMonth} \tab Computes n-th ocurrance of a n-day in year/month, \cr
    \code{timeLastNdayInMonth} \tab Computes the last n-day in year/month.}
    
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

timeNdayOnOrAfter(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    zone = myFinCenter, FinCenter = myFinCenter)
timeNdayOnOrBefore(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    zone = myFinCenter, FinCenter = myFinCenter)

timeNthNdayInMonth(charvec, nday = 1, nth = 1, format = "\%Y-\%m-\%d", 
    zone = myFinCenter, FinCenter = myFinCenter)
timeLastNdayInMonth(charvec, nday = 1, format = "\%Y-\%m-\%d", 
    zone = myFinCenter, FinCenter = myFinCenter)
}


\arguments{
  
    \item{charvec}{
        a character vector of dates and times.
        }
    \item{FinCenter}{
        a character with the the location of the  
        financial center named as "continent/city". 
        }
    \item{format}{
        the format specification of the input character vector.
        }
    \item{nday}{
        an integer vector with entries ranging from 
        \code{0} (Sunday) to \code{6} (Saturday).
        }
    \item{nth}{
        an integer vector numbering the n-th occurence.
        } 
    \item{zone}{
        the time zone or financial center where the data were recorded.
        }      
       
}


\value{

    \code{timeLastDayInMonth}\cr
    \code{timeFirstDayInMonth}\cr
    \code{timeLastDayInQuarter}\cr
    \code{timeFirstDayInQuarter}\cr
    \code{timeNdayOnOrAfter}\cr
    \code{timeNdayOnOrBefore}\cr
    \code{timeNthNdayInMonth}\cr
    \code{timeLastNdayInMonth}
    \cr
    these functions return \code{timeDate} objects on special dates. For 
    the functions \code{timeLastDayInMonth} and \code{timeLastDayInMonth}
    return the last or first day respectively in a given month and year.
    The same functionality for quarterly time horizons is returned by the
    functions \code{timeLastDayInQuarter} and \code{timeLastDayInQuarter}.
    Ror the function \code{timeNdayOnOrAfter} the date in the specified 
    month that is a n-day (e.g. Sun-day) on or after a given date, for 
    \code{timeNdayOnOrBefore} the date that is a n-day on or before a 
    given date, for \code{timeNthNdayInMonth} the \code{nth} ocurrance 
    of a n-day (nth = 1,...,5) in \code{year}, \code{month}, and
    for {timeLastNdayInMonth} the last \code{nday} in \code{year},
    \code{month} will be returned as \code{"timeDate"} objects.
    
}


\details{

    We have implemented functions to generate special \code{"timeDate"} 
    sequences. These are functions to compute the last day in a given 
    month and year, to compute the dates in a month that is a n-day on or 
    after a given date, to compute the dates in a month that is a n-day 
    on or before a specified date, to compute the n-th ocurrances of a 
    n-day for a specified year/month vectors, and finally to compute 
    the last n-day for a specified year/month value or vector. n-days
    are numbered from 0 to 6 where 0 correspond to the Sunday and 6 to 
    the Saturday.
    \cr

}


\seealso{

    \code{timeDate}
    
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


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\keyword{chron}

