\name{timeCalendar}


\title{'timeDate' from Calendar Atoms}


\alias{timeCalendar}


\description{

    Create a 'timeDate' object from calendar atoms.

}


\usage{
timeCalendar(y = currentYear, m = 1:12, d = 1,
    h = 0, min = 0, s = 0,
    zone = myFinCenter, FinCenter = myFinCenter)
}


\arguments{

    \item{y, m, d}{
        calendar years (e.g. 1997), defaults are 1960,
        calendar months (1-12), defaults are 1, and
        calendar days (1-31), defaults are 1, \cr
        }
    \item{h, min, s}{
        hours of the days (0-23), defaults are 0,
        minutes of the days (0-59), defaults are 0, and
        seconds of the days (0-59), defaults are 0.
        }    
    \item{zone}{
        a character string, denoting the time zone or financial 
        center where the data were recorded.
        }
    \item{FinCenter}{
        a character with the the location of the
        financial center named as "continent/city".
        }

}


\value{

    returns a S4 object of \code{\link{class}} \code{"timeDate"}.

}


\examples{
## timeCalendar -
   
   # Current Year:
   currentYear
   
   # 12 months of current year
   timeCalendar() 
   
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9),
                y = c(1989, 2001, 2004, 1990), FinCenter = "GMT")
   
   timeCalendar(m = c(9, 1, 8, 2), d = c(28, 15, 30, 9),
                y = c(1989, 2001, 2004, 1990), FinCenter = "Europe/Zurich")
                
   timeCalendar(h = c(9, 14), min = c(15, 23))
}


\keyword{chron}

