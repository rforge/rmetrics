\name{holidayCalendar}

\alias{holidayCalendar}

\alias{holidayNYSE}
\alias{holidayZURICH}
\alias{holidayTSX}


\title{Holiday Calendars}


\description{
    
    Returns a holiday calendar.     
                
}


\usage{
holidayNYSE(year = currentYear)
holidayZURICH(year = currentYear)
holidayTSX(year = currentYear)
}


\arguments{

    \item{year}{
        an integer value or vector of years, formatted as \code{YYYY}.
        }
        
}


\value{

    \code{holidayNYSE}\cr
    \code{holidayZURICH}\cr
    \code{holidayTSX}
    \cr
    return an holiday calendar as an object of class \code{timeDate}. 

}


\details{

    \bold{Holiday Calendars:}
    \cr\cr
    Two examples are implemented, the holiday calendar for the
    New York Stock Exchange, and the public holidays for Zurich.
    Have a look on the code to write your own holiday calendars.

}


\examples{
## holidayNYSE -
   holidayNYSE(currentYear)
   
## holidayZURICH =
   holidayZURICH = function(year = currentYear) {
     holidays = c(
        NewYearsDay(year),
        GoodFriday(year),   
        EasterMonday(year), 
        LaborDay(year),
        PentecostMonday(year),  
        ChristmasDay(year),
        BoxingDay(year),
        CHBerchtoldsDay(year),
        CHSechselaeuten(year),
        CHAscension(year),
        CHConfederationDay(year),
        CHKnabenschiessen(year) )
     # Sort and Remove Weekends: 
     holidays = sort(holidays)
     holidays = holidays[isWeekday(holidays)]
     # Add Financial Center:
     holidays@FinCenter = "Zurich"
     # Return Value:
     holidays }

   holidayZURICH(2007:2009)
}

\keyword{chron}

