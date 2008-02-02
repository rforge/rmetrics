\name{isWeekday}


\alias{isWeekday}
\alias{isWeekend}
\alias{isBizday}
\alias{isHoliday}


\title{Weekends and Holidays}


\description{
    
    Tests if a day belongs to the weekend or is a
    holiday.
    
    \tabular{ll}{
    \code{isWeekday} \tab Tests if a date is a weekday or not, \cr
    \code{isWeekend} \tab Tests if a date falls on a weekend or not, \cr
    \code{isBizday} \tab Tests if a date is a business day or not, \cr
    \code{isHoliday} \tab Tests if a date is a non-business day or not. }

}


\usage{
isWeekday(x)
isWeekend(x)

isBizday(x, holidays = holidayNYSE())
isHoliday(x, holidays = holidayNYSE())
}


\arguments{
  
   
    \item{holidays}{
        holiday dates from a holiday calendar. An object of class
        \code{timeDate}.
        }
    \item{x}{
        an object of class \code{timeDate}.
        }  
       
}


\value{

    the functions return logical vectors indicating if a date is a 
    business day, or a holiday. Note, that for business and holidays 
    extraction an holiday/business calendar has to be specified. 

}


\seealso{

    \code{timeDate}.
    
    We also recommend to inspect the help pages for the POSIX time and date 
    class, \code{?Dates}, and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\examples{  
## Dates in April, currentYear: 
   tS = timeSequence(
      from = paste(currentYear, "-03-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS
    
## Subset of Weekends:
   isWeekend(tS)
   tS[isWeekend(tS)]
   
## Subset Business Days at NYSE:
   holidayNYSE()
   isBizday(tS, holidayNYSE())
   tS[isBizday(tS, holidayNYSE())]
}


\keyword{chron}

