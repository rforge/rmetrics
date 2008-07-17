\name{isBizday}


\alias{isBizday}
\alias{isHoliday}


\title{Business and Holidays}


\description{
    
    Tests if a date is a business day or not.

}


\usage{
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
    business day, or a holiday. 

}


\examples{  
## Dates in April, currentYear: 
   tS = timeSequence(
      from = paste(currentYear, "-03-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS
      
## Subset Business Days at NYSE:
   holidayNYSE()
   isBizday(tS, holidayNYSE())
   tS[isBizday(tS, holidayNYSE())]
}


\keyword{chron}

