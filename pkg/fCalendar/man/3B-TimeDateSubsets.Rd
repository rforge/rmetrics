\name{TimeDateSubsets}

\alias{TimeDateSubsets}

\alias{isWeekday}
\alias{isWeekend}

\alias{isBizday}
\alias{isHoliday}

\alias{getDayOfWeek}
\alias{getDayOfYear}

\alias{length.timeDate}

\alias{[.timeDate}
\alias{window.timeDate}
\alias{cut.timeDate}
\alias{start.timeDate}
\alias{end.timeDate}

\alias{blockStart}
\alias{blockEnd}


\title{timeDate Class, Subsetting}


\description{
    
    A collection and description of functions and methods
    for subsetting 'timeDate' objects.
    \cr
   
    Functions for testing 'timeDate' objects:
    
    \tabular{ll}{
    \code{isWeekday} \tab Tests if a date is a weekday or not, \cr
    \code{isWeekend} \tab Tests if a date falls on a weekend or not, \cr
    \code{isBizday} \tab Tests if a date is a business day or not, \cr
    \code{isHoliday} \tab Tests if a date is a non-business day or not. }
    
    Functions for getting additional information on 'timeDate' objects:
    
    \tabular{ll}{
    \code{getDayOfWeek} \tab Returns the day of the week to a 'timeDate' object, \cr
    \code{getDayOfYear} \tab Returns the day of the year to a 'timeDate' object. }
    
    Functions to extract subsets from 'timeDate' objects:
    
    \tabular{ll}{
    \code{[.timeDate} \tab Extracts or replaces subsets from 'timeDate' objects, \cr
    \code{window.timeDate} \tab Extracts a piece from a 'timeDate' object, \cr
    \code{cut.timeDate} \tab A synonyme for the window function, \cr
    \code{start.timeDate} \tab Extracts the first entry of a 'timeDate' object, \cr
    \code{end.timeDate} \tab Extracts the last entry of a 'timeDate' object, \cr
    \code{length.timeDate} \tab Gets the length of a 'timeDate' object, \cr
    \code{blockStart} \tab Creates start dates for equally sized blocks, \cr
    \code{blockEnd} \tab Creates end dates for equally sized blocks. }
       
}


\usage{
isWeekday(x)
isWeekend(x)
isBizday(x, holidays = holidayNYSE())
isHoliday(x, holidays = holidayNYSE())

getDayOfWeek(x)
getDayOfYear(x)

\method{length}{timeDate}(x)

\method{[}{timeDate}(x, \dots, drop = TRUE)
\method{window}{timeDate}(x, from , to, \dots)
\method{cut}{timeDate}(x, from , to, \dots)
\method{start}{timeDate}(x, \dots)
\method{end}{timeDate}(x, \dots)

blockStart(x, block = 20)
blockEnd(x, block = 20)
}


\arguments{
  
    \item{block}{
        an integer value specifying the length in number of records
        for numerically sized blocks of dates.
        }
    \item{drop}{
        ["["] - \cr
        a logical flag, by default \code{TRUE}.
        }
    \item{from, to}{
        starting date, required, and end date, ptional. If supplied 
        \code{to} must be after \code{from}.
        }
    \item{holidays}{
        [isBizday] - \cr
        holiday dates from a holiday calendar. An object of class
        \code{timeDate}.
        }
    \item{method}{
        [modify] - \cr
        a character string defining the modification method, one of
        \code{"sort"}, \code{"round"}, or \code{"trunc"}. 
        }     
    \item{x}{
        [isWeekday][isWeekend][isBizday][weekDay] - \cr
        an object of class \code{timeDate}.
        \cr
        [format][print] - \cr
        an object of class \code{timeDate}.
        }  
    \item{\dots}{
        arguments passed to other methods.
        }
       
}


\value{

    \code{isWeekday}\cr
    \code{isWeekend}
    \cr
    the functions return logical vectors indicating if a date is a 
    weekday, or a weekend day.
    \cr
    
    \code{isBizday}\cr
    \code{isHoliday}
    \cr
    the functions return logical vectors indicating if a date is a 
    business day, or a holiday. Note, that for business and holidays 
    extraction an holiday/business calendar has to be specified. 
    \cr
    
    \code{getDayOfWeek}\cr
    \code{getDayOfYear}
    \cr
    the function \code{getDayOfWeek} returns a three letter character 
    string with the names of the day of the week, and the function 
    \code{getDayOfYear} returns the day count as integer value starting
    January, 1st. 
    \cr
   
    \code{[.timeDate}\cr
    \code{cut.timeDate}\cr
    \code{start.timeDate}\cr
    \code{end.timeDate}\cr
    \code{blockStart}\cr
    \code{blockEnd}
    \cr
    these are functions for subsetting \code{"timeDate"} objects.
    The function \code{[.timeDate} extracts or replaces subsets,
    the function \code{cut} extracts a piece,
    the functions \code{start} and \code{end} extract the first and last 
    element,
    the functions \code{blockStart} and \code{blockEnd} create vectors of 
    start and end values for equally sized blocks. All functions return an
    object of class \code{"timeDate"}.

}


\seealso{

    \code{timeDateCoercion}, \code{timeDateCoercion}, 
    \code{timeDateMathOps}, \code{timeDateSpecDates}
    
    We also recommend to inspect the help pages for the POSIX time and date 
    class, \code{?Dates}, and the help pages from the contributed R packages
    \code{chron} and \code{date}.
}


\author{

    Diethelm Wuertz for the Rmetrics \R-port.
    
}


\examples{  
## Dates in April, currentYear: 
   tS = timeSequence(
      from = paste(currentYear, "-04-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS
    
## Subset of Weekends:
   isWeekend(tS)
   tS[isWeekend(tS)]
   
## Subset Wednesdays:
   getDayOfWeek(tS)
   tS[getDayOfWeek(tS) == "Wed"]
   
## Subset Business Days at NYSE:
   holidayNYSE()
   isBizday(tS, holidayNYSE())
   tS[isBizday(tS, holidayNYSE())]
     
## [ - Subsetting First Five Days:
   tS[1:5]   
   
## window - 
   GoodFriday(currentYear)
   EasterMonday(currentYear)
   window(tS, 
      start = GoodFriday(currentYear), 
      end = EasterMonday(currentYear))
   
## [ - 
   # All dates after Easter Monday:
   tS[tS > EasterMonday(currentYear)]
   
## Subset Start and End Date:
   c(start(tS), end(tS))
}


\keyword{chron}

