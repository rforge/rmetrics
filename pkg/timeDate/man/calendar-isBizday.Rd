\name{isBizday}

\alias{isBizday}
\alias{isHoliday}

\title{Business and Holidays}

\description{
    Tests if a date is a business day or not.
}

\usage{
isBizday(x, holidays = holidayNYSE(), wday = 1:5)
isHoliday(x, holidays = holidayNYSE(), wday = 1:5)
}

\arguments{
  \item{x}{an object of class \code{timeDate}.}
  \item{holidays}{
    holiday dates from a holiday calendar.  An object of class
    \code{timeDate}.}
  \item{wday}{
    Specify which days should be considered as weekdays. By default
    from Mondays to Fridays.
  }
}

\value{
  a logical vector of the same length as \code{x} indicating if a date
  is a business day, or a holiday, respectively.
}

\examples{## Dates in April, currentYear:
   currentYear = getRmetricsOptions("currentYear")
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
