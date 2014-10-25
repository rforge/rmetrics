\name{isWeekday}


\alias{isWeekday}
\alias{isWeekend}


\title{Weekdays and Weekends}


\description{

    Tests if a date is a weekday or not.

}


\usage{
isWeekday(x, wday = 1:5)
isWeekend(x, wday = 1:5)
}


\arguments{

  \item{x}{
    an object of class \code{timeDate}.
  }
  \item{wday}{
    Specify which days should be considered as weekdays. By default
    from Mondays to Fridays.
  }
}


\value{

    the functions return logical vectors indicating if a date is a
    weekday, or a weekend day.

}


\examples{
## Dates in April, currentYear:
   currentYear = getRmetricsOptions("currentYear")
   tS = timeSequence(
      from = paste(currentYear, "-03-01", sep = ""),
      to = paste(currentYear, "-04-30", sep = ""))
   tS

## Subset of Weekends:
   isWeekend(tS)
   tS[isWeekend(tS)]
}


\keyword{chron}

